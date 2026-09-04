# Plan: cache del resumen por gasto

## Qué queremos

Tres cosas que hoy no se pueden hacer, y que se resuelven con lo mismo:

1. **Resumen personalizado por gasto en la lista** — cuánto puso y cuánto consumió cada
   participante en ese gasto, no sólo el monto total.
2. **La razón de por qué un gasto es inválido** — hoy el front sólo sabe `isValid : Bool`.
   El `[ErrorResumen]` que explica el porqué se calcula y se tira.
3. **Sumar todos los gastos sin traer cada gasto entero de la base.**

## Estado de partida

### El valor derivado ya existe, sólo que en memoria

`getResumenPago :: Pago -> ResumenNetos` (`BananaSplit.Core`) ya calcula exactamente lo que
piden (1) y (2): los netos por participante y la lista de errores. `ResumenPago`
(`Site.Api`) ya empaqueta las tres piezas que la UI quiere, y ya se sirve por
`POST /pagos/resumen` como preview de un gasto sin guardar. **Lo calculamos y lo
descartamos al guardar.**

### Ya existe un cache write-through de un valor derivado

`pagos.is_valid` es un cache booleano de `isValid pago`, y **todos los caminos de escritura
que pueden invalidarlo ya están centralizados**:

- `savePago` recalcula con `addIsValidPago` y escribe la columna. `updatePago` es
  `savePago`.
- `saveRepartijaClaim` / `deleteRepartijaClaim` cambian el reparto sin tocar la fila del
  pago, y ya llaman a `recalcValidezPago` vía `fetchPagoIdFromRepartija` /
  `fetchPagoIdFromClaim`.
- Ya hay comando de backfill: `run-migration recompute-pagos` → `recomputePagos`, por lotes
  de 100 ordenados por ULID.

**Esto es lo importante: ensanchar ese cache de `Bool` a un resumen completo no necesita
maquinaria de invalidación nueva.** Los ganchos ya están y ya se usan.

Borrar un participante no es un agujero: `distribuciones_partes_items.participante__id`
tiene FK sin `ON DELETE CASCADE`, así que borrar un participante que aparece en un gasto
falla en la base en vez de cambiar un reparto en silencio.

### El N+1 es real y está en dos lugares

`handleGetNetos` (`Site/Handler/Grupos.hs:80`) y `handleFreezeGrupo` (`:144`) hacen los dos:

```haskell
shallowPagos <- fetchShallowPagos grupoId
forM shallowPagos $ \shallowPago -> fetchPago shallowPago.pagoId
```

`fetchPago` son 1 query por la fila más `fetchDistribucion` dos veces, y cada una hasta 3
subqueries por los items. O sea ~`1 + N×7` queries para recalcular netos que podríamos
tener guardados.

El camino de grupo congelado no toca nada de esto: sirve las transferencias guardadas y no
calcula netos. El cache sólo ayuda al camino abierto y al freeze.

### Ya está hecho el prerrequisito

Los montos se guardan como enteros de unidades mínimas (`UnidadesMinimas = Int64`) y la
escala sale de `escalaDe` aplicado a la moneda de la fila. Por eso las columnas de este
cache pueden ser `bigint` sumables directamente con `SUM()`, sin inventar una segunda
representación de `Monto`. Ese era el argumento más fuerte en contra de esta opción.

## Decisiones tomadas

- **Tabla de netos por gasto y participante**, no un blob JSON por gasto. La tabla es la
  única opción que cumple (3) de verdad: `SUM() GROUP BY` en Postgres en vez de traer todos
  los blobs y plegarlos en Haskell. Además la validez pasa a ser emergente — un gasto
  inválido no tiene filas y la agregación lo saltea sin leerlo.
- **Los errores van igual en una columna `jsonb` en `pagos`**, porque no entran en la
  tabla. `NULL` = todavía no calculado, `[]` = válido, no vacío = inválido con motivos.
  La ausencia de filas **no** es la autoridad sobre la validez: la columna lo es. Eso evita
  que un gasto recién migrado (cache frío) desaparezca de los netos en silencio.
- **Cache frío = inválido.** Con `errores IS NULL` el gasto cae en `cantidadPagosInvalidos`,
  que la UI ya muestra. El modo de falla es visible, no silencioso, y se cura corriendo el
  backfill.
- **La tabla se llama `pago_netos`**, consistente con el schema de hoy (`pagos`,
  `repartija_items`). El rename a Gastos por ahora es sólo de `ui/`; cuando el backend se
  renombre, esta tabla va en el mismo movimiento.
- **PK compuesta `(pago__id, participante__id)`**, sin `id` sustituto. Es una tabla
  derivada, no tiene identidad propia, y así el reemplazo por gasto es idempotente. Se
  aparta de la convención del resto del schema a propósito.
- **`moneda` se duplica en la fila.** Sin ella una fila no se puede interpretar (las
  unidades mínimas dependen de la moneda) y la agregación necesitaría un join a `pagos`.
  No puede desincronizarse porque se reescribe en el mismo `savePago`.

## Pasos

### Fase 1 — Dominio

- [ ] **1.1** En `BananaSplit.Core`, agregar el tipo que devuelve el resumen por gasto:
      ```haskell
      data NetoDeParticipante = NetoDeParticipante { pagado :: Monto, consumido :: Monto }
      data ResumenGasto = ResumenGasto
        { netos :: Netos NetoDeParticipante
        , errores :: [ErrorResumen]
        }
      ```
      `ErrorResumen` ya vive en `BananaSplit.Deudas`, así que la capa de persistencia lo
      puede importar sin ciclo (a diferencia de `ResumenPago`, que vive en `Site.Api`).
- [ ] **1.2** `getResumenGasto :: Pago -> ResumenGasto`, armado con los `getResumen` de
      `pagadores` y `deudores` que `getResumenPago` ya usa. Una sola fuente de verdad para
      handler y persistencia.
- [ ] **1.3** Helper de agregación: de `[(Moneda, ResumenGasto)]` a
      `PorMoneda (Netos Monto)`, salteando los que tienen errores. Es lo que reemplaza al
      `fetchPago` en loop cuando no querramos ir a SQL.
- [ ] **1.4** Test unitario: para un `Pago` cualquiera,
      `pagado <> fmap negate consumido == calcularNetosPago pago`, y que un pago inválido
      tenga `errores` no vacío. Esto ancla el invariante antes de guardarlo en ningún lado.

**Verificación:** `cabal test`.

### Fase 2 — Migración

- [ ] **2.1** Tabla `pago_netos`:
      - `pago__id` text not null, FK → `pagos(id)` **`ON DELETE CASCADE`** (que borrar un
        gasto se lleve su cache solo, sin tocar `deletePago`).
      - `participante__id` text not null, FK → `participantes(id)`, sin cascade.
      - `moneda` text not null.
      - `pagado_en_unidades_minimas` bigint not null.
      - `consumido_en_unidades_minimas` bigint not null.
      - PK compuesta `(pago__id, participante__id)`.
      - Índice por `(participante__id)` para la consulta "cuánto debe X".
- [ ] **2.2** Columna `pagos.errores` jsonb **nullable** (NULL = sin calcular).
- [ ] **2.3** Dejar `pagos.is_valid` como está por ahora. Se dropea en la fase 7, cuando ya
      nada la lea.

**Verificación:** `cabal run banana-split -- migrations migrate` en dev.

### Fase 3 — Schema y persistencia

- [ ] **3.1** `Persistence/Schema.hs`: tabla `PagoNetoT` y `pago_netos` en `BananaSplitDb`;
      campo `pagoErrores :: Columnar f (Maybe (PgJSONB [M.ErrorResumen]))` en `PagoT`.
- [ ] **3.2** `savePago`: después de guardar las distribuciones, calcular
      `getResumenGasto`, borrar las filas del gasto e insertar las nuevas, y escribir
      `errores`. Un gasto con errores escribe `errores` y **cero filas**.
- [ ] **3.3** Renombrar `recalcValidezPago` → `recalcularResumenGasto` y que haga lo mismo
      que 3.2 sin volver a guardar el pago. Los dos llamadores
      (`saveRepartijaClaim` / `deleteRepartijaClaim`) quedan igual.
- [ ] **3.4** `fetchShallowPagos`: leer `errores` y las filas de `pago_netos` del grupo en
      **una** query aparte (no una por gasto), y armar el `ResumenGasto` de cada uno.
- [ ] **3.5** `netosDeGrupo :: ULID -> Pg (PorMoneda (Netos Monto))`: la agregación en SQL.
      ```sql
      SELECT participante__id, moneda,
             SUM(pagado_en_unidades_minimas - consumido_en_unidades_minimas)
      FROM pago_netos JOIN pagos ON ...
      WHERE pagos.grupo__id = ? AND pagos.errores = '[]'::jsonb
      GROUP BY 1, 2
      ```
      Reconstruir cada `Monto` con `desdeUnidadesMinimas` sobre la `moneda` del grupo.

**Verificación:** que el property test `"Pago roundtrips from the db"` siga pasando, más uno
nuevo: guardar un grupo con varios gastos y comparar `netosDeGrupo` contra
`calcularNetosTotales` sobre el mismo grupo cargado a mano. Ese test es el que justifica
todo el cache y tiene que quedar en el repo.

### Fase 4 — Backfill

- [ ] **4.1** `recomputePagos` ya recorre todos los gastos y los re-guarda, así que después
      de la fase 3 ya llena el cache sin tocarlo. Confirmar que sea así y, si no, ajustarlo.
- [ ] **4.2** Correr `run-migration recompute-pagos` en dev y verificar que no queden
      `pagos` con `errores IS NULL`.

### Fase 5 — Handlers (acá muere el N+1)

- [ ] **5.1** `handleGetNetos` (camino abierto): reemplazar el loop de `fetchPago` por
      `netosDeGrupo`. Ojo: los netos totales siguen siendo
      `netosDeGrupo <> netosDeTransferencias hechas` — el cache cubre gastos, las
      transferencias van aparte y ya son baratas.
- [ ] **5.2** `cantidadPagos` / `cantidadPagosInvalidos` salen de `fetchShallowPagos`
      (inválido = `errores` no vacío **o** `NULL`).
- [ ] **5.3** `handleFreezeGrupo` (`Grupos.hs:144`): misma sustitución antes de
      `minimizeTransactions`.
- [ ] **5.4** Sacar `fetchPago` de los imports de `Grupos.hs` si ya no se usa.

**Verificación:** con el server corriendo, `GET /api/grupo/{id}/resumen` da los mismos netos
que antes y en los logs de query se ve **una** consulta de agregación, no N. Comparar contra
un grupo real de dev antes y después.

### Fase 6 — API y front

- [ ] **6.1** `ShallowPago`: cambiar `isValid :: Bool` por `resumen :: Maybe ResumenGasto`
      (`Nothing` = cache frío). Agregar los `DefineElm` de `ResumenGasto`,
      `NetoDeParticipante` y `Netos NetoDeParticipante` en `BananaSplit.Elm`.
- [ ] **6.2** Regenerar Elm y arreglar los usos de `pago.isValid`
      (`Pages/Grupos/Id_.elm`, `Pages/Grupos/GrupoId_/Gastos.elm`).
- [ ] **6.3** Mostrar en la lista de gastos cuánto puso y cuánto consumió cada uno.
- [ ] **6.4** Mostrar el motivo de invalidez desde `errores` en vez del cartel genérico.
      `TipoErrorResumen` ya está en Elm, falta el texto de cada caso.

**Verificación:** `pnpm build && pnpm format && pnpm review`, y mirarlo en el navegador con
un gasto válido, uno inválido y uno de repartija.

### Fase 7 — Limpieza

- [ ] **7.1** Borrar `updateIsValidPago` y el campo `isValid` de `Pago` si ya nadie lo usa
      (ojo: `addIsValidPago` lo escribe hoy en el modelo).
- [ ] **7.2** Migración que dropea `pagos.is_valid`.
- [ ] **7.3** Correr `recompute-pagos` en prod después de deployar la fase 3.

## Lo que este plan deja afuera a propósito

- **La home con saldos por grupo.** Es el caso que más justifica que esto sea una tabla y no
  un blob, pero es una feature aparte. Cuando se haga, la query es la misma de 3.5 sin
  filtrar por grupo y con un join a `participantes.user__id`.
- **`convertirMonto`** (`TasaDeCambio.hs`) todavía tiene el `2` hardcodeado y ahora puede
  usar `escalaDe tabla.base`. No tiene que ver con el cache.
- **Recalcular el cache cuando cambia la moneda del grupo.** No hace falta: el resumen por
  gasto es en la moneda del gasto, y la conversión a la moneda del grupo pasa después, en
  `consolidarNetos`.
