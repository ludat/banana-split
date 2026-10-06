# Telemetría del frontend

El frontend manda métricas y traces por OTLP/HTTP a `https://otlp.ludat.io`.
La implementación está toda en [`src/js/telemetry.js`](src/js/telemetry.js) y se
arranca desde `src/interop.js`. El código Elm no sabe que esto existe.

## El contrato de privacidad

El objetivo explícito es no necesitar banner de consentimiento en ningún
momento. Eso impone dos cosas, y las dos están en el código:

**1. No se guarda ni se lee nada del dispositivo.** Ni cookies, ni
`localStorage`, ni `sessionStorage`, ni IndexedDB. Esto es lo que saca al
frontend del alcance del artículo 5(3) de la ePrivacy (el que exige
consentimiento para "almacenar o acceder a información en el equipo terminal"),
que es la regla que obliga al banner — y aplica independientemente de si el dato
es personal o no.

**2. No hay ningún identificador.** No hay session id, device id, visitor id ni
user id, ni siquiera uno en memoria que viva lo que vive la página. Sin un
identificador no se pueden hilvanar dos eventos en una misma persona, y los
datos quedan agregados en lugar de ser un perfil de comportamiento.

De eso se desprende el resto:

| Qué | Por qué |
|---|---|
| Nada de `user_agent.original` | Es el principal vector de fingerprinting y la instrumentación de `document-load` lo pone por defecto; se borra en el span processor. |
| Nada de idioma, resolución ni zona horaria | Mismo motivo. |
| Nada de texto libre por su cuenta | Ni mensajes de error, ni stack traces, ni bodies, ni headers. Un mensaje de error puede traer el nombre de un grupo o un monto que tipeó la persona. De los errores se registra solo el *tipo* (`TypeError`, etc.). La única excepción son los comentarios que la persona escribe y manda a propósito, más abajo. |
| URLs siempre plantilladas | Los ULIDs de grupo/pago/repartija se reemplazan por `:id`, y el query string y el fragment se descartan enteros. `/grupos/01J…/gastos?x=y` queda en `/grupos/:id/gastos`. |
| No se instrumentan las interacciones | `@opentelemetry/instrumentation-user-interaction` nombra los spans según el target DOM de cada click, lo que arrastra ids y textos de elementos a la telemetría. Es justo lo que convertiría esto en tracking de comportamiento, así que no está. |
| Se respeta GPC | En producción, si el browser manda `navigator.globalPrivacyControl`, no se inicializa nada. En desarrollo se ignora a propósito: la data va a tu propia máquina, y que tu propia preferencia te esconda tus propias trazas solo genera confusión. No se mira `navigator.doNotTrack`: está deprecado —fuera de la especificación, Safari lo removió hace años y Firefox le sacó la UI en la 135— así que ya no queda quién lo mande. |

El plantillado de URLs es deliberadamente paranoico: un segmento de path que no
se reconozca como palabra de ruta conocida (porque parece ULID o UUID, es
numérico, mide más de 20 caracteres, o tiene `@` o `%`) se enmascara. Preferimos
enmascarar de más que filtrar de menos. Los tests de
[`tests/telemetry.test.mjs`](tests/telemetry.test.mjs) fijan este
comportamiento.

### Lo que falta del lado del collector

Hay dos cosas que el browser no puede garantizar solo, y sin ellas el contrato
de arriba se rompe:

1. **Descartar la IP del cliente.** El collector la ve en cada request, y una IP
   es dato personal. En el Collector hay que dejar `include_metadata: false` en
   el receiver OTLP (es el default) y no agregar el procesador que la convierte
   en atributo; y en el ingress que esté adelante, no pasar `X-Forwarded-For`
   hacia atributos de telemetría. Si la IP termina guardada junto a los eventos,
   esto deja de ser anónimo y el banner vuelve a ser necesario.
2. **CORS.** El browser postea cross-origin a `otlp.ludat.io`, así que el
   receiver tiene que permitir el origin del frontend:

   ```yaml
   receivers:
     otlp:
       protocols:
         http:
           endpoint: 0.0.0.0:4318
           include_metadata: false
           cors:
             allowed_origins:
               - https://split.ludat.io
             allowed_headers:
               - content-type
   ```

   Sin esto no llega absolutamente nada y el browser no muestra error visible
   más allá de la consola.

## Qué se manda

### Métricas

Se exportan cada 30 s, con temporalidad **delta** (un contador en el browser
arranca de cero en cada carga, así que acumulativo no significaría nada), más un
flush cuando el documento se oculta, para que una visita corta no se pierda.

| Métrica | Tipo | Unidad | Atributos |
|---|---|---|---|
| `app.page.views` | counter | `1` | `url.template` |
| `http.client.request.duration` | histogram | `s` | `http.request.method`, `http.response.status_code`, `url.template`, `error.type` |
| `app.errors` | counter | `1` | `error.type`, `app.page.route` |
| `web_vitals.lcp` / `.inp` / `.fcp` / `.ttfb` | histogram | `ms` | `web_vitals.rating`, `url.template` |
| `web_vitals.cls` | histogram | `1` | `web_vitals.rating`, `url.template` |

Los web vitals salen de la librería [`web-vitals`](https://github.com/GoogleChrome/web-vitals).
El `url.template` que llevan es la ruta en el momento en que se reportó el
vital: para LCP, FCP y TTFB es la ruta de entrada, pero para INP y CLS puede ser
otra si la persona ya navegó.

Los page views se cuentan parcheando `history.pushState`/`replaceState` y
escuchando `popstate`. elm-land rutea del lado del cliente, así que después de
la primera carga no hay más document loads que contar; parchear `history` evita
que el lado Elm tenga que enterarse de nada. Un `replaceState` que no cambia la
ruta no cuenta como view.

### Traces

- `document-load`: la carga inicial con los tiempos de navigation timing.
- `xml-http-request`: el `Http` de Elm usa XMLHttpRequest, así que esto cubre
  todo el cliente de API generado por servant-elm.
- `fetch`: lo que no pase por Elm.

Los spans llevan `url.full` (origin + path plantillado), `url.template`,
`http.request.method`, `http.response.status_code` y `app.page.route`.

Como `/api` es same-origin en producción, el SDK propaga el header `traceparent`
en los requests a la API, y el backend lo continúa: un click en el browser y las
queries que desencadena quedan en un solo trace. Ver
[`../TELEMETRY.md`](../TELEMETRY.md).

No se usa `@opentelemetry/context-zone`: la app no anida trabajo async adentro
de nuestros spans, así que el `StackContextManager` por defecto alcanza y zone.js
se queda afuera del bundle.

## Costo en bundle

El SDK agrega unos 173 KB sin comprimir / ~55 KB gzip al bundle principal (de
esos, unos 6 KB gzip son el SDK de logs, que carga los eventos de la app y los
comentarios). Si en algún momento molesta, lo primero que conviene mirar es cargar
`telemetry.js` con `import()` dinámico después del primer render, en lugar de
recortar instrumentaciones.

## Desarrollo

El proceso `observability` de `process-compose.yaml` levanta el servicio
homónimo de `docker-compose.yaml`: la imagen `grafana/otel-lgtm`, que trae
Collector, Prometheus, Tempo, Loki y Grafana ya conectados entre sí. **Grafana
queda en http://localhost:3000** (usuario y contraseña `admin`), y ahí se ven
los traces en Tempo y las métricas en Prometheus.

El dev server reporta ahí solo. El endpoint sale de la variable de entorno
`OTLP_ENDPOINT`, que está declarada en `elm-land.json` — por eso elm-land la
expone al browser como `import.meta.env.ELM_LAND_OTLP_ENDPOINT` — y que
`process-compose.yaml` fija en `http://localhost:4318` para el proceso
`frontend`. Levantando todo con process-compose esto funciona sin hacer nada.

Un `pnpm start` suelto, sin esa variable, no instrumenta nada: así no llena la
consola de exports fallidos cuando no hay stack local.

### Es build-time, no runtime

Vite sustituye el valor en el bundle al compilar, así que **el endpoint queda
fijo en el artefacto** y cambiarlo es un rebuild. En producción lo pone la
derivación `elm-ui` de `flake.nix`, que es donde se compila el frontend.

Eso también quiere decir que no se puede apuntar una misma imagen a dos
collectors distintos. Si algún día hace falta, las opciones son un endpoint de
config JSON que el backend sirva y el frontend lea antes de arrancar (cuesta un
round trip antes del primer render, porque `initTelemetry` tiene que esperarlo o
las instrumentaciones se registran después de los primeros requests de Elm), o
proxear OTLP same-origin desde el ingress y usar una URL relativa — ojo que eso
último manda la cookie de sesión al collector, que es justamente el tipo de
identificador que el contrato de arriba excluye, así que habría que stripearla
en el ingress.

**No hay default hardcodeado**, y es a propósito: un build al que no le llegó la
variable no reporta, en lugar de reportar a un destino que nadie eligió y en
silencio. Por eso tampoco existe más un `?telemetry=1`: sin endpoint no hay nada
que forzar.

## ¿Está activa?

Lo primero es la consola, **en cualquier ambiente, producción incluida**: al
arrancar el módulo imprime una línea y una sola,

```
[telemetry] on, exporting to https://otlp.ludat.io
[telemetry] off: OTLP_ENDPOINT was not set when the bundle was built
```

Y lo mismo queda en `window.__telemetry`, que es a lo que conviene ir cuando la
página lleva un rato abierta o alguien limpió la consola:

```js
window.__telemetry
// { on: true, endpoint: "https://otlp.ludat.io", version: "C10GL13Y" }
// { on: false, reason: "globalPrivacyControl is on" }
```

Que esto se reporte en producción es deliberado: antes todo el logging estaba
detrás de `import.meta.env.PROD` y el resultado era que desde el browser no había
ninguna forma de saber si la telemetría estaba corriendo. Una línea no es ruido,
y no sale nada del dispositivo.

### Si está activa pero no llegan datos

El logger interno del SDK (nivel WARN) es lo que delata un export fallido, y está
prendido en **todos** los ambientes. Solo habla cuando algo falla, y el único
costo es ruido en una consola que los usuarios no abren — mucho más barato que no
poder distinguir "no se manda" de "se manda y se pierde".

Ojo que eso cubre el salto browser → collector nada más; si el collector acepta
la data y la pierde más adelante, el browser ve un 200 y no se entera. Para ese
caso los contadores del collector son el lugar donde mirar:

```bash
docker exec banana-split-observability-1 \
  curl -s http://127.0.0.1:8888/metrics | grep -E 'otelcol_(receiver|exporter)_.*(spans|metric_points)'
```

### Delta vs acumulativo

El frontend manda las métricas en temporalidad **delta** a propósito, y el
receiver OTLP de Prometheus las rechaza por defecto con `invalid temporality
and type combination`. Por eso el servicio pasa
`PROMETHEUS_EXTRA_ARGS=--enable-feature=otlp-deltatocumulative`. Si algún día
las métricas del browser dejan de aparecer en Grafana pero los traces siguen
estando, revisar esto primero.

### Logs

El frontend manda dos cosas como logs: los eventos de la app y los comentarios
que la gente escribe (las dos, más abajo). Nada más, y es deliberado — no hay
una facilidad de logging de propósito general, porque una disponible para todo
el código es la forma en que el texto libre empieza a filtrarse. El resto del
pipeline está disponible para cuando se instrumente el backend.

Los tests corren con `pnpm test:js` (y van incluidos en `pnpm test`, que es lo
que corre CI).

## Pendiente

`service.version` no se manda, así que no se puede distinguir entre builds. El
frontend se compila dentro de la derivación `elm-ui` de `flake.nix`, que no
recibe la revisión de git; para arreglarlo habría que pasarle `self.rev` como
variable de entorno y leerla desde `import.meta.env`.

## Eventos de la app

Además de la instrumentación automática, la app reporta un puñado de eventos
propios. El criterio para que uno entre es que **no se pueda deducir de los logs
del backend**: si el backend puede verlo, que lo logee el backend, que tiene más
contexto y ninguna restricción de privacidad.

Por eso no hay eventos de "se creó un gasto", "se escaneó un ticket" ni "se
verificó el login": todos esos son requests. Los que sí están:

- **`app.share`** — qué mecanismo de compartir usó realmente el browser
  (`native`, `clipboard`, `dismissed`, `unsupported`, `failed`). Invisible desde
  el servidor, y la única forma de saber si la feature funciona en los browsers
  que tus usuarios usan de verdad.
- **`app.api.decode_error`** — el backend contestó 200 y el frontend no pudo
  decodificar la respuesta. Nadie lo ve hoy: el servidor lo logeó como éxito.
  La causa habitual es un cliente con un bundle viejo contra una API más nueva,
  y por eso se cruza con `service.version`.
- **`app.gasto.edit_abandoned`** — alguien cargó parte del formulario de un
  gasto y lo cerró sin guardar. El backend solo ve los gastos que se guardaron,
  nunca los que se perdieron. Lleva `mode` (`nuevo` / `existente`) y `section`
  (`basico` / `pagadores` / `deudores`), que es el paso en el que se rindieron y
  lo más accionable del evento.

  Se emite desde `reportarEdicionAbandonada`, en `Components/PagoDetalleModal`,
  colgado del mismo lugar que ya detectaba el cierre de la edición para apagar
  el aviso del browser. Guardar y borrar también cierran la edición y **no** son
  abandonos, así que se excluyen por mensaje; todo el resto —el botón de cerrar,
  el botón atrás, descartar— sí lo es. El `section` del evento tiene que seguir
  en paridad con `Models.PagoForm.Section`: un paso nuevo allá sin su valor acá
  hace que el evento se descarte, y hay un test que lo fija.

Van como **log records**, no como métricas, y se consultan en Grafana con
`{event_name="app.share"}` o `{event_name="app.api.decode_error"}`. El criterio
frente a una métrica es el volumen y qué querés preguntar: un page view o un web
vital es una tasa, así que es métrica; un share o un error de decodificación es
una ocurrencia discreta que querés mirar de a una, y al volumen de esta app Loki
igual te los cuenta con `count_over_time`, mientras que de un counter no podés
volver atrás a las ocurrencias.

El body del record es el nombre del evento, nunca prosa: el detalle vive en los
atributos, que pasaron por el vocabulario. Eso es lo que los mantiene seguros.

Las fallas de red, los timeouts y los 4xx/5xx **no** son eventos propios: la
instrumentación de XHR ya los marca con `error.type` en el span, y eso se
propaga a `http.client.request.duration`.

### El vocabulario cerrado

`src/js/telemetry.js` declara, en `APP_EVENTS`, los eventos válidos y por cada
uno las claves de atributo permitidas y sus valores admitidos. Una clave que no
esté declarada se descarta; un valor fuera de su conjunto tira el evento
completo. Esto es lo que mantiene el contrato cierto con el tiempo en lugar de
depender de que cada call site se acuerde: no hay camino por el que un nombre de
grupo, un monto o un id lleguen al exporter, no importa cómo se escriba el
llamado.

Los helpers de más alto nivel viven en `src/Utils/Telemetry.elm`. El de errores
de decodificación es el ejemplo de por qué conviene que estén ahí: `Http.BadBody`
trae el mensaje del decoder, que **incluye un fragmento del payload**, y la
función lo descarta por construcción — hace pattern match y nunca liga ese
String a un nombre.

### `service.version`

Es el hash de contenido del bundle, sacado de `import.meta.url` (que en
producción es `/assets/index-<hash>.js`). No hace falta pasarle la revisión de
git al build: Vite ya versiona el archivo, y el hash cambia exactamente cuando
cambia el JS. En el dev server vale `dev`.

## Comentarios de los usuarios

El ítem "Enviar comentarios" del menú abre un modal que manda lo que la persona
escribió como log record, con `event.name=app.feedback`. No pasa por la API: va
derecho a la telemetría.

En Grafana se consultan con `{event_name="app.feedback"}`; el stream trae
`service_version` y `app_page_route` como labels, así que sabés desde qué build
y desde qué pantalla escribieron.

Es la única excepción a "no sale texto libre del browser", y no reabre la
pregunta del banner porque la persona lo escribió y apretó un botón para
mandarlo, que es consentimiento en el sentido corriente de la palabra. El modal
lo dice al lado del botón. Tres consecuencias que conviene tener presentes:

1. **Es un canal de ida.** No se adjunta ningún identificador, así que no hay a
   quién responderle. El modal avisa que si quieren respuesta tienen que dejar
   cómo contactarlos.
2. **Va a contener datos de terceros.** Los nombres de la gente del grupo, por
   ejemplo. Ese stream de Loki quiere retención más corta que el resto; es una
   configuración del lado del collector, no algo que el browser pueda imponer.
3. **Es un vector de abuso.** `otlp.ludat.io` es público y con CORS abierto, así
   que cualquiera puede postear log records sin pasar por la app. Del lado del
   cliente el mensaje se corta en 2000 caracteres, pero el rate limit tiene que
   estar en el ingress.

Ninguna otra parte de la app puede emitir log records, y conviene que siga así:
una facilidad de logging disponible para todo el código es la forma en que el
texto libre empieza a filtrarse.

## El backend

Está instrumentado aparte, con sus propias reglas (que son bastante menos
estrictas, porque no corre en el equipo de nadie): ver
[`../TELEMETRY.md`](../TELEMETRY.md).
