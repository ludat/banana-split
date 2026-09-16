-- | Tests que necesitan dos conexiones commiteando de verdad, así que no pueden
-- vivir bajo @test/BananaSplit/Persistence/@: el 'SpecHook' de esa carpeta
-- envuelve cada test en una transacción que se rollbackea, y una transacción no
-- ve lo que otra no commiteó.
--
-- Todo lo que necesitan vive en este archivo a propósito: es andamiaje de un
-- test, no de la app.
module Integracion.ConcurrenciaSpec (
  spec,
) where

import Data.Pool qualified as Pool
import Data.String (fromString)
import Data.Text qualified as Text
import Data.Time (fromGregorian)
import Database.Beam
import Database.Beam.Postgres (Connection)
import Database.PostgreSQL.Simple qualified as Simple
import Test.Hspec

import BananaSplit qualified as M
import BananaSplit.Persistence
import BananaSplit.Persistence.Schema
import BananaSplit.PgRoll qualified as PgRoll
import BananaSplit.ULID (ULID, nullUlid)
import Preludat
import Site.Config qualified as Config

-- | Cuántas veces se repite la carrera. La ventana es ancha (el recálculo son
-- ~10 queries), así que sin aislamiento falla en casi todas: con esto alcanza y
-- sobra para detectarlo.
vueltas :: Int
vueltas = 25

spec :: Spec
spec =
  around conConexiones $ do
    -- El oráculo no sabe nada de estas carreras en particular: compara el cache
    -- contra recalcular desde las distribuciones, así que también atraparía
    -- otras formas de dejarlo inconsistente.
    describe "dos personas reclamando en la misma repartija" $
      it "dejan el cache consistente con lo que dicen las distribuciones" $
        \(connA, connB, escenario) ->
          replicateM_ vueltas $ do
            (desdeCache, recalculado) <- correrVueltaDeClaims connA connB escenario
            desdeCache `shouldBe` recalculado

    describe "alguien editando el gasto mientras otro reclama" $
      it "dejan el cache consistente con lo que dicen las distribuciones" $
        \(connA, connB, escenario) ->
          replicateM_ vueltas $ do
            (desdeCache, recalculado) <- correrVueltaDeGuardarYClaim connA connB escenario
            desdeCache `shouldBe` recalculado

conConexiones :: ((Connection, Connection, Escenario) -> IO ()) -> IO ()
conConexiones correr = do
  config <- Config.createConfig "test"
  -- Migrar es idempotente y barato, así que este spec no depende de que otro
  -- haya corrido antes. La base tiene que existir de antes, igual que para el
  -- resto del suite.
  PgRoll.init config
  PgRoll.startAndComplete config
  bracket (makePool config) Pool.destroyAllResources $ \pool ->
    -- Anidar dos 'withResource' da dos conexiones distintas —el pool nunca
    -- entrega el mismo recurso a dos tomadores a la vez—, que es justo lo que
    -- la carrera necesita. Es también de donde las saca la app.
    Pool.withResource pool $ \connA ->
      Pool.withResource pool $ \connB -> do
        -- Lo que escribe este test queda commiteado, y hay tests que cuentan
        -- filas sin filtrar por grupo: si se cayera sin limpiar, el orden entre
        -- specs decidiría si pasan.
        limpiarTodo connA
        escenario <- prepararEscenario connA
        correr (connA, connB, escenario) `finally` limpiarTodo connA

-- | Vacía la base para que el test de integración arranque siempre del mismo
-- estado. No puede apoyarse en transacciones que se rollbackeen como el resto
-- del suite: la carrera necesita dos conexiones que commiteen de verdad, así
-- que lo que escriben queda.
--
-- Las tablas salen del catálogo y no de una lista a mano, así que una tabla
-- nueva se limpia sola. Y @TRUNCATE ... CASCADE@ resuelve el orden entre claves
-- foráneas, que es lo que obligaría a ordenar la lista de hijas a padres.
--
-- Mira sólo @public@ porque ahí viven las tablas reales: el @search_path@
-- apunta al esquema versionado de pgroll, donde son vistas —y por eso
-- @pg_tables@ no las trae—, y el bookkeeping de pgroll está en su propio
-- esquema.
limpiarTodo :: Connection -> IO ()
limpiarTodo conn = do
  tablas <- Simple.query_ conn "SELECT tablename FROM pg_tables WHERE schemaname = 'public'"
  case fmap Simple.fromOnly tablas of
    [] -> pure ()
    nombres ->
      void
        $ Simple.execute_ conn
        $ fromString
        $ toS
        $ "TRUNCATE "
        <> Text.intercalate ", " (fmap entrecomillar nombres)
        <> " RESTART IDENTITY CASCADE"
  where
    entrecomillar nombre = "public.\"" <> Text.replace "\"" "\"\"" nombre <> "\""

-- | El escenario que reproduce la carrera: un gasto cuya repartija tiene dos
-- items, y dos participantes que reclaman uno cada uno.
data Escenario = Escenario
  { grupoId :: ULID
  , pagoId :: ULID
  , repartijaId :: ULID
  , itemA :: ULID
  , itemB :: ULID
  , participanteA :: M.ParticipanteId
  , participanteB :: M.ParticipanteId
  }

-- | Una vuelta de "dos personas reclaman a la vez en la misma repartija", cada
-- una en su propia conexión y transacción.
--
-- Devuelve el par (lo que dice el cache, lo que da recalcular desde las
-- distribuciones). Que sean iguales es un oráculo de consistencia que no sabe
-- nada de esta carrera en particular, así que también atraparía otras.
--
-- Con el lock de 'recalcularResumenGasto' siempre coinciden. Para ver la
-- versión rota, sacá el 'lockearPago' de esa función: deja de coincidir en casi
-- todas las vueltas.
correrVueltaDeClaims ::
  Connection
  -> Connection
  -> Escenario
  -> IO (M.PorMoneda (M.Netos M.Monto), M.PorMoneda (M.Netos M.Monto))
correrVueltaDeClaims connA connB escenario = do
  limpiarClaims connA escenario

  -- Las dos transacciones arrancan lo más juntas posible y cada una reclama su
  -- item. Sin lock, cada una lee los claims sin ver el de la otra.
  listoA <- newEmptyMVar
  listoB <- newEmptyMVar
  _ <- forkIO $ reclamar connA escenario.repartijaId escenario.itemA escenario.participanteA >> putMVar listoA ()
  _ <- forkIO $ reclamar connB escenario.repartijaId escenario.itemB escenario.participanteB >> putMVar listoB ()
  takeMVar listoA
  takeMVar listoB

  desdeCache <- conTransaccionDeLecturaRapida connA $ netosDeGrupo escenario.grupoId
  recalculado <- conTransaccionDeLecturaRapida connA $ do
    pago <- fetchPago escenario.pagoId
    pure $ M.calcularNetosPago pago `M.enMoneda` pago.moneda
  pure (desdeCache, recalculado)

-- | La otra carrera: alguien edita el gasto mientras otro reclama en su
-- repartija.
--
-- 'savePago' arma el resumen sin releer el gasto, así que tiene que ir a buscar
-- los claims por su cuenta: son el único insumo que no escribe él mismo y que
-- no viaja en lo que manda el front. Esto verifica justamente eso.
--
-- El item A viene reclamado de antes para que el final sea un gasto válido: si
-- quedaran items sin reclamar el gasto sería inválido igual y la carrera no se
-- notaría.
--
-- Para ver la versión rota, hacé que 'savePago' use las distribuciones tal como
-- se las devolvió 'saveDistribucion', sin pasarlas por 'conClaimsGuardados':
-- el cache queda diciendo que el gasto no cierra.
correrVueltaDeGuardarYClaim ::
  Connection
  -> Connection
  -> Escenario
  -> IO (M.PorMoneda (M.Netos M.Monto), M.PorMoneda (M.Netos M.Monto))
correrVueltaDeGuardarYClaim connA connB escenario = do
  limpiarClaims connA escenario
  reclamar connA escenario.repartijaId escenario.itemA escenario.participanteA

  -- El gasto como lo tiene el front antes de mandar la edición. Los claims no
  -- viajan con él, así que da igual cuáles trae: 'savePago' los relee.
  pago <- conTransaccionDeLecturaRapida connA $ fetchPago escenario.pagoId

  listoA <- newEmptyMVar
  listoB <- newEmptyMVar
  _ <- forkIO $ reguardar connA escenario.grupoId pago >> putMVar listoA ()
  _ <- forkIO $ reclamar connB escenario.repartijaId escenario.itemB escenario.participanteB >> putMVar listoB ()
  takeMVar listoA
  takeMVar listoB

  desdeCache <- conTransaccionDeLecturaRapida connA $ netosDeGrupo escenario.grupoId
  recalculado <- conTransaccionDeLecturaRapida connA $ do
    guardado <- fetchPago escenario.pagoId
    pure $ M.calcularNetosPago guardado `M.enMoneda` guardado.moneda
  pure (desdeCache, recalculado)

-- | Una transacción como la del handler de editar: vuelve a guardar el gasto
-- con un cambio que no toca el reparto.
reguardar :: Connection -> ULID -> M.Pago -> IO ()
reguardar conn unGrupoId pago =
  conTransaccionDeEscritura conn
    $ void
    $ savePago unGrupoId pago{M.nombre = "Cena editada"}
-- | Una transacción como la del handler: reclamar un item.
reclamar :: Connection -> ULID -> ULID -> M.ParticipanteId -> IO ()
reclamar conn unaRepartijaId unItemId participante =
  conTransaccionDeEscritura conn
    $ void
    $ saveRepartijaClaim unaRepartijaId (M.RepartijaClaim nullUlid participante unItemId Nothing)

-- | Vuelve al estado sin claims, con el cache al día.
limpiarClaims :: Connection -> Escenario -> IO ()
limpiarClaims conn escenario =
  conTransaccionDeEscritura conn $ do
    runDelete
      $ delete
        db.repartija_claims
        ( \claim ->
            claim.repartijaclaimRepartijaItem
              `in_` [val_ (RepartijaItemId escenario.itemA), val_ (RepartijaItemId escenario.itemB)]
        )
    recalcularResumenGasto escenario.pagoId

prepararEscenario :: Connection -> IO Escenario
prepararEscenario conn = conTransaccionDeEscritura conn $ do
  grupo <- createGrupo "Concurrencia" "uno"
  otro <-
    addParticipante grupo.id "otro" >>= \case
      Right participante -> pure $ M.ParticipanteId participante.id
      Left e -> panic e
  let uno = case grupo.participantes of
        (p : _) -> M.ParticipanteId p.id
        [] -> panic "el grupo deberia tener un participante"

  pago <-
    savePago grupo.id
      $ M.Pago
        { M.pagoId = nullUlid
        , M.monto = 100
        , M.moneda = M.ARS
        , M.nombre = "Cena"
        , M.fecha = fromGregorian 2026 1 1
        , M.pagadores =
            M.Distribucion nullUlid $ M.TipoDistribucionPartes $ M.DistribucionPartes nullUlid [M.MontoFijo 100 uno]
        , M.deudores =
            M.Distribucion nullUlid
              $ M.TipoDistribucionRepartija
              $ M.Repartija
                { M.id = nullUlid
                , M.nombre = "Cena"
                , M.extra = 0
                , M.distribucionDeSobras = M.SobrasNoDistribuir
                , M.items =
                    [ M.RepartijaItem nullUlid "Item A" 50 1
                    , M.RepartijaItem nullUlid "Item B" 50 1
                    ]
                , M.claims = []
                }
        }

  case pago.deudores.tipo of
    M.TipoDistribucionRepartija repartija ->
      case repartija.items of
        (a : b : _) ->
          pure
            Escenario
              { grupoId = grupo.id
              , pagoId = pago.pagoId
              , repartijaId = repartija.id
              , itemA = a.id
              , itemB = b.id
              , participanteA = uno
              , participanteB = otro
              }
        _ -> panic "esperaba dos items"
    _ -> panic "esperaba una repartija"
