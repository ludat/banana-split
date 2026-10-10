-- | Que una 'Pg' pueda instrumentarse: spans y logs desde adentro de una
-- acción de Beam.
--
-- Es un test de integración y no un unit test porque la instancia interpreta la
-- acción de adentro con un runner anidado sobre la conexión que presta
-- @liftIOWithHandle@: lo único que prueba algo es correrla contra una base de
-- verdad.
module Integracion.PgSpec (
  spec,
) where

import Data.IORef (readIORef)
import Data.Pool qualified as Pool
import Database.Beam.Postgres (Connection)
import OpenTelemetry.Attributes qualified as Attributes
import OpenTelemetry.Common qualified as Common
import OpenTelemetry.Exporter.InMemory.Span (inMemoryListExporter)
import OpenTelemetry.Trace.Core (ImmutableSpan, SpanContext (..), SpanHot (..))
import OpenTelemetry.Trace.Core qualified as Otel
import Protolude
import Test.Hspec

import BananaSplit.Persistence (
  Pg,
  conConexionDelPool,
  conTransaccionDeLecturaRapida,
  correrEnLaTransaccionDeAfuera,
  fetchGrupo,
  makePool,
 )
import BananaSplit.PgRoll qualified as PgRoll
import BananaSplit.Telemetry (Telemetry (..), telemetryFromGlobals)
import BananaSplit.Telemetry qualified as Telemetry
import BananaSplit.ULID (nullUlid)
import Site.Config qualified as Config
import TestRunner (runTest)

-- | Un 'Telemetry' igual al del suite pero con el tracer apuntando a un exporter
-- en memoria.
--
-- Esto es lo que gana que el tracer salga del reader y no de un provider global: para
-- ver los spans de una transacción alcanza con cambiarle un campo al record, sin tocar
-- estado global del proceso ni ordenar los tests entre sí.
conSpansEnMemoria :: Telemetry -> IO (Telemetry, IO [ImmutableSpan])
conSpansEnMemoria base = do
  (processor, ref) <- inMemoryListExporter
  provider <- Otel.createTracerProvider [processor] Otel.emptyTracerProviderOptions
  pure
    ( base{tracer = Otel.makeTracer provider instrumentationLibrary Otel.tracerOptions}
    , readIORef ref
    )

instrumentationLibrary :: Otel.InstrumentationLibrary
instrumentationLibrary =
  Otel.InstrumentationLibrary
    { libraryName = "test"
    , libraryVersion = ""
    , librarySchemaUrl = ""
    , libraryAttributes = Attributes.emptyAttributes
    }

atributoTexto :: ImmutableSpan -> Text -> IO (Maybe Text)
atributoTexto span clave = do
  hot <- readIORef span.spanHot
  pure $ case Attributes.lookupAttribute hot.hotAttributes clave of
    Just (Attributes.AttributeValue (Attributes.TextAttribute texto)) -> Just texto
    _ -> Nothing

-- | Una query cualquiera que no necesita datos: el grupo no existe, pero el
-- @SELECT@ corre igual, que es lo único que este módulo mira.
unaQuery :: Pg ()
unaQuery = void $ fetchGrupo nullUlid

spec :: Spec
spec = around conConexion $ do
  describe "MonadTelemetry Pg" $ do
    it "exporta el span con las queries que corrieron adentro" $ \(telemetry, _pool, conn) -> do
      (conMemoria, leerSpans) <- conSpansEnMemoria telemetry
      correrEnLaTransaccionDeAfuera conMemoria conn $ Telemetry.inSpan "adentro" unaQuery

      leerSpans >>= \case
        [span] -> do
          -- Que el atributo esté prueba las dos mitades: el span se cerró (si no,
          -- no se exportaba) y las queries del runner anidado se le colgaron a él.
          atributoTexto span "db.query.summary" `shouldReturn` Just "SELECT grupos"
        otros -> panic $ "se esperaba un span y salieron " <> show (length otros)

    -- Esto es lo que hace que un handler y un comando se vean igual en el trace: el
    -- span lo abre la transacción, no el llamador. Antes el handler lo abría a mano
    -- y un comando no abría ninguno, así que sus queries se colgaban del span que
    -- hubiera quedado activo, o de nada.
    it "la transacción abre su propio span, con las queries adentro" $ \(telemetry, _pool, conn) -> do
      (conMemoria, leerSpans) <- conSpansEnMemoria telemetry
      conTransaccionDeLecturaRapida conMemoria conn unaQuery

      leerSpans >>= \case
        [span] -> do
          hot <- readIORef span.spanHot
          hot.hotName `shouldBe` "db.read"
          -- El @kind@ también sale de un solo lado ahora. Grafana lo usa para
          -- distinguir el trabajo propio de la llamada a otro sistema.
          span.spanKind `shouldBe` Otel.Client
          atributoTexto span "db.query.summary" `shouldReturn` Just "SELECT grupos"
        otros -> panic $ "se esperaba un span y salieron " <> show (length otros)

    it "sigue el trace que ya estaba abierto" $ \(telemetry, _pool, conn) -> do
      -- El caso real: el handler abre @db.write@ y la transacción corre adentro. Lo
      -- que se quiere es que el span de adentro cuelgue de ese y no arranque un
      -- trace nuevo, que es lo que pasaría si no leyera el contexto thread-local.
      (conMemoria, leerSpans) <- conSpansEnMemoria telemetry
      traceDeAfuera <-
        Otel.inSpan' conMemoria.tracer "afuera" Otel.defaultSpanArguments $ \afuera -> do
          correrEnLaTransaccionDeAfuera conMemoria conn $ Telemetry.inSpan "adentro" unaQuery
          (.traceId) <$> Otel.getSpanContext afuera

      spans <- leerSpans
      length spans `shouldBe` 2
      fmap (.spanContext.traceId) spans `shouldBe` [traceDeAfuera, traceDeAfuera]

    it "cierra el span aunque la acción explote" $ \(telemetry, _pool, conn) -> do
      -- Sin el @bracket@ de 'Otel.inSpan'' el span quedaría abierto para siempre y
      -- no se exportaría nunca, que es la forma más silenciosa de perder una traza.
      (conMemoria, leerSpans) <- conSpansEnMemoria telemetry
      resultado <-
        try @SomeException $
          correrEnLaTransaccionDeAfuera conMemoria conn $
            Telemetry.inSpan "explota" (unaQuery >> liftIO (throwIO (ErrorCall "boom")) :: Pg ())

      resultado `shouldSatisfy` isLeft
      leerSpans >>= \spans -> length spans `shouldBe` 1

  -- Conseguir una conexión es una espera distinta de la de las queries, y cuando un
  -- endpoint tarda querés saber cuál de las dos fue. Antes quedaba sumada adentro del
  -- span de la transacción, así que no se podía separar.
  describe "conConexionDelPool" $ do
    it "deja la espera por una conexión en su propio span" $ \(telemetry, pool, _conn) -> do
      (conMemoria, leerSpans) <- conSpansEnMemoria telemetry
      conConexionDelPool conMemoria pool $ \_ -> pure ()

      leerSpans >>= \case
        [span] -> do
          hot <- readIORef span.spanHot
          hot.hotName `shouldBe` "db.pool.acquire"
          -- Que haya cerrado es lo que se puede chequear acá. Que /arranque/ cuando
          -- se pidió la conexión y no cuando llegó depende del 'startTime' que pone
          -- la implementación, y con el pool sin contención la espera es demasiado
          -- corta para distinguirla de cero.
          Common.isEnded hot.hotEnd `shouldBe` True
        otros -> panic $ "se esperaba un span y salieron " <> show (length otros)

conConexion :: ((Telemetry, Pool.Pool Connection, Connection) -> IO ()) -> IO ()
conConexion correr = do
  config <- Config.createConfig "test"
  -- Migrar es idempotente y barato, así que este spec no depende de que otro haya
  -- corrido antes, igual que "Integracion.ConcurrenciaSpec".
  telemetry <- telemetryFromGlobals config
  runTest telemetry $ do
    PgRoll.init config
    PgRoll.startAndComplete config
  bracket (makePool config) Pool.destroyAllResources $ \pool ->
    -- Sin BEGIN/ROLLBACK de afuera, a diferencia del resto del suite: todo lo que
    -- corre acá es de lectura, y los casos de abajo quieren abrir sus propias
    -- transacciones, que es justo lo que no se puede hacer anidado.
    Pool.withResource pool $ \conn ->
      correr (telemetry, pool, conn)
