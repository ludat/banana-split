module BananaSplit.Telemetry.ScribeSpec (
  spec,
) where

import Data.Aeson ((.=))
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Katip qualified
import OpenTelemetry.Common (Timestamp (..))
import OpenTelemetry.Exporter.InMemory.LogRecord (getExportedLogRecords, inMemoryLogRecordExporter)
import OpenTelemetry.Internal.Log.Types (
  ImmutableLogRecord (..),
  SeverityNumber (..),
  TracingDetails (..),
  toBaseMaybe,
 )
import OpenTelemetry.Log (AnyValue (..))
import OpenTelemetry.Log.Core (
  createLoggerProvider,
  emptyLoggerProviderOptions,
  forceFlushLoggerProvider,
  instrumentationLibrary,
  makeLogger,
  readLogRecord,
 )
import OpenTelemetry.LogAttributes qualified as LogAttributes
import OpenTelemetry.Processor.Simple.LogRecord (
  SimpleLogRecordProcessorConfig (..),
  simpleLogRecordProcessor,
 )
import OpenTelemetry.Trace.Core (SpanContext (..))
import OpenTelemetry.Trace.Core qualified as Otel
import OpenTelemetry.Trace.Id qualified as Id
import OpenTelemetry.Trace.TraceState qualified as TraceState
import Protolude
import Test.Hspec

import BananaSplit.Telemetry qualified as Telemetry
import BananaSplit.Telemetry.Scribe (LogPayload (..), otelScribe)

-- | Corre un log por el scribe y devuelve el único record que salió del otro
-- lado, ya exportado.
--
-- El @closeScribes@ es lo que hace falta esperar: Katip entrega los items desde
-- un worker propio, así que sin cerrar el scribe el record todavía no existe
-- cuando el test lo busca.
emitThrough :: LogPayload -> Katip.Severity -> Text -> IO ImmutableLogRecord
emitThrough payload severity message =
  conScribe payload $ Katip.logFM severity (Katip.ls message)

-- | Corre una acción en la mónada de Katip, con el scribe de OTLP colgado y un
-- exporter en memoria del otro lado, y devuelve el único record que salió.
conScribe :: LogPayload -> Katip.KatipContextT IO () -> IO ImmutableLogRecord
conScribe payload accion = do
  (exporter, ref) <- inMemoryLogRecordExporter
  processor <- simpleLogRecordProcessor (SimpleLogRecordProcessorConfig exporter 30000000)
  loggerProvider <- createLoggerProvider [processor] emptyLoggerProviderOptions
  let logger = makeLogger loggerProvider (instrumentationLibrary "test" "0.0.0")
  scribe <- otelScribe logger Katip.DebugS Katip.V2
  logEnv <-
    Katip.registerScribe "otlp" scribe Katip.defaultScribeSettings
      =<< Katip.initLogEnv "test" "test"
  Katip.runKatipContextT logEnv payload mempty accion
  Katip.closeScribes logEnv
  void $ forceFlushLoggerProvider loggerProvider Nothing
  records <- getExportedLogRecords ref
  case records of
    [record] -> readLogRecord record
    other -> panic $ "se esperaba un record y salieron " <> show (length other)

sampledContext :: Text -> Text -> SpanContext
sampledContext rawTraceId rawSpanId =
  SpanContext
    { traceFlags = Otel.setSampled Otel.defaultTraceFlags
    , isRemote = False
    , traceId = either (panic . toS) identity $ Id.baseEncodedToTraceId Id.Base16 (encodeUtf8 rawTraceId)
    , spanId = either (panic . toS) identity $ Id.baseEncodedToSpanId Id.Base16 (encodeUtf8 rawSpanId)
    , traceState = TraceState.empty
    }

attributesOf :: ImmutableLogRecord -> HashMap.HashMap Text AnyValue
attributesOf record = snd $ LogAttributes.getAttributeMap record.logRecordAttributes

spec :: Spec
spec = do
  -- Esto existe porque es lo que el bridge oficial
  -- (@hs-opentelemetry-instrumentation-katip@) hace mal: promete correlación
  -- automática con los traces y no la tiene, porque emite desde el worker de
  -- Katip, donde el contexto thread-local está vacío. Si alguien cambia este
  -- scribe por el de upstream, o si 'BananaSplit.Telemetry.logEvent' deja de
  -- capturar el span context, este test se cae.
  describe "correlación con el trace" $ do
    let rawTraceId = "4bf92f3577b34da6a3ce929d0e0e4736"
        rawSpanId = "00f067aa0ba902b7"

    it "el record sale con el trace y el span del contexto que viajó en el payload" $ do
      record <-
        emitThrough
          LogPayload{spanContext = Just (sampledContext rawTraceId rawSpanId), attributes = []}
          Katip.InfoS
          "auth.logged_in"

      case record.logRecordTracingDetails of
        TracingDetails traceId spanId _ -> do
          Id.traceIdBaseEncodedText Id.Base16 traceId `shouldBe` rawTraceId
          Id.spanIdBaseEncodedText Id.Base16 spanId `shouldBe` rawSpanId
        NoTracingDetails ->
          expectationFailure "el record salió sin trace, no se puede cruzar con el span"

    it "un log fuera de un span sale sin trace y sin romperse" $ do
      record <-
        emitThrough
          LogPayload{spanContext = Nothing, attributes = []}
          Katip.InfoS
          "migration.pgroll.start"

      record.logRecordTracingDetails `shouldBe` NoTracingDetails

  describe "atributos" $ do
    it "pasa los que puso quien loguea" $ do
      record <-
        emitThrough
          LogPayload{spanContext = Nothing, attributes = ["app.user.email" .= ("a@b.com" :: Text)]}
          Katip.InfoS
          "auth.code_sent"

      HashMap.lookup "app.user.email" (attributesOf record)
        `shouldBe` Just (TextValue "a@b.com")

    -- Las claves con las que viaja el span context son un detalle interno del
    -- transporte: si se filtran como atributos, cada log queda con el trace id
    -- repetido al lado del que ya lleva el record.
    it "no filtra las claves internas del span context" $ do
      record <-
        emitThrough
          LogPayload
            { spanContext = Just (sampledContext "4bf92f3577b34da6a3ce929d0e0e4736" "00f067aa0ba902b7")
            , attributes = []
            }
          Katip.InfoS
          "auth.logged_in"

      let keys = HashMap.keys (attributesOf record)
      filter (Text.isPrefixOf "__otel") keys `shouldBe` []

    it "lleva el namespace de Katip" $ do
      record <-
        emitThrough
          LogPayload{spanContext = Nothing, attributes = []}
          Katip.InfoS
          "auth.logged_in"

      HashMap.lookup "katip.namespace" (attributesOf record)
        `shouldBe` Just (TextValue "test")

  describe "el record" $ do
    -- El cuerpo es el nombre del evento, no prosa: es la convención del
    -- proyecto, acá y en el frontend, porque un nombre estable es lo que se
    -- puede consultar.
    it "usa el mensaje como nombre del evento y como cuerpo" $ do
      record <-
        emitThrough
          LogPayload{spanContext = Nothing, attributes = []}
          Katip.InfoS
          "auth.logged_in"

      toBaseMaybe record.logRecordEventName `shouldBe` Just "auth.logged_in"
      record.logRecordBody `shouldBe` TextValue "auth.logged_in"

    -- Sin esto el record sale con @time_unix_nano = 0@ y el único tiempo que
    -- viaja es el @observed_time_unix_nano@, o sea cuándo el worker del scribe
    -- llegó a procesarlo en vez de cuándo pasó el evento.
    it "lleva el tiempo del item, no el del momento en que se exportó" $ do
      before <- getCurrentTime
      record <-
        emitThrough
          LogPayload{spanContext = Nothing, attributes = []}
          Katip.InfoS
          "auth.logged_in"
      after <- getCurrentTime

      case toBaseMaybe record.logRecordTimestamp of
        Nothing -> expectationFailure "el record salió sin timestamp"
        Just (Timestamp nanos) -> do
          let seconds = fromIntegral nanos / 1_000_000_000 :: Double
              asDouble = realToFrac . utcTimeToPOSIXSeconds
          seconds `shouldSatisfy` (>= asDouble before)
          seconds `shouldSatisfy` (<= asDouble after)

    it "traduce la severidad de Katip a la de OTel" $ do
      let severityOf severity = do
            record <-
              emitThrough
                LogPayload{spanContext = Nothing, attributes = []}
                severity
                "evento"
            pure $ toBaseMaybe record.logRecordSeverityNumber

      severityOf Katip.DebugS `shouldReturn` Just Debug
      severityOf Katip.InfoS `shouldReturn` Just Info
      severityOf Katip.WarningS `shouldReturn` Just Warn
      severityOf Katip.ErrorS `shouldReturn` Just Error

  -- Esto es lo que el wrapper viejo hacía imposible: hacía un
  -- 'runKatipContextT' por llamada, con payload fresco, así que no había forma
  -- de poner contexto una vez arriba y que lo heredaran los logs de abajo.
  describe "los helpers de BananaSplit.Telemetry" $ do
    it "conservan el contexto que puso quien llama y le suman el propio" $ do
      record <-
        conScribe (LogPayload{spanContext = Nothing, attributes = []}) $
          Katip.katipAddContext (Katip.sl "app.request.id" ("req-1" :: Text)) $
            Telemetry.logInfo "auth.logged_in" [Telemetry.logAttr "enduser.id" ("u1" :: Text)]

      let attrs = attributesOf record
      HashMap.lookup "app.request.id" attrs `shouldBe` Just (TextValue "req-1")
      HashMap.lookup "enduser.id" attrs `shouldBe` Just (TextValue "u1")

    it "respetan el namespace que puso quien llama" $ do
      record <-
        conScribe (LogPayload{spanContext = Nothing, attributes = []}) $
          Katip.katipAddNamespace "auth" $
            Telemetry.logInfo "auth.logged_in" []

      HashMap.lookup "katip.namespace" (attributesOf record)
        `shouldBe` Just (TextValue "test.auth")

    -- Los helpers usan 'logLocM' con 'withFrozenCallStack', así que la ubicación
    -- apunta al call site y no al helper.
    it "dejan de qué archivo salió el log" $ do
      record <-
        conScribe (LogPayload{spanContext = Nothing, attributes = []}) $
          Telemetry.logInfo "auth.logged_in" []

      case HashMap.lookup "code.file.path" (attributesOf record) of
        Just (TextValue path) -> path `shouldSatisfy` Text.isSuffixOf "ScribeSpec.hs"
        otro -> expectationFailure $ "se esperaba el archivo y hubo " <> show otro
