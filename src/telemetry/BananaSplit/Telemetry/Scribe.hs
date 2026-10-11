-- | El puente de Katip a OpenTelemetry: un 'Katip.Scribe' que convierte cada
-- item de Katip en un log record y lo emite por OTLP.
--
-- == Por qué no usamos el paquete oficial
--
-- @hs-opentelemetry-instrumentation-katip@ hace exactamente esto, y su
-- documentación promete que la correlación con los traces es automática. No lo
-- es, y el motivo es estructural:
--
-- * 'Katip.registerScribe' encola los items y los entrega desde un worker
--   propio (@Katip.Core.spawnScribeWorker@), así que @liPush@ /nunca/ corre en
--   el hilo que logueó.
-- * El bridge oficial llama a @emitLogRecord@ sin pasar contexto, y
--   @emitLogRecord@ cae entonces al contexto thread-local /del hilo en el que
--   corre/ (@ctx <- maybe getContext pure (context args)@).
-- * Ese hilo es el del worker, que se crea al arrancar la app y nunca tiene un
--   span activo, así que @getContext@ devuelve el contexto vacío.
--
-- El resultado son log records sin @trace_id@ ni @span_id@, es decir sin el
-- cruce con el trace en Grafana — que es justamente lo que más nos interesa de
-- mandar los logs por OTLP.
--
-- Acá el 'SpanContext' se captura en el hilo que loguea (ver
-- 'BananaSplit.Telemetry.logInfo' y compañía), viaja dentro del payload del
-- item y el scribe lo reconstruye antes de emitir. El payload JSON es el único
-- canal posible: un 'Katip.Scribe' recibe el item existencialmente
-- (@forall a. LogItem a => Item a@), así que no hay forma de recuperar un valor
-- tipado del otro lado.
module BananaSplit.Telemetry.Scribe (
  LogPayload (..),
  otelScribe,
  severityToOtel,
) where

import Data.Aeson (Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Pair)
import Data.HashMap.Strict qualified as HashMap
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as Builder
import Data.Time.Clock (UTCTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Katip qualified
import Katip.Core qualified
import Language.Haskell.TH.Syntax qualified as TH
import OpenTelemetry.Common (Timestamp (..))
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Log (AnyValue (..), Logger, SeverityNumber)
import OpenTelemetry.Log qualified as Log
import OpenTelemetry.Trace.Core (SpanContext (..))
import OpenTelemetry.Trace.Core qualified as Otel
import OpenTelemetry.Trace.Id qualified as Id
import OpenTelemetry.Trace.TraceState qualified as TraceState
import Protolude

-- | El payload de un item de Katip: los atributos que puso quien loguea, más el
-- 'SpanContext' que estaba activo en ese momento.
--
-- El span context va acá y no se busca en el scribe porque allá ya es tarde: el
-- scribe corre en otro hilo (ver el encabezado del módulo).
data LogPayload = LogPayload
  { spanContext :: Maybe SpanContext
  , attributes :: [Pair]
  }

-- | Las claves con las que viaja el span context. El prefijo las marca como
-- internas: el scribe las saca del payload antes de convertirlo en atributos,
-- así que no llegan a la telemetría con este nombre.
traceIdKey, spanIdKey, sampledKey :: Key.Key
traceIdKey = "__otel.trace_id"
spanIdKey = "__otel.span_id"
sampledKey = "__otel.sampled"

instance Aeson.ToJSON LogPayload where
  toJSON payload =
    Aeson.object $ spanPairs payload.spanContext <> payload.attributes
    where
      spanPairs Nothing = []
      spanPairs (Just context) =
        [ (traceIdKey, Aeson.toJSON (Id.traceIdBaseEncodedText Id.Base16 context.traceId))
        , (spanIdKey, Aeson.toJSON (Id.spanIdBaseEncodedText Id.Base16 context.spanId))
        , (sampledKey, Aeson.toJSON (Otel.isSampled context.traceFlags))
        ]

-- | El payload se serializa tal cual lo da el 'Aeson.ToJSON' de arriba.
instance Katip.ToObject LogPayload

instance Katip.LogItem LogPayload where
  -- Nunca se descarta nada: lo que llega al payload es porque alguien lo puso a
  -- propósito, y el filtrado por severidad ya pasó antes.
  payloadKeys _ _ = Katip.AllKeys

-- | Un scribe que emite por OTLP los items que superen @minSeverity@.
--
-- @verbosity@ decide cuánto del payload se serializa; con 'Katip.V2' entra todo
-- (ver 'LogPayload').
otelScribe :: Logger -> Katip.Severity -> Katip.Verbosity -> IO Katip.Scribe
otelScribe logger minSeverity verbosity =
  pure
    Katip.Scribe
      { Katip.liPush = \item -> do
          permitted <- Katip.permitItem minSeverity item
          when permitted $ emitItem logger verbosity item
      , Katip.scribeFinalizer = pure ()
      , Katip.scribePermitItem = Katip.permitItem minSeverity
      }

emitItem :: (Katip.LogItem a) => Logger -> Katip.Verbosity -> Katip.Item a -> IO ()
emitItem logger verbosity item = do
  let (severityNumber, severityText) = severityToOtel (Katip.Core._itemSeverity item)
      payload = Katip.payloadObject verbosity (Katip.Core._itemPayload item)
      -- El nombre del evento es el mensaje: por convención del proyecto es un
      -- nombre estable y no prosa, así que sirve como identificador del evento.
      name = itemMessage item
  void $
    Log.emitLogRecord logger $
      Log.emptyLogRecordArguments
        { Log.timestamp = Just $ utcTimeToTimestamp (Katip.Core._itemTime item)
        , Log.eventName = Just name
        , Log.body = Log.toValue name
        , Log.severityNumber = Just severityNumber
        , Log.severityText = Just severityText
        , Log.context = Just $ payloadContext payload
        , Log.attributes = itemAttributes item payload
        }

-- | El 'Timestamp' de OTel son nanosegundos desde epoch.
--
-- Se usa el tiempo que Katip le puso al item y no el de ahora, porque \"ahora\"
-- acá es cuando el worker del scribe llegó a procesarlo. Sin esto el record
-- sale con @time_unix_nano = 0@ y el único tiempo que viaja es el
-- @observed_time_unix_nano@, que no es cuándo pasó el evento.
utcTimeToTimestamp :: UTCTime -> Timestamp
utcTimeToTimestamp time =
  Timestamp $ round $ nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds time) * 1_000_000_000

itemMessage :: Katip.Item a -> Text
itemMessage item =
  LazyText.toStrict $
    Builder.toLazyText $
      Katip.Core.unLogStr (Katip.Core._itemMessage item)

-- | Reconstruye el contexto desde las claves internas del payload. Si no están
-- —un log emitido fuera de un span— devuelve el contexto vacío, que es lo mismo
-- que habría dado el thread-local.
--
-- El span va como 'Otel.wrapSpanContext', o sea un span no-grabador que solo
-- lleva los ids: alcanza, porque lo único que el log record necesita del span es
-- su identidad.
payloadContext :: Aeson.Object -> Context.Context
payloadContext payload =
  case (textAt traceIdKey, textAt spanIdKey) of
    (Just rawTraceId, Just rawSpanId)
      -- Con nombres propios porque @traceId@ choca con el de 'Protolude.Debug'.
      | Right parsedTraceId <- Id.baseEncodedToTraceId Id.Base16 (encodeUtf8 rawTraceId)
      , Right parsedSpanId <- Id.baseEncodedToSpanId Id.Base16 (encodeUtf8 rawSpanId) ->
          Context.insertSpan
            ( Otel.wrapSpanContext
                SpanContext
                  { traceFlags =
                      if boolAt sampledKey
                        then Otel.setSampled Otel.defaultTraceFlags
                        else Otel.defaultTraceFlags
                  , isRemote = False
                  , traceId = parsedTraceId
                  , spanId = parsedSpanId
                  , traceState = TraceState.empty
                  }
            )
            Context.empty
    _ -> Context.empty
  where
    textAt key = case KeyMap.lookup key payload of
      Just (String value) -> Just value
      _ -> Nothing
    boolAt key = case KeyMap.lookup key payload of
      Just (Bool value) -> value
      _ -> False

-- | Los atributos del log record: el payload de quien loguea más lo que agrega
-- Katip por su cuenta (el namespace, de dónde salió).
--
-- Las claves internas del span context se descartan: su contenido ya viajó al
-- contexto en 'payloadContext' y repetirlo como atributo solo sería ruido.
itemAttributes :: Katip.Item a -> Aeson.Object -> HashMap.HashMap Text AnyValue
itemAttributes item payload =
  HashMap.union
    (HashMap.fromList $ ("katip.namespace", Log.toValue namespace) : ubicacion)
    (objectToAttributes (KeyMap.filterWithKey (\key _ -> key `notElem` internalKeys) payload))
  where
    Katip.Namespace segments = Katip.Core._itemNamespace item
    namespace = Text.intercalate "." segments
    internalKeys = [traceIdKey, spanIdKey, sampledKey]

    -- De dónde salió el log, cuando el item lo trae: los helpers usan 'logLocM',
    -- que la saca de 'HasCallStack'. Los nombres son los estables de semconv.
    ubicacion = case Katip.Core._itemLoc item of
      Nothing -> []
      Just loc ->
        [ ("code.file.path", Log.toValue (Text.pack (TH.loc_filename loc)))
        , ("code.line.number", IntValue (fromIntegral (fst (TH.loc_start loc))))
        , ("code.function.name", Log.toValue (Text.pack (TH.loc_module loc)))
        ]

objectToAttributes :: Aeson.Object -> HashMap.HashMap Text AnyValue
objectToAttributes object =
  HashMap.fromList
    [ (Key.toText key, valueToAttribute value)
    | (key, value) <- KeyMap.toList object
    ]

valueToAttribute :: Value -> AnyValue
valueToAttribute = \case
  String value -> TextValue value
  Number value -> DoubleValue (realToFrac value)
  Bool value -> BoolValue value
  Null -> NullValue
  Array values -> ArrayValue (valueToAttribute <$> toList values)
  Object object -> HashMapValue (objectToAttributes object)

-- | El mapeo de severidades. Katip tiene ocho niveles y OTel una escala más
-- fina, así que la traducción es la natural.
severityToOtel :: Katip.Severity -> (SeverityNumber, Text)
severityToOtel = \case
  Katip.DebugS -> (Log.Debug, "DEBUG")
  Katip.InfoS -> (Log.Info, "INFO")
  Katip.NoticeS -> (Log.Info2, "NOTICE")
  Katip.WarningS -> (Log.Warn, "WARN")
  Katip.ErrorS -> (Log.Error, "ERROR")
  Katip.CriticalS -> (Log.Fatal, "CRITICAL")
  Katip.AlertS -> (Log.Fatal2, "ALERT")
  Katip.EmergencyS -> (Log.Fatal4, "EMERGENCY")
