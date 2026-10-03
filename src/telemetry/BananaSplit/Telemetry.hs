-- | El setup de OpenTelemetry y los helpers que valen en cualquier parte de la
-- app, sea un handler HTTP o un comando de consola.
--
-- Está en su propia librería interna porque lo usan tanto @database@ (las
-- migraciones) como @http@ (los requests), y @database@ no puede depender de
-- @http@. Lo que es específico de HTTP —el middleware, el 'Site.Mailer'
-- instrumentado— vive en "Site.Telemetry"; lo que hay acá es el único lugar
-- donde se levanta el SDK.
--
-- Se configura con las variables de entorno estándar de OpenTelemetry
-- (@OTEL_EXPORTER_OTLP_ENDPOINT@, @OTEL_SERVICE_NAME@, @OTEL_TRACES_SAMPLER@,
-- etc.) y no con Conferer: son las que entiende cualquier SDK de OTel, las que
-- documenta la especificación y las que ya usa el resto del ecosistema.
--
-- Sin @OTEL_EXPORTER_OTLP_ENDPOINT@ no se exporta nada, igual que en el
-- frontend: un @cabal run@ suelto, sin stack de observabilidad levantado, no
-- tiene que llenar la consola de exports fallidos. Los spans y los log records
-- se siguen creando (contra providers no-op) para que el código instrumentado
-- sea el mismo en los dos casos.
module BananaSplit.Telemetry (
  Telemetry (..),
  telemetryFromGlobals,
  withTelemetry,

  -- * Spans
  inSpan,
  inSpan',

  -- * Log records
  logAttr,
  logDebug,
  logError,
  logInfo,
  logWarn,
) where

import Data.HashMap.Strict qualified as HashMap
import OpenTelemetry.Attributes (emptyAttributes)
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import OpenTelemetry.Log (AnyValue, Logger, SeverityNumber, ToValue)
import OpenTelemetry.Log qualified as Log
import OpenTelemetry.Metric (Meter)
import OpenTelemetry.Metric qualified as Metric
import OpenTelemetry.Propagator (TextMapPropagator)
import OpenTelemetry.Propagator qualified as Propagator
import OpenTelemetry.SDK (OTelSignals (..), withOpenTelemetry)
import OpenTelemetry.Trace.Core (InstrumentationLibrary (..), Span, Tracer)
import OpenTelemetry.Trace.Core qualified as Otel
import Protolude
import System.Environment (lookupEnv, setEnv)

-- | Lo que hace falta para instrumentar. Se arma una vez en 'withTelemetry' y se
-- pasa para abajo.
data Telemetry = Telemetry
  { tracer :: Tracer
  , logger :: Logger
  -- ^ Para log records por OTLP. Los @putText@ que ya había siguen yendo a
  -- stdout: esto es adicional, no un reemplazo, así que un dev sin stack
  -- levantado no pierde la consola que tenía.
  , meter :: Meter
  -- ^ Para crear instrumentos. Los instrumentos concretos los crea quien los
  -- usa (ver 'Site.Telemetry.traceApiMiddleware'), porque hay que crearlos una
  -- sola vez y no por medición.
  , propagators :: TextMapPropagator
  }

-- | Levanta los tres providers (traces, métricas, logs), corre la acción y los
-- baja al salir. El shutdown es lo que hace el flush final, así que tiene que
-- envolver a todo el trabajo: lo del último request (o el último lote de una
-- migración) se exporta recién ahí.
withTelemetry :: (Telemetry -> IO a) -> IO a
withTelemetry act = do
  endpoint <- telemetryEndpoint
  case endpoint of
    Nothing -> do
      putText "[telemetry] off: no hay OTEL_EXPORTER_OTLP_ENDPOINT"
      -- Sin esto el SDK usa el default de la especificación (localhost:4318) y
      -- se pone a reintentar contra un puerto cerrado. Van como defaults y no
      -- forzados para no pisar a alguien que eligió a mano otro exporter (el
      -- de consola, por ejemplo) justamente para no necesitar endpoint.
      setDefaultEnv "OTEL_TRACES_EXPORTER" "none"
      setDefaultEnv "OTEL_METRICS_EXPORTER" "none"
      setDefaultEnv "OTEL_LOGS_EXPORTER" "none"
      start
    Just url -> do
      putText $ "[telemetry] on, exporting to " <> toS url
      start
  where
    start = do
      -- Solo default: si ya viene del ambiente, manda el ambiente.
      setDefaultEnv "OTEL_SERVICE_NAME" "banana-split"
      withOpenTelemetry (mkTelemetry >=> act)

-- | Un 'Telemetry' armado con los providers globales, para código que no quiere
-- levantar el SDK: los tests, que corren migraciones y no tienen nada que
-- observar.
--
-- Sin un SDK instalado —el caso de los tests— los providers globales son
-- no-ops: instrumentar funciona y no sale nada hacia ningún lado. Si /sí/ hay
-- uno instalado, esto devuelve el de verdad; no es una forma de apagar la
-- telemetría, es una forma de no tener que encenderla.
telemetryFromGlobals :: IO Telemetry
telemetryFromGlobals = do
  tracerProvider <- Otel.getGlobalTracerProvider
  loggerProvider <- Log.getGlobalLoggerProvider
  meterProvider <- Metric.getGlobalMeterProvider
  logger <- Log.getLogger loggerProvider instrumentationLibrary
  meter <- Metric.getMeter meterProvider instrumentationLibrary
  propagators <- Propagator.getGlobalTextMapPropagator
  pure
    Telemetry
      { tracer = Otel.makeTracer tracerProvider instrumentationLibrary Otel.tracerOptions
      , logger = logger
      , meter = meter
      , propagators = propagators
      }

mkTelemetry :: OTelSignals -> IO Telemetry
mkTelemetry signals = do
  meter <- Metric.getMeter signals.otelMeterProvider instrumentationLibrary
  logger <- Log.getLogger signals.otelLoggerProvider instrumentationLibrary
  -- OJO: el global, /no/ @signals.otelPropagators@. Ese último viene como un
  -- propagador vacío (@propagatorFields@ da @[]@ y extraer por él no devuelve
  -- ningún span), con lo cual el @traceparent@ del browser se ignora y todos
  -- los spans del backend quedan como raíz en lugar de colgar del trace que
  -- arrancó en el frontend. El que el SDK configura de verdad es este.
  propagators <- Propagator.getGlobalTextMapPropagator
  -- Un propagador que no declara nada es el no-op: nada de lo que mande el
  -- browser se va a continuar. Pasa con @OTEL_PROPAGATORS=none@ y pasaba con
  -- @signals.otelPropagators@, así que vale avisarlo en voz alta.
  when (null (Propagator.propagatorFields propagators)) $
    putText
      "[telemetry] warning: el propagador no declara nada, los traces del frontend y del backend van a quedar separados"
  pure
    Telemetry
      { tracer = Otel.makeTracer signals.otelTracerProvider instrumentationLibrary Otel.tracerOptions
      , logger = logger
      , meter = meter
      , propagators = propagators
      }

instrumentationLibrary :: InstrumentationLibrary
instrumentationLibrary =
  InstrumentationLibrary
    { libraryName = "banana-split"
    , libraryVersion = ""
    , librarySchemaUrl = ""
    , libraryAttributes = emptyAttributes
    }

-- | @Nothing@ cuando no hay a dónde exportar. Mira las dos variables que puede
-- usar el exporter: la general y la específica de traces.
telemetryEndpoint :: IO (Maybe [Char])
telemetryEndpoint = do
  general <- lookupEnv "OTEL_EXPORTER_OTLP_ENDPOINT"
  traces <- lookupEnv "OTEL_EXPORTER_OTLP_TRACES_ENDPOINT"
  pure $ find (not . null) $ catMaybes [traces, general]

setDefaultEnv :: [Char] -> [Char] -> IO ()
setDefaultEnv name value = do
  existing <- lookupEnv name
  when (isNothing existing) $ setEnv name value

-- | Un span alrededor de un pedazo de trabajo en 'IO'. Cuelga de lo que haya
-- activo en el contexto thread-local, así que anida solo.
inSpan :: Telemetry -> Text -> IO a -> IO a
inSpan telemetry name = Otel.inSpan telemetry.tracer name Otel.defaultSpanArguments

-- | Como 'inSpan', pero te da el span para colgarle atributos.
inSpan' :: Telemetry -> Text -> (Span -> IO a) -> IO a
inSpan' telemetry name = Otel.inSpan' telemetry.tracer name Otel.defaultSpanArguments

-- | Emite un log record por OTLP, correlacionado con el trace: lleva el
-- contexto activo, así que Grafana lo cruza con el span que lo produjo (ya está
-- configurado en el datasource de Loki) y se ve el /por qué/ al lado del resto
-- del trace en lugar de suelto en stdout.
--
-- El cuerpo del record es el nombre del evento, nunca prosa: el detalle va en
-- los atributos. Es lo mismo que hace el frontend, y por la misma razón — un
-- nombre estable es lo que se puede consultar.
logEvent :: SeverityNumber -> Telemetry -> Text -> [(Text, AnyValue)] -> IO ()
logEvent severity telemetry name attributes = do
  context <- ThreadLocal.getContext
  void $
    Log.emitLogRecord telemetry.logger $
      Log.emptyLogRecordArguments
        { Log.eventName = Just name
        , Log.body = Log.toValue name
        , Log.severityNumber = Just severity
        , Log.context = Just context
        , Log.attributes = HashMap.fromList attributes
        }

-- | Algo pasó y salió como se esperaba.
logInfo :: Telemetry -> Text -> [(Text, AnyValue)] -> IO ()
logInfo = logEvent Log.Info

-- | Algo salió mal pero es una respuesta válida del sistema: un código
-- equivocado, un rate limit, un mail que no se pudo atribuir a nadie.
logWarn :: Telemetry -> Text -> [(Text, AnyValue)] -> IO ()
logWarn = logEvent Log.Warn

-- | Algo falló y no se pudo hacer el trabajo.
logError :: Telemetry -> Text -> [(Text, AnyValue)] -> IO ()
logError = logEvent Log.Error

-- | Para lo que pasa seguido y solo importa cuando estás mirando de cerca.
logDebug :: Telemetry -> Text -> [(Text, AnyValue)] -> IO ()
logDebug = logEvent Log.Debug

-- | Azúcar para armar los atributos de un log record.
logAttr :: (ToValue a) => Text -> a -> (Text, AnyValue)
logAttr name value = (name, Log.toValue value)
