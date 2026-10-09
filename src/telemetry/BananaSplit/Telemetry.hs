module BananaSplit.Telemetry (
  Telemetry (..),
  registerRuntimeMetrics,
  telemetryFromGlobals,
  withLogging,
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

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.Types (Pair)
import GHC.Stats (getRTSStatsEnabled)
import Katip (Environment (..), KatipContext, LogEnv, Namespace (..), Severity (..), Verbosity (..))
import Katip qualified
import OpenTelemetry.Attributes (emptyAttributes)
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as ThreadLocal
import OpenTelemetry.Instrumentation.GHCMetrics qualified as GHCMetrics
import OpenTelemetry.Instrumentation.ProcessMetrics qualified as ProcessMetrics
import OpenTelemetry.Log (Logger)
import OpenTelemetry.Log qualified as Log
import OpenTelemetry.Metric (Meter)
import OpenTelemetry.Metric qualified as Metric
import OpenTelemetry.Propagator (TextMapPropagator)
import OpenTelemetry.Propagator qualified as Propagator
import OpenTelemetry.SDK (OTelSignals (..), withOpenTelemetry)
import OpenTelemetry.Trace.Core (InstrumentationLibrary (..), Span, SpanContext, Tracer)
import OpenTelemetry.Trace.Core qualified as Otel
import Protolude
import System.Environment (lookupEnv, setEnv)

import BananaSplit.Telemetry.Scribe (LogPayload (..), otelScribe)

-- | Lo que hace falta para instrumentar. Se arma una vez en 'withTelemetry' y se
-- pasa para abajo.
data Telemetry = Telemetry
  { tracer :: Tracer
  , logEnv :: LogEnv
  -- ^ El frontend de logging: Katip. Tiene dos scribes colgados, uno a stdout y
  -- uno a OTLP, así que un log sale por los dos lados y un dev sin stack de
  -- observabilidad levantado sigue viendo todo en la consola.
  --
  -- El 'OpenTelemetry.Log.Logger' que hay del otro lado /no/ está acá a
  -- propósito: ver la nota del encabezado del módulo.
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
  logEnv <- mkLogEnv logger
  pure
    Telemetry
      { tracer = Otel.makeTracer tracerProvider instrumentationLibrary Otel.tracerOptions
      , logEnv = logEnv
      , meter = meter
      , propagators = propagators
      }

-- | El 'LogEnv' de Katip con los dos scribes.
--
-- El de stdout va con 'Katip.V2' y formato JSON: en Kubernetes los logs del
-- contenedor son la red de contención cuando el OTLP no anda, y en JSON siguen
-- siendo consultables.
--
-- El de OTLP es el de "BananaSplit.Telemetry.Scribe". Cuando no hay endpoint el
-- pipeline de logs del SDK es no-op, así que el scribe queda colgado igual y no
-- intenta mandar nada: el código instrumentado es el mismo en los dos casos.
mkLogEnv :: Logger -> IO LogEnv
mkLogEnv logger = do
  serviceName <- fromMaybe "banana-split" <$> lookupEnv "OTEL_SERVICE_NAME"
  -- Solo etiqueta la salida de consola: lo que viaja por OTLP toma el ambiente
  -- del resource del SDK (@OTEL_RESOURCE_ATTRIBUTES@), no de acá.
  environment <- fromMaybe "local" <$> lookupEnv "DEPLOYMENT_ENVIRONMENT"
  base <- Katip.initLogEnv (Namespace [toS serviceName]) (Environment $ toS environment)
  console <-
    Katip.mkHandleScribeWithFormatter
      Katip.jsonFormat
      (Katip.ColorLog False)
      stdout
      (Katip.permitItem InfoS)
      V2
  otlp <- otelScribe logger DebugS V2
  base
    & Katip.registerScribe "stdout" console Katip.defaultScribeSettings
      >>= Katip.registerScribe "otlp" otlp Katip.defaultScribeSettings

mkTelemetry :: OTelSignals -> IO Telemetry
mkTelemetry signals = do
  meter <- Metric.getMeter signals.otelMeterProvider instrumentationLibrary
  logger <- Log.getLogger signals.otelLoggerProvider instrumentationLibrary
  logEnv <- mkLogEnv logger
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
      , logEnv = logEnv
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

registerRuntimeMetrics :: Telemetry -> IO ()
registerRuntimeMetrics telemetry = do
  rtsEnabled <- getRTSStatsEnabled
  unless rtsEnabled $
    putText
      "[telemetry] warning: el RTS no tiene estadísticas habilitadas (falta -T), no va a haber métricas de runtime"
  void $ GHCMetrics.registerGHCMetrics telemetry.meter
  void $ ProcessMetrics.registerProcessMetrics telemetry.meter

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
-- | Lo que hacen todos los helpers de abajo: agrega los atributos al contexto de
-- Katip y emite.
--
-- Es 'Katip.katipAddContext' y no 'Katip.runKatipContextT' justamente para no
-- tapar a Katip: el contexto que haya puesto quien llama —un
-- 'Katip.katipAddContext' con el user y el grupo al principio del handler, un
-- 'Katip.katipAddNamespace'— se conserva y se mezcla con esto.
--
-- El 'SpanContext' se agrega acá y no se deja al llamador porque es gratis y
-- siempre correcto: sin él el log record no se cruza con el trace.
logEvent :: (KatipContext m, HasCallStack) => Severity -> Text -> [Pair] -> m ()
logEvent severity name attributes = do
  spanContext <- liftIO currentSpanContext
  Katip.katipAddContext (LogPayload{spanContext, attributes}) $
    withFrozenCallStack $
      Katip.logLocM severity (Katip.ls name)

-- | Establece la mónada de Katip para un bloque de código que corre en 'IO'.
--
-- Para los comandos de consola, que no tienen una mónada de aplicación donde
-- colgar las instancias como la tiene 'Site.Types.AppHandler'. Adentro se usa
-- Katip normalmente, incluidos 'Katip.katipAddContext' y
-- 'Katip.katipAddNamespace'.
--
-- Es el límite explícito, y la alternativa a envolver cada log en su propio
-- @runKatipContextT@ —que es lo que había antes y lo que hacía imposible acumular
-- contexto.
withLogging :: Telemetry -> Katip.KatipContextT IO a -> IO a
withLogging telemetry = Katip.runKatipContextT telemetry.logEnv () mempty

-- | El 'SpanContext' activo en /este/ hilo, para que viaje con el item.
--
-- Se lee acá y no en el scribe porque el scribe corre en otro hilo: Katip
-- entrega los items desde un worker propio, donde el contexto thread-local está
-- vacío. Ver el encabezado de "BananaSplit.Telemetry.Scribe".
currentSpanContext :: IO (Maybe SpanContext)
currentSpanContext = do
  context <- ThreadLocal.getContext
  traverse Otel.getSpanContext (Context.lookupSpan context)

-- | Algo pasó y salió como se esperaba.
logInfo :: (KatipContext m, HasCallStack) => Text -> [Pair] -> m ()
logInfo = withFrozenCallStack (logEvent InfoS)

-- | Algo salió mal pero es una respuesta válida del sistema: un código
-- equivocado, un rate limit, un mail que no se pudo atribuir a nadie.
logWarn :: (KatipContext m, HasCallStack) => Text -> [Pair] -> m ()
logWarn = withFrozenCallStack (logEvent WarningS)

-- | Algo falló y no se pudo hacer el trabajo.
logError :: (KatipContext m, HasCallStack) => Text -> [Pair] -> m ()
logError = withFrozenCallStack (logEvent ErrorS)

-- | Para lo que pasa seguido y solo importa cuando estás mirando de cerca.
logDebug :: (KatipContext m, HasCallStack) => Text -> [Pair] -> m ()
logDebug = withFrozenCallStack (logEvent DebugS)

-- | Azúcar para armar los atributos de un log record.
--
-- El payload de Katip es JSON, así que los atributos son pares de Aeson y lo
-- que hace falta del valor es 'Aeson.ToJSON'. El scribe los traduce a los tipos
-- de OTel del otro lado.
logAttr :: (Aeson.ToJSON a) => Text -> a -> Pair
logAttr name value = Key.fromText name .= value
