module BananaSplit.Telemetry (
  Telemetry (..),
  registerRuntimeMetrics,
  telemetryFromGlobals,
  withTelemetry,

  -- * Spans
  MonadTelemetry (..),
  conSpanActivo,
  inSpanConTracer',

  -- * Log records
  logAttr,
  runLogging,
  logDebug,
  logError,
  logInfo,
  logWarn,
) where

import Conferer qualified
import Control.Monad.IO.Unlift (MonadUnliftIO)
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
withTelemetry :: Conferer.Config -> (Telemetry -> IO a) -> IO a
withTelemetry config act = do
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
      withOpenTelemetry (mkTelemetry config >=> act)

-- | Un 'Telemetry' armado con los providers globales, para código que no quiere
-- levantar el SDK: los tests, que corren migraciones y no tienen nada que
-- observar.
--
-- Sin un SDK instalado —el caso de los tests— los providers globales son
-- no-ops: instrumentar funciona y no sale nada hacia ningún lado. Si /sí/ hay
-- uno instalado, esto devuelve el de verdad; no es una forma de apagar la
-- telemetría, es una forma de no tener que encenderla.
telemetryFromGlobals :: Conferer.Config -> IO Telemetry
telemetryFromGlobals config = do
  tracerProvider <- Otel.getGlobalTracerProvider
  loggerProvider <- Log.getGlobalLoggerProvider
  meterProvider <- Metric.getGlobalMeterProvider
  logger <- Log.getLogger loggerProvider instrumentationLibrary
  meter <- Metric.getMeter meterProvider instrumentationLibrary
  propagators <- Propagator.getGlobalTextMapPropagator
  logEnv <- mkLogEnv config logger
  pure
    Telemetry
      { tracer = Otel.makeTracer tracerProvider instrumentationLibrary Otel.tracerOptions
      , logEnv = logEnv
      , meter = meter
      , propagators = propagators
      }

-- | La configuración de Katip, por Conferer.
--
-- Esto es configuración de la app, no del SDK de OpenTelemetry, así que va por
-- el mismo camino que el resto —@config/dev.properties@, variables
-- @BANANASPLIT_*@, argumentos de línea de comandos— en lugar de leer el ambiente
-- a mano. Lo del SDK (endpoints, intervalos, exporters) sigue en variables
-- @OTEL_*@ porque son las que documenta la especificación y las que entiende
-- cualquier SDK de OTel.
--
-- Las claves, todas opcionales:
--
-- * @log.namespace@ — el namespace raíz de Katip. Default @banana-split@.
-- * @log.environment@ — la etiqueta de ambiente de Katip. Default @local@.
-- * @log.severity@ — desde qué severidad se escribe en la terminal. Default
--   @info@.
-- * @log.console@ — si además de OTLP se escribe en la terminal. Sin valor, lo
--   decide 'consolaPedida'.
data LogConfig = LogConfig
  { namespace :: Namespace
  , environment :: Environment
  , severity :: Severity
  , console :: Maybe Bool
  }

fetchLogConfig :: Conferer.Config -> IO LogConfig
fetchLogConfig config = do
  namespace <- Conferer.fetchFromConfig @(Maybe Text) "log.namespace" config
  environment <- Conferer.fetchFromConfig @(Maybe Text) "log.environment" config
  severityText <- Conferer.fetchFromConfig @(Maybe Text) "log.severity" config
  console <- Conferer.fetchFromConfig @(Maybe Bool) "log.console" config
  severity <- case severityText of
    Nothing -> pure InfoS
    Just texto ->
      Katip.textToSeverity texto
        & maybe
          ( panic $
              "unknown log.severity: "
                <> texto
                <> " (expected debug, info, notice, warning, error, critical, alert or emergency)"
          )
          pure
  pure
    LogConfig
      { namespace = Namespace [fromMaybe "banana-split" namespace]
      , environment = Environment $ fromMaybe "local" environment
      , severity = severity
      , console = console
      }

mkLogEnv :: Conferer.Config -> Logger -> IO LogEnv
mkLogEnv config logger = do
  logConfig <- fetchLogConfig config
  base <- Katip.initLogEnv logConfig.namespace logConfig.environment
  otlp <- otelScribe logger DebugS V2
  conOtlp <- Katip.registerScribe "otlp" otlp Katip.defaultScribeSettings base
  consola <- consolaPedida logConfig.console
  if not consola
    then pure conOtlp
    else do
      console <-
        Katip.mkHandleScribeWithFormatter
          Katip.bracketFormat
          Katip.ColorIfTerminal
          stdout
          (Katip.permitItem logConfig.severity)
          V2
      Katip.registerScribe "stdout" console Katip.defaultScribeSettings conOtlp

consolaPedida :: Maybe Bool -> IO Bool
consolaPedida = \case
  Just pedida -> pure pedida
  Nothing -> isNothing <$> telemetryEndpoint

mkTelemetry :: Conferer.Config -> OTelSignals -> IO Telemetry
mkTelemetry config signals = do
  meter <- Metric.getMeter signals.otelMeterProvider instrumentationLibrary
  logger <- Log.getLogger signals.otelLoggerProvider instrumentationLibrary
  logEnv <- mkLogEnv config logger
  propagators <- Propagator.getGlobalTextMapPropagator
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

-- | Poder abrir spans.
--
-- La capacidad, no la implementación: lo que la app necesita de la telemetría para
-- instrumentarse es esto, y quién sabe abrir un span es cosa de la mónada concreta
-- que elija cada entry point.
--
-- Hay dos implementaciones y son genuinamente distintas, que es lo que justifica
-- la clase:
--
-- * las mónadas con 'MonadUnliftIO' y el 'Telemetry' en el reader la derivan de
--   'inSpanConTracer'' en una línea;
-- * 'Site.Types.AppHandler' no tiene 'MonadUnliftIO' —abajo está el @ExceptT@ de
--   Servant— así que la implementa a mano, corriendo el handler para que un
--   @ServerError@ no se escape sin cerrar el span.
--
-- Las métricas no están acá porque los instrumentos se crean una sola vez al
-- arrancar, así que nadie los necesita de forma polimórfica. Los logs tampoco: esa
-- capacidad ya es 'Katip.KatipContext', que es exactamente esta misma idea y la
-- trae la librería.
class (Monad m) => MonadTelemetry m where
  -- | Un span alrededor de un pedazo de trabajo. Cuelga de lo que haya activo en
  -- el contexto thread-local, así que anida solo.
  inSpan :: Text -> m a -> m a
  inSpan name = inSpan' name . const

  -- | Como 'inSpan', pero te da el span para colgarle atributos.
  inSpan' :: Text -> (Span -> m a) -> m a

-- | Instrumentar desde código que no tiene una mónada donde pedir
-- 'MonadTelemetry'.
--
-- Le pasa el span que esté activo en el contexto thread-local, o no hace nada si no
-- hay ninguno. Es el escape hatch para los tres lugares donde no hay alternativa:
--
-- * el dominio, donde las funciones son puras y cambiar sus firmas arrastraría a
--   todos sus tests;
-- * las acciones de Beam, que corren en 'Database.Beam.Postgres.Pg' y no tienen
--   dónde colgar las instancias de Katip;
-- * las librerías que corren en 'IO' pelado, como el cliente de OpenRouter.
--
-- Sirve para spans, no para logs: lo que se le puede colgar son atributos y
-- eventos del span que ya existe. Si hace falta loguear, el dato tiene que llegar
-- hasta un borde que tenga mónada.
conSpanActivo :: (MonadIO m) => (Span -> m ()) -> m ()
conSpanActivo accion = do
  contexto <- ThreadLocal.getContext
  for_ (Context.lookupSpan contexto) accion

-- | Con qué implementar 'inSpan'' cuando la mónada tiene unlift y el
-- 'Telemetry' en el reader. El caso de 'AppRunner' y del runner de los tests.
inSpanConTracer' :: (MonadUnliftIO m, MonadReader Telemetry m) => Text -> (Span -> m a) -> m a
inSpanConTracer' name action = do
  telemetry <- ask
  Otel.inSpan' telemetry.tracer name Otel.defaultSpanArguments action

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

-- | Loguear desde un callback que una librería invoca desde su propio hilo.
--
-- Los dos casos son el @onException@ de Warp y el natural transformation con el
-- que Servant corre los handlers: los llama la librería, fuera de cualquier
-- mónada nuestra, y no hay contexto ambiente que heredar.
--
-- No es la forma normal de loguear. Para todo lo demás la mónada se establece una
-- sola vez en el entry point —@AppRunner@, @TestRunner@, 'Site.Types.AppHandler'—
-- y los logs salen de las instancias de Katip. Envolver cada log en su propio
-- @runKatipContextT@ es justamente lo que hace imposible acumular contexto.
runLogging :: Telemetry -> Katip.KatipContextT IO a -> IO a
runLogging telemetry = Katip.runKatipContextT telemetry.logEnv () mempty

-- | Azúcar para armar los atributos de un log record.
--
-- El payload de Katip es JSON, así que los atributos son pares de Aeson y lo
-- que hace falta del valor es 'Aeson.ToJSON'. El scribe los traduce a los tipos
-- de OTel del otro lado.
logAttr :: (Aeson.ToJSON a) => Text -> a -> Pair
logAttr name value = Key.fromText name .= value
