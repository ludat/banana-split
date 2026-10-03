module Site.Handler.Utils (
  err200,
  err423,
  inSpan,
  inSpan',
  logDebug,
  logInfo,
  logWarn,
  orElse,
  orElseMay,
  orElse_,
  redirect,
  runBeamFastRead,
  runBeamWrite,
  throwJsonError,
) where

import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.Pool qualified as Pool
import Database.Beam.Postgres qualified as Beam
import OpenTelemetry.Log (AnyValue)
import OpenTelemetry.Trace.Core (Span, SpanArguments (..), SpanKind (..))
import OpenTelemetry.Trace.Core qualified as Otel
import Servant

import BananaSplit.Persistence qualified as Persistence
-- Los campos, no solo el tipo: OverloadedRecordDot necesita el selector en
-- scope acá para generar el HasField de @app.telemetry.tracer@.
import BananaSplit.Telemetry (Telemetry (..))
import BananaSplit.Telemetry qualified as Telemetry
import Preludat
import Site.Types

redirect :: ByteString -> AppHandler a
redirect s = throwError err302{errHeaders = [("Location", s)]}

err200 :: ServerError
err200 =
  ServerError
    { errHTTPCode = 200
    , errReasonPhrase = "OK"
    , errBody = ""
    , errHeaders = []
    }

err423 :: ServerError
err423 =
  ServerError
    { errHTTPCode = 423
    , errReasonPhrase = "Locked"
    , errBody = ""
    , errHeaders = []
    }

-- | This function always overrides the error message and append
-- the content type header (to be json)
throwJsonError :: ServerError -> Text -> AppHandler a
throwJsonError serverError errorMessage =
  throwError
    serverError
      { errBody = encode $ object ["error" .= errorMessage]
      , errHeaders = errHeaders serverError ++ [("Content-Type", "application/json")]
      }

-- | Para cualquier handler que escriba. Ver 'Persistence.conTransaccionDeEscritura'.
runBeamWrite :: Beam.Pg a -> AppHandler a
runBeamWrite dbAction = do
  app <- ask

  liftIO $ Otel.inSpan app.telemetry.tracer "db.write" dbSpanArguments $ do
    Pool.withResource app.beamConnectionPool $ \conn -> do
      Persistence.conTransaccionDeEscritura conn dbAction

-- | Para un handler que sólo lee y muestra. Es más barato y no le estorba a las
-- escrituras en paralelo, a cambio de no poder decidir con lo leído algo que
-- después se vaya a escribir. Ante la duda, 'runBeamWrite'.
runBeamFastRead :: Beam.Pg a -> AppHandler a
runBeamFastRead dbAction = do
  app <- ask

  liftIO $ Otel.inSpan app.telemetry.tracer "db.read" dbSpanArguments $ do
    Pool.withResource app.beamConnectionPool $ \conn -> do
      Persistence.conTransaccionDeLecturaRapida conn dbAction

-- | El span incluye la espera por una conexión del pool, que es justo lo que
-- querés ver cuando un endpoint tarda y las queries son rápidas.
dbSpanArguments :: SpanArguments
dbSpanArguments = Otel.defaultSpanArguments{kind = Client}

-- | Un span alrededor de un pedazo de handler. Lo que hace falta hacer a mano
-- es que un 'ServerError' lanzado adentro no se escape del span sin cerrarlo:
-- 'AppHandler' no es 'MonadUnliftIO' (abajo tiene un @ExceptT@), así que
-- 'Otel.inSpan' no se le puede aplicar derecho.
--
-- Solo los 5xx marcan el span como error, por lo mismo que en
-- 'Site.Telemetry.traceApiMiddleware': un 401 o un 409 es una respuesta, no una
-- falla. El código igual queda en un atributo, así que se puede filtrar.
inSpan :: Text -> AppHandler a -> AppHandler a
inSpan name action = inSpan' name (const action)

-- | Como 'inSpan', pero te da el span para colgarle atributos.
inSpan' :: Text -> (Span -> AppHandler a) -> AppHandler a
inSpan' name action = do
  app <- ask
  outcome <- liftIO $ Otel.inSpan' app.telemetry.tracer name Otel.defaultSpanArguments $ \handlerSpan -> do
    result <- runHandler $ runReaderT (action handlerSpan) app
    case result of
      Right _ -> pure ()
      Left err -> do
        Otel.addAttribute handlerSpan "http.response.status_code"
          $ (fromIntegral (errHTTPCode err) :: Int64)
        when (errHTTPCode err >= 500)
          $ Otel.setStatus handlerSpan (Otel.Error $ "HTTP " <> show (errHTTPCode err))
    pure result
  either throwError pure outcome

-- | Emite un log record por OTLP, correlacionado con el trace: lleva el
-- contexto activo, así que Grafana lo cruza con el span del request (ya está
-- configurado en el datasource de Loki) y se ve el /por qué/ al lado del resto
-- del trace en lugar de suelto en stdout.
--
-- El cuerpo del record es el nombre del evento, nunca prosa: el detalle va en
-- los atributos. Es lo mismo que hace el frontend, y por la misma razón — un
-- nombre estable es lo que se puede consultar.
logEvent ::
  (Telemetry -> Text -> [(Text, AnyValue)] -> IO ())
  -> Text
  -> [(Text, AnyValue)]
  -> AppHandler ()
logEvent emit name attributes = do
  app <- ask
  liftIO $ emit app.telemetry name attributes

-- | Algo pasó y salió como se esperaba.
logInfo :: Text -> [(Text, AnyValue)] -> AppHandler ()
logInfo = logEvent Telemetry.logInfo

-- | Algo salió mal pero es una respuesta válida del sistema: un código
-- equivocado, un rate limit, un mail que no se pudo atribuir a nadie.
logWarn :: Text -> [(Text, AnyValue)] -> AppHandler ()
logWarn = logEvent Telemetry.logWarn

-- | Para lo que pasa seguido y solo importa cuando estás mirando de cerca.
logDebug :: Text -> [(Text, AnyValue)] -> AppHandler ()
logDebug = logEvent Telemetry.logDebug
