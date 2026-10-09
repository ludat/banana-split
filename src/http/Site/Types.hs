{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Site.Types (
  App (..),
  AppHandler (..),
) where

import Control.Monad.Reader
import Crypto.JWT (JWK)
import Data.Int (Int64)
import Data.Pool
import Database.Beam.Postgres qualified as Beam
import Katip qualified
import OpenTelemetry.Trace.Core qualified as Otel
import Servant

import BananaSplit.Receipts
import BananaSplit.Telemetry (Telemetry (..))
import BananaSplit.Telemetry qualified as Telemetry
import Preludat
import Site.Mailer (Mailer)

data App = App
  { beamConnectionPool :: Pool Beam.Connection
  , receipts :: ReceiptsReaderConfig
  , jwk :: JWK
  -- ^ Symmetric key used to sign and verify session JWTs.
  , authPepper :: ByteString
  -- ^ Server secret peppering the login-code commitment (see "Site.Auth").
  , cookieSecure :: Bool
  -- ^ Whether session cookies are marked @Secure@ (HTTPS only). Off in dev.
  , mailer :: Mailer
  -- ^ Delivers login confirmation codes (console in dev, email in prod).
  , telemetry :: Telemetry
  , logContexts :: Katip.LogContexts
  , logNamespace :: Katip.Namespace
  }

-- | La mónada de los handlers.
--
-- Es un newtype y no un sinónimo de @ReaderT App Servant.Handler@ por las
-- instancias de Katip de abajo: con el sinónimo chocaban con la genérica que trae
-- Katip (@instance Katip m => Katip (ReaderT s m)@) y había que resolverlas con
-- @OVERLAPPING@. Un newtype no es un @ReaderT@, así que el solapamiento no existe.
--
-- Lo que el @ReaderT@ daba gratis se deriva: si algún handler necesita una clase
-- que no esté en la lista, GHC lo dice y se agrega ahí.
newtype AppHandler a = AppHandler {runAppHandler :: ReaderT App Servant.Handler a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader App
    , MonadError ServerError
    )

instance Katip.Katip AppHandler where
  getLogEnv = asks (.telemetry.logEnv)
  localLogEnv f =
    local $ \app -> app{telemetry = app.telemetry{Telemetry.logEnv = f app.telemetry.logEnv}}

instance Katip.KatipContext AppHandler where
  getKatipContext = asks (.logContexts)
  localKatipContext f = local $ \app -> app{logContexts = f app.logContexts}
  getKatipNamespace = asks (.logNamespace)
  localKatipNamespace f = local $ \app -> app{logNamespace = f app.logNamespace}

-- | Un span alrededor de un pedazo de handler. Lo que hace falta hacer a mano
-- es que un 'ServerError' lanzado adentro no se escape del span sin cerrarlo:
-- 'AppHandler' no es 'MonadUnliftIO' (abajo tiene un @ExceptT@), así que
-- 'Otel.inSpan' no se le puede aplicar derecho.
--
-- Solo los 5xx marcan el span como error, por lo mismo que en
-- 'Site.Telemetry.traceApiMiddleware': un 401 o un 409 es una respuesta, no una
-- falla. El código igual queda en un atributo, así que se puede filtrar.
instance Telemetry.MonadTelemetry AppHandler where
  inSpan' name action = do
    app <- ask
    outcome <- liftIO $ Otel.inSpan' app.telemetry.tracer name Otel.defaultSpanArguments $ \handlerSpan -> do
      result <- runHandler $ runReaderT (runAppHandler (action handlerSpan)) app
      case result of
        Right _ -> pure ()
        Left err -> do
          Otel.addAttribute handlerSpan "http.response.status_code"
            $ (fromIntegral (errHTTPCode err) :: Int64)
          when (errHTTPCode err >= 500)
            $ Otel.setStatus handlerSpan (Otel.Error $ "HTTP " <> show (errHTTPCode err))
      pure result
    either throwError pure outcome
