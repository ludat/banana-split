{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Site.Types (
  App (..),
  AppHandler (..),
) where

import Control.Monad.Reader
import Crypto.JWT (JWK)
import Data.Pool
import Database.Beam.Postgres qualified as Beam
import Katip qualified
import OpenTelemetry.Trace.Core qualified as Otel
import Servant

import BananaSplit.Persistence qualified as Persistence
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
  -- ^ Los providers y el estado de logging de Katip, todo junto: es lo que hace
  -- que las instancias de abajo sean cirugía sobre un solo campo, y lo único que
  -- hay que pasarle a 'Persistence.conTransaccionDeEscritura'.
  }

newtype AppHandler a = AppHandler {runAppHandler :: ReaderT App Servant.Handler a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader App
    , MonadError ServerError
    )

-- | Las de Katip van a mano y no por @deriving via@ 'Telemetry.ConTelemetria'
-- porque el reader de 'AppHandler' no es el 'Telemetry' sino el 'App' entero.
-- Siguen siendo una línea cada una: la cirugía la hacen los @sobre*@.
instance Katip.Katip AppHandler where
  getLogEnv = asks (.telemetry.logEnv)
  localLogEnv f = sobreTelemetry (Telemetry.sobreLogEnv f)

instance Katip.KatipContext AppHandler where
  getKatipContext = asks (.telemetry.logContexts)
  localKatipContext f = sobreTelemetry (Telemetry.sobreLogContexts f)
  getKatipNamespace = asks (.telemetry.logNamespace)
  localKatipNamespace f = sobreTelemetry (Telemetry.sobreLogNamespace f)

sobreTelemetry :: (Telemetry -> Telemetry) -> AppHandler a -> AppHandler a
sobreTelemetry f = local $ \app -> app{telemetry = f app.telemetry}

instance Persistence.MonadPg AppHandler where
  runBeamWrite dbAction = conUnaConexionDelPool $ \telemetry conn ->
    Persistence.conTransaccionDeEscritura telemetry conn dbAction

  runBeamFastRead dbAction = conUnaConexionDelPool $ \telemetry conn ->
    Persistence.conTransaccionDeLecturaRapida telemetry conn dbAction

conUnaConexionDelPool :: (Telemetry -> Beam.Connection -> IO a) -> AppHandler a
conUnaConexionDelPool accion = do
  app <- ask
  liftIO
    $ Persistence.conConexionDelPool app.telemetry app.beamConnectionPool
    $ accion app.telemetry

instance Telemetry.MonadTracer AppHandler where
  getTracer = asks (.telemetry.tracer)

instance Telemetry.MonadTelemetry AppHandler where
  inSpan' name action = do
    app <- ask
    outcome <-
      liftIO
        $ Otel.inSpan' app.telemetry.tracer name Otel.defaultSpanArguments
        $ \span -> runHandler $ runReaderT (runAppHandler (action span)) app
    either throwError pure outcome
