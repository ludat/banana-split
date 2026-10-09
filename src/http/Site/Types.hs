module Site.Types (
  App (..),
  AppHandler,
) where

import Control.Monad.Reader
import Crypto.JWT (JWK)
import Data.Pool
import Database.Beam.Postgres qualified as Beam
import Katip qualified
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

type AppHandler = ReaderT App Servant.Handler

instance {-# OVERLAPPING #-} Katip.Katip AppHandler where
  getLogEnv = asks (.telemetry.logEnv)
  localLogEnv f =
    local $ \app -> app{telemetry = app.telemetry{Telemetry.logEnv = f app.telemetry.logEnv}}

instance {-# OVERLAPPING #-} Katip.KatipContext AppHandler where
  getKatipContext = asks (.logContexts)
  localKatipContext f = local $ \app -> app{logContexts = f app.logContexts}
  getKatipNamespace = asks (.logNamespace)
  localKatipNamespace f = local $ \app -> app{logNamespace = f app.logNamespace}
