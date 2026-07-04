module Site.Types (
  App (..),
  AppHandler,
) where

import Control.Monad.Reader
import Crypto.JWT (JWK)
import Data.Pool
import Database.Beam.Postgres qualified as Beam
import Servant

import BananaSplit.Receipts
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
  }

type AppHandler = ReaderT App Servant.Handler
