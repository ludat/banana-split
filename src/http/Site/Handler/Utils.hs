module Site.Handler.Utils (
  err200,
  err423,
  orElse,
  orElseMay,
  orElse_,
  redirect,
  runBeam,
  runBeamCon,
  throwJsonError,
) where

import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Data.Aeson
import Data.Pool qualified as Pool
import Database.Beam.Postgres qualified as Beam
import Servant

import BananaSplit.Persistence qualified as Persistence
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

-- | El default, que es el seguro: cualquier cosa que escriba tiene que ir por
-- acá. Para una lectura que no necesita tanto está 'runBeamCon'
-- 'Persistence.SoloLectura'.
runBeam :: Beam.Pg a -> AppHandler a
runBeam = runBeamCon Persistence.Serializable

runBeamCon :: Persistence.Aislamiento -> Beam.Pg a -> AppHandler a
runBeamCon aislamiento dbAction = do
  pool <- asks (.beamConnectionPool)

  liftIO $ Pool.withResource pool $ \conn -> do
    Persistence.conTransaccion aislamiento conn (Beam.runBeamPostgres conn dbAction)
