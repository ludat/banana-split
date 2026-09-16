module Site.Handler.Utils (
  err200,
  err423,
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

-- | Para cualquier handler que escriba. Ver 'Persistence.conTransaccionDeEscritura'.
runBeamWrite :: Beam.Pg a -> AppHandler a
runBeamWrite dbAction = do
  pool <- asks (.beamConnectionPool)

  liftIO $ Pool.withResource pool $ \conn -> do
    Persistence.conTransaccionDeEscritura conn dbAction

-- | Para un handler que sólo lee y muestra. Es más barato y no le estorba a las
-- escrituras en paralelo, a cambio de no poder decidir con lo leído algo que
-- después se vaya a escribir. Ante la duda, 'runBeamWrite'.
runBeamFastRead :: Beam.Pg a -> AppHandler a
runBeamFastRead dbAction = do
  pool <- asks (.beamConnectionPool)

  liftIO $ Pool.withResource pool $ \conn -> do
    Persistence.conTransaccionDeLecturaRapida conn dbAction
