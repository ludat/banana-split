module Site.Handler.Utils (
  err200,
  err423,
  orElse,
  orElseMay,
  orElse_,
  redirect,
  throwJsonError,

  -- * Base de datos

  -- La instancia de 'Persistence.MonadPg' para 'AppHandler'. Los dos métodos se
  -- re-exportan desde acá porque es el módulo que los handlers ya importan.
  runBeamFastRead,
  runBeamWrite,
) where

import Control.Monad.Error.Class
import Data.Aeson
import Servant

import BananaSplit.Persistence (runBeamFastRead, runBeamWrite)
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
