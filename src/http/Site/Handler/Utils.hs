module Site.Handler.Utils (
  err200,
  err423,
  onForeignKeyViolation,
  orElse,
  orElseMay,
  orElse_,
  redirect,
  throwJsonError,

  -- * Base de datos
  runBeamFastRead,
  runBeamWrite,
) where

import Control.Monad.Error.Class
import Data.Aeson
import Database.PostgreSQL.Simple (SqlError (..))
import Servant

import BananaSplit.Persistence (runBeamFastRead, runBeamWrite)
import Preludat
import Site.Types

redirect :: ByteString -> AppHandler a
redirect s = throwError err302{errHeaders = [("Location", s)]}

-- | Corre la acción y, si la base la rechaza por una foreign key (por ejemplo,
-- se quiso borrar una fila que alguien todavía referencia), corre la segunda en
-- su lugar. La transacción ya hizo rollback cuando la excepción llega acá.
--
-- 'AppHandler' no tiene 'MonadCatch' (el 'ExceptT' de Servant), así que se baja
-- a 'IO' para atraparla, igual que en su 'inSpan''.
onForeignKeyViolation :: AppHandler a -> AppHandler a -> AppHandler a
onForeignKeyViolation action onViolation = do
  app <- ask
  outcome <- liftIO $ try @SqlError $ runHandler $ runReaderT (runAppHandler action) app
  case outcome of
    Left sqlError
      | sqlError.sqlState == "23503" -> onViolation
      | otherwise -> liftIO $ throwIO sqlError
    Right result -> either throwError pure result

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
