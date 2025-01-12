{-# LANGUAGE OverloadedStrings #-}
module Site.Handler.Utils
    ( orElse
    , orElseMay
    , orElse_
    , redirect
    , runSelda
    , throwJsonError
    ) where

import Control.Monad.Error.Class
import Control.Monad.Reader.Class

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Pool qualified as Pool

import Database.Selda
import Database.Selda.Backend (runSeldaT)
import Database.Selda.PostgreSQL (PG)

import Servant

import Types


orElse :: Monad m => m (Either error a) -> (error -> m a) -> m a
orElse action recovery = action >>= either recovery pure

orElse_ :: Monad m => m (Either error a) -> m a -> m a
orElse_ action recovery = orElse action (const recovery)

orElseMay :: Monad m => m (Maybe a) -> m a -> m a
orElseMay action recovery = action >>= maybe recovery pure

redirect :: ByteString -> AppHandler a
redirect s = throwError err302 {errHeaders = [("Location", s)]}
-- redirect s = throwError err200 {errHeaders = [("Hx-Location", s)]}


err200 :: ServerError
err200 = ServerError { errHTTPCode = 200
                     , errReasonPhrase = "OK"
                     , errBody = ""
                     , errHeaders = []
                     }

-- | This function always overrides the error message and append
-- the content type header (to be json)
throwJsonError :: ServerError -> Text -> AppHandler a
throwJsonError serverError errorMessage =
  throwError serverError
    { errBody = encode $ object ["error" .= errorMessage]
    , errHeaders = errHeaders serverError ++ [("Content-Type", "application/json")]
    }

runSelda :: SeldaT PG IO a -> AppHandler a
runSelda dbAction = do
  pool <- asks (.connection)

  liftIO $ Pool.withResource pool $ \seldaConn -> do
    runSeldaT dbAction seldaConn
