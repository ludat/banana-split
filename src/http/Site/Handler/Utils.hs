{-# LANGUAGE OverloadedStrings #-}
module Site.Handler.Utils
    ( err200
    , orElse
    , orElseMay
    , orElse_
    , redirect
    , runBeam
    , throwJsonError
    ) where

import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Reader.Class

import Data.Aeson
import Data.Pool qualified as Pool

import Database.Beam.Postgres qualified as Beam
import Database.PostgreSQL.Simple qualified as Simple

import Preludat

import Servant

import Site.Types

redirect :: ByteString -> AppHandler a
redirect s = throwError err302 {errHeaders = [("Location", s)]}

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

runBeam :: Beam.Pg a -> AppHandler a
runBeam dbAction = do
  pool <- asks (.beamConnectionPool)

  liftIO $ Pool.withResource pool $ \conn -> do
    Simple.withTransaction conn (Beam.runBeamPostgres conn dbAction)
