{-# LANGUAGE OverloadedStrings #-}
module Site.Handler.Utils
  ( orElse
  , orElseMay
  , orElse_
  , redirect
  , throwHtml
  ) where

import Data.ByteString (ByteString)

import Servant

import Types
import Lucid
import Control.Monad.Error.Class

orElse :: Monad m => m (Either error a) -> (error -> m a) -> m a
orElse action recovery = action >>= either recovery pure

orElse_ :: Monad m => m (Either error a) -> m a -> m a
orElse_ action recovery = orElse action (const recovery)

orElseMay :: Monad m => m (Maybe a) -> (m a) -> m a
orElseMay action recovery = action >>= maybe recovery pure

redirect :: ByteString -> AppHandler a
redirect s = throwError err307 {errHeaders = [("Location", s)]}

throwHtml :: (MonadError ServerError m) => HtmlT m () -> m a
throwHtml html = do
  let err200 = err500 { errHTTPCode = 200}
  body <- renderBST html
  throwError err200 {errBody = body}

-- | This function always overrides the error message and append
-- the content type header (to be json)
-- throwJsonError :: ServerError -> Text -> AppHandler a
-- throwJsonError serverError errorMessage =
--   throwError serverError
--     { errBody = encode $ object ["error" .= errorMessage]
--     , errHeaders = serverError.errHeaders ++ [("Content-Type", "application/json")]
--     }
