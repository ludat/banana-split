{-# LANGUAGE OverloadedStrings #-}
module Site.Handler.Utils
  ( orElse
  , orElseMay
  , orElse_
  , redirect
  , throwHtml
  , htmlLayout
  , renderHtml
  , postForm
  ) where

import Data.ByteString (ByteString)

import Servant

import Types
import Lucid
import Control.Monad.Error.Class
import Data.Text (Text)
import Site.HTML (RawHtml(..))
import Control.Monad.IO.Class
import qualified Text.Digestive as Digestive
import qualified Web.FormUrlEncoded as Form
import Data.Function
import Lucid.Htmx (hxBoost_)

orElse :: Monad m => m (Either error a) -> (error -> m a) -> m a
orElse action recovery = action >>= either recovery pure

orElse_ :: Monad m => m (Either error a) -> m a -> m a
orElse_ action recovery = orElse action (const recovery)

orElseMay :: Monad m => m (Maybe a) -> m a -> m a
orElseMay action recovery = action >>= maybe recovery pure

redirect :: ByteString -> AppHandler a
-- redirect s = throwError err302 {errHeaders = [("Location", s)]}
redirect s = throwError err200 {errHeaders = [("Hx-Location", s)]}


err200 :: ServerError
err200 = err500 {errHTTPCode = 200}

throwHtml :: (MonadError ServerError m) => HtmlT m () -> m a
throwHtml html = do
  body <- renderBST html
  throwError err200 {errBody = body}

htmlLayout :: Monad m => HtmlT m () -> HtmlT m () -> HtmlT m ()
htmlLayout navBarItems content =
  doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "UTF-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      title_ "Banana Split"
      link_ [rel_ "shortcut icon", type_ "image/png", href_ "/static/favicon.png"]

      script_ [src_ "https://unpkg.com/htmx.org@1.9.10"] $ toHtml @Text ""
      script_ [src_ "https://unpkg.com/htmx.org@1.9.10/dist/ext/debug.js"] $ toHtml @Text ""

      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/styles.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.min.css"]

    body_ [hxBoost_ "true"] $ do
      nav_ $ do
        ul_ $ do
          li_ $ strong_ $ a_ [href_ "/"] "🍌 Banana Split"
          navBarItems

      content

renderHtml :: MonadIO m => HtmlT m () -> m RawHtml
renderHtml content = do
  html <- renderBST content
  pure $ RawHtml html

postForm :: Monad m => Text -> Form.Form -> Digestive.Form v m a -> m (Digestive.View v, Maybe a)
postForm name form digestiveForm =
  Digestive.postForm name digestiveForm
    (\_e -> pure $ \p ->
      Form.lookupAll (Digestive.fromPath p) form & fmap (Digestive.TextInput) & pure)

-- | This function always overrides the error message and append
-- the content type header (to be json)
-- throwJsonError :: ServerError -> Text -> AppHandler a
-- throwJsonError serverError errorMessage =
--   throwError serverError
--     { errBody = encode $ object ["error" .= errorMessage]
--     , errHeaders = serverError.errHeaders ++ [("Content-Type", "application/json")]
--     }
