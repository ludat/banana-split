
module Site.HTML
    ( HTML
    , RawHtml (..)
    ) where

import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty qualified as NE
import Data.Typeable (Typeable)

import Network.HTTP.Media qualified as M

import Servant.API (Accept (..), MimeRender (..))

data HTML deriving Typeable

newtype RawHtml = RawHtml LBS.ByteString

-- | @text/html;charset=utf-8@
instance Accept HTML where
  contentTypes _ =
    "text" M.// "html" M./: ("charset", "utf-8") NE.:|
    ["text" M.// "html"]

instance MimeRender HTML RawHtml where
  mimeRender _ (RawHtml bs) = bs
