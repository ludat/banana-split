
module Site.HTML (HTML, RawHtml(RawHtml)) where

import Data.List.NonEmpty qualified as NE
import Data.Typeable (Typeable)
import Lucid (ToHtml (..), renderBS)
import Network.HTTP.Media qualified as M
import Servant.API (Accept (..), MimeRender (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

-- import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS

data HTML deriving Typeable

newtype RawHtml = RawHtml LBS.ByteString

-- | @text/html;charset=utf-8@
instance Accept HTML where
  contentTypes _ =
    "text" M.// "html" M./: ("charset", "utf-8") NE.:|
    ["text" M.// "html"]

instance MimeRender HTML RawHtml where
  mimeRender _ (RawHtml bs) = bs