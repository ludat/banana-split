module Site.FrontendSpec (
  spec,
) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Text qualified as Text
import Protolude
import Test.Hspec
import Text.XmlHtml qualified as Html

import BananaSplit (User (..), nullUlid)
import Site.Frontend (InitialData (..), parseFrontendShell, renderFrontendShell)

-- | The shape of what elm-land builds.
template :: ByteString
template =
  "<!DOCTYPE html>\n<html lang=\"en\"><head><title>Banana split</title>\
  \<meta charset=\"UTF-8\">\
  \<script src=\"/api/initial-data.js\" defer></script>\
  \<script type=\"module\" crossorigin src=\"/assets/index.js\"></script>\
  \</head><body></body></html>"

render :: InitialData -> ByteString
render initialData = case parseFrontendShell template of
  Left reason -> panic reason
  Right shell -> BSL.toStrict $ renderFrontendShell shell initialData

-- | What a browser would read back: the @content@ of the injected @<meta>@.
readBack :: ByteString -> Maybe Text
readBack html = do
  document <- rightToMaybe (Html.parseHTML "index.html" html)
  meta <-
    find
      (\n -> Html.getAttribute "name" n == Just "initial-data")
      (concatMap (Html.descendantElementsTag "meta") (Html.docContent document))
  Html.getAttribute "content" meta

spec :: Spec
spec = describe "renderFrontendShell" $ do
  it "puts the data in a <meta> at the end of <head>" $ do
    decodeUtf8 (render InitialData{user = Nothing})
      `shouldSatisfy` Text.isInfixOf "<meta name='initial-data' content='{\"user\":null}' /></head>"

  it "drops the dev server's script, the data is already inline" $ do
    decodeUtf8 (render InitialData{user = Nothing})
      `shouldNotSatisfy` Text.isInfixOf "initial-data.js"

  it "keeps the rest of the document as HTML, not XML" $ do
    let html = decodeUtf8 (render InitialData{user = Nothing})
    html `shouldSatisfy` Text.isPrefixOf "<!DOCTYPE html>"
    -- An empty <script /> would swallow the rest of the page.
    html `shouldSatisfy` Text.isInfixOf "src='/assets/index.js'></script>"

  it "gets any name back intact, whatever it has in it" $ do
    let nombre = "O'Brien \"el\" </SCRIPT><!--<script> &amp; '>"
        initialData = InitialData{user = Just User{id = nullUlid, email = "a@example.com", nombre = nombre, datosDeCobro = Nothing}}
    readBack (render initialData)
      `shouldBe` Just (decodeUtf8 (BSL.toStrict (Aeson.encode initialData)))

  it "rejects a document without <head>" $ do
    isLeft (parseFrontendShell "<html><body></body></html>") `shouldBe` True
