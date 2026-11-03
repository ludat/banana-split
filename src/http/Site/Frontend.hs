{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

module Site.Frontend (
  FrontendShell,
  InitialData (..),
  initialDataScript,
  initialDataScriptPath,
  loadFrontendShell,
  parseFrontendShell,
  renderFrontendShell,
) where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy qualified as BSL
import Text.XmlHtml qualified as Html

import BananaSplit
import Preludat

-- | The frontend's @index.html@ (what elm-land builds), parsed once at startup.
-- Only built through 'parseFrontendShell', which guarantees it has a @<head>@
-- for 'renderFrontendShell' to put the 'InitialData' in.
newtype FrontendShell = FrontendShell Html.Document

data InitialData = InitialData
  { user :: Maybe User
  }
  deriving (Show, Eq, Generic)
  deriving anyclass (ToJSON)

loadFrontendShell :: FilePath -> IO (Either Text FrontendShell)
loadFrontendShell path = do
  contents <- try (BS.readFile path)
  pure $ case contents of
    Left (e :: IOException) -> Left (show e)
    Right html -> parseFrontendShell html

parseFrontendShell :: ByteString -> Either Text FrontendShell
parseFrontendShell html = do
  document <- first toS (Html.parseHTML "index.html" html)
  when (isNothing (traverseHead Just document)) $ Left "index.html has no <head>"
  pure (FrontendShell document)

-- | Where the elm-land dev server gets the 'InitialData', since the backend
-- never sees its HTML: @ui/elm-land.json@ puts a @<script src>@ to it in the
-- head. The built shell has the data inline instead, so that tag is dropped.
initialDataScriptPath :: Text
initialDataScriptPath = "/api/initial-data.js"

-- | The shell with the 'InitialData' as JSON in a @<meta>@ at the end of
-- @<head>@, in place of the dev server's 'initialDataScriptPath'. An attribute
-- and not a @<script>@ because HTML has no escaping inside a script (it ends at
-- the first @</script@, whatever the case), while an attribute value is escaped
-- by the renderer and decoded back by the browser, so the JSON arrives intact.
renderFrontendShell :: FrontendShell -> InitialData -> BSL.ByteString
renderFrontendShell (FrontendShell document) initialData =
  document
    & withHead (\children -> filter (not . isDevScript) children <> [inlineData])
    & Html.render
    & toLazyByteString
 where
  isDevScript node =
    Html.tagName node == Just "script" && Html.getAttribute "src" node == Just initialDataScriptPath
  inlineData =
    Html.Element
      { elementTag = "meta"
      , elementAttrs =
          [ ("name", "initial-data")
          , ("content", decodeUtf8 (BSL.toStrict (Aeson.encode initialData)))
          ]
      , elementChildren = []
      }
  withHead f = fromMaybe document . traverseHead (Just . f)

-- | What 'initialDataScriptPath' answers: the same data, as a script that
-- leaves it in @window.initialData@. Being a file of its own, there is no HTML
-- around it to escape from.
initialDataScript :: InitialData -> BSL.ByteString
initialDataScript initialData = "window.initialData = " <> Aeson.encode initialData <> ";\n"

-- | Change the children of @<html><head>@; 'Nothing' if the document has none.
traverseHead :: ([Html.Node] -> Maybe [Html.Node]) -> Html.Document -> Maybe Html.Document
traverseHead f document = do
  content <- inChild "html" (inChild "head" f) (Html.docContent document)
  pure document{Html.docContent = content}
 where
  -- Apply @g@ to the children of the first element called @tag@.
  inChild tag g = \case
    [] -> Nothing
    node@Html.Element{} : rest
      | Html.elementTag node == tag -> do
          children <- g (Html.elementChildren node)
          pure (node{Html.elementChildren = children} : rest)
    node : rest -> (node :) <$> inChild tag g rest
