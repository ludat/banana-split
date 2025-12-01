{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Site.OpenRouter
    ( OpenRouterConfig (..)
    , ParsedReceipt (..)
    , ParsedReceiptItem
    , analyzeReceiptImage
    ) where


import Control.Arrow (left)
import Control.Monad.Error.Class

import Data.Aeson
import Data.String.Interpolate (__i)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE

import Network.HTTP.Client (Manager)
import Network.HTTP.Req

import Protolude

import Site.Api (ParsedReceiptItem)


data OpenRouterConfig = OpenRouterConfig
  { apiKey :: Text
  , httpManager :: Manager
  , models :: [Text]
  }

newtype ParsedReceipt = ParsedReceipt
  { items :: [ParsedReceiptItem]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data OpenRouterRequest = OpenRouterRequest
  { model :: Text
  , messages :: [OpenRouterMessage]
  , fallbackModels :: [Text]
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data OpenRouterMessage = OpenRouterMessage
  { role :: Text
  , content :: [MessageContent]
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON)

data MessageContent
  = TextContent Text
  | ImageContent ImageUrl
  deriving (Show, Generic)

instance ToJSON MessageContent where
  toJSON (TextContent txt) = object
    [ "type" .= ("text" :: Text)
    , "text" .= txt
    ]
  toJSON (ImageContent imgUrl) = object
    [ "type" .= ("image_url" :: Text)
    , "image_url" .= imgUrl
    ]

newtype ImageUrl = ImageUrl
  { url :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)


newtype OpenRouterResponse = OpenRouterResponse
  { choices :: [Choice]
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

newtype Choice = Choice
  { message :: ResponseMessage
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

newtype ResponseMessage = ResponseMessage
  { content :: Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

analyzeReceiptImage :: OpenRouterConfig -> Text -> IO (Either Text ParsedReceipt)
analyzeReceiptImage config base64Image = runExceptT $ do
  (model, fallbackModels) <- case config.models of
    [] -> throwError "Reading recepts is not properly configured. Talk to an admin."
    (model:fallback) -> pure (model, fallback)

  let prompt = [__i|
        You are a receipt parser. Analyze this receipt image and extract the items in JSON format.
        Return EXACTLY this JSON structure with the extracted data:"
        {
          "items": [
            {
              "nombre": "item name",
              "monto": {"lugaresDespuesDeLaComa": 2, "valor": 1050},
              "cantidad": 1
            }
          ]
        }

        CRITICAL INSTRUCTIONS:
        - For monto, valor is the amount in CENTS (multiply by 100). Example: $10.50 becomes 1050
        - lugaresDespuesDeLaComa must always be 2 for currency
        - cantidad is the quantity of each item (default to 1 if not specified)
        - Extract ALL items from the receipt
        - Return ONLY the raw JSON object starting with { and ending with }
        - DO NOT wrap the JSON in markdown code blocks
        - DO NOT include ```json or ``` markers
        - DO NOT add any explanatory text before or after the JSON
        - Your entire response must be valid, parseable JSON
        - If you encounter a problem with the parsing just return a plain text BRIEF explanation for the enduser without json at all
        - The error text MUST be in spanish always
        |]

  let imageUrl = "data:" <> base64Image
  let requestBody = OpenRouterRequest
        { model = model
        , fallbackModels = fallbackModels
        , messages =
            [ OpenRouterMessage
                { role = "user"
                , content =
                    [ TextContent prompt
                    , ImageContent (ImageUrl imageUrl)
                    ]
                }
            ]
        }
  putText "starting request"
  response <- req
    POST
    (https "openrouter.ai" /: "api" /: "v1" /: "chat" /: "completions")
    (ReqBodyJson requestBody)
    bsResponse
    (mconcat
      [ header "Authorization" ("Bearer " <> TE.encodeUtf8 config.apiKey)
      , header "HTTP-Referer" "https://split.ludat.io"
      , header "X-Title" "Banana Split Receipt Parser"
      ]
    )

  putByteString $ responseBody response
  openRouterResp <- liftEither $ left Text.pack $ eitherDecodeStrict @OpenRouterResponse $ responseBody response

  Choice (ResponseMessage contentText) <- case openRouterResp.choices of
    [] -> throwError "No response from OpenRouter"
    (c:_) -> pure c

  let cleanedText = contentText
        & T.dropWhile (/= '{')
        & T.dropWhileEnd (/= '}')

  liftEither $ left Text.pack $ eitherDecodeStrict $ TE.encodeUtf8 cleanedText
