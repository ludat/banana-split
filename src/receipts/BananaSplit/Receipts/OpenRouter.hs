{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module BananaSplit.Receipts.OpenRouter
    ( ParsedReceipt (..)
    , ParsedReceiptItem (..)
    , analyzeReceiptImage
    , analyzeReceiptText
    ) where


import BananaSplit.Receipts.Tesseract (extractTextFromImage)
import BananaSplit.Receipts.Types

import Control.Arrow (left)
import Control.Monad.Error.Class

import Data.Aeson
import Data.Scientific (Scientific)
import Data.String.Interpolate (__i)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

import Network.HTTP.Req

import Protolude

newtype ParsedReceipt = ParsedReceipt
  { items :: [ParsedReceiptItem]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ParsedReceiptItem = ParsedReceiptItem
  { nombre :: Text
  , monto :: Scientific
  , cantidad :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data OpenRouterRequest = OpenRouterRequest
  { model :: Text
  , messages :: [OpenRouterMessage]
  , models :: [Text]
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

analyzeReceiptImage :: ReceiptsReaderConfig -> Text -> IO (Either Text ParsedReceipt)
analyzeReceiptImage config base64Image =
  analyzeReceiptFromMessage config $ ImageContent (ImageUrl $ "data:" <> base64Image)

analyzeReceiptText :: ReceiptsReaderConfig -> Text -> IO (Either Text ParsedReceipt)
analyzeReceiptText config base64Image = runExceptT $ do
  ocrText <- ExceptT $ liftIO $ extractTextFromImage base64Image

  ExceptT $ analyzeReceiptFromMessage config $ TextContent ocrText

analyzeReceiptFromMessage :: ReceiptsReaderConfig -> MessageContent -> IO (Either Text ParsedReceipt)
analyzeReceiptFromMessage config msg = runReq defaultHttpConfig {httpConfigCheckResponse = \_ _ _ -> Nothing} $ runExceptT $ do
  (model, fallbackModels) <- case config.models of
    [] -> throwError "Reading receipts is not properly configured. Talk to an admin."
    (model:fallback) -> pure (model, fallback)
  let systemPrompt = [__i|
        You are a receipt parser. Analyze receipt text extracted via OCR and extract the items in JSON format.
        Return EXACTLY this JSON structure with the extracted data:

        #{encode $ ParsedReceipt [ParsedReceiptItem "food" 10.50 1]}

        CRITICAL INSTRUCTIONS:
        - Sometimes the reciept mentions both total price and unitary price for an item, make sure to always pick the total
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

  let requestBody = OpenRouterRequest
        { model = model
        , models = fallbackModels
        , messages =
            [ OpenRouterMessage
                { role = "system"
                , content = [TextContent systemPrompt]
                }
            , OpenRouterMessage
                { role = "user"
                , content = [msg]
                }
            ]
        }
  putText $ "starting request with model: " <> show config.models
  response <- req
    POST
    (https "openrouter.ai" /: "api" /: "v1" /: "chat" /: "completions")
    (ReqBodyJson requestBody)
    bsResponse
    (mconcat
      [ header "Authorization" ("Bearer " <> Text.encodeUtf8 config.apiKey)
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
        & Text.dropWhile (/= '{')
        & Text.dropWhileEnd (/= '}')

  liftEither $ left Text.pack $ eitherDecodeStrict $ Text.encodeUtf8 cleanedText
