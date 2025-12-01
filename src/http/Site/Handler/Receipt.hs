{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedStrings #-}

module Site.Handler.Receipt
    ( handleReceiptImageParse
    ) where

import Control.Monad.Reader (asks)

import Protolude

import Site.Api (ParsedReceiptItem (..), ReceiptImageRequest (..), ReceiptImageResponse (..))
import Site.OpenRouter (OpenRouterConfig (..), ParsedReceipt (..), analyzeReceiptImage)
import Site.Types

handleReceiptImageParse :: ReceiptImageRequest -> AppHandler ReceiptImageResponse
handleReceiptImageParse req = do
  apiKey <- asks (.openRouterApiKey)
  models <- asks (.openRouterModels)
  manager <- asks (.httpManager)

  let config = OpenRouterConfig
        { apiKey = apiKey
        , httpManager = manager
        , models = models
        }

  result <- liftIO $ analyzeReceiptImage config req.imageBase64

  case result of
    Left err -> pure $ ReceiptImageError { error = err }
    Right parsedReceipt -> pure $ ReceiptImageSuccess { items = parsedReceipt.items }
