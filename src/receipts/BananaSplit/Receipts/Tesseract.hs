{-# LANGUAGE OverloadedStrings #-}

module BananaSplit.Receipts.Tesseract
    ( extractTextFromImage
    ) where

import Control.Arrow (left)
import Control.Monad.Error.Class (liftEither)

import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Protolude

import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcess)

-- | Extract text from a base64-encoded image using Tesseract OCR
--
-- This function is not actually used since tesseract is very heavy
-- and the image detection actually works better
extractTextFromImage :: Text -> IO (Either Text Text)
extractTextFromImage base64Image = runExceptT $ do
  let imageData =  case T.breakOn "," base64Image of
          (_, withComma) -> T.drop 1 withComma

  imageBytes <- liftEither $ left show $
    Base64.decode $ TE.encodeUtf8 imageData

  result <- liftIO $ withSystemTempFile "receipt_image.png" $ \tmpImagePath tmpImageHandle -> do
    BS.hPut tmpImageHandle imageBytes
    hClose tmpImageHandle

    tesseractResult <- try @SomeException $
      readProcess "tesseract"
        [ "-c", "preserve_interword_spaces=1"
        , "--psm", "4"
        , tmpImagePath
        , "stdout"
        ] ""

    case tesseractResult of
      Left err -> pure $ Left $ "Tesseract failed: " <> show err
      Right text -> pure $ Right $ T.strip $ toS text

  liftEither result
