{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BananaSplit.Email (
  Email,
  mkEmail,
  unEmail,
) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Elm.Derive qualified as Elm
import Protolude

newtype Email = Email Text
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord)

mkEmail :: Text -> Email
mkEmail = Email . Text.toLower . Text.strip

unEmail :: Email -> Text
unEmail (Email e) = e

instance IsString Email where
  fromString = mkEmail . toS

instance ToJSON Email where
  toJSON :: Email -> Value
  toJSON = String . unEmail

instance FromJSON Email where
  parseJSON :: Value -> Parser Email
  parseJSON = withText "Email" (pure . mkEmail)

Elm.deriveElmDef Elm.defaultOptions ''Email
