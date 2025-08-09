{-# OPTIONS_GHC -Wno-orphans #-}
module BananaSplit.ULID
    ( module Data.ULID
    , nullUlid
    ) where

import Control.Monad

import Data.Aeson
import Data.Aeson.Types
import Data.ULID

import Elm.TyRep (EPrimAlias (..), ETCon (..), EType (..), ETypeDef (..), ETypeName (..),
                  IsElmDefinition (..))

import Protolude
import Protolude.Error

import Servant.API

instance ToJSON ULID where
  toJSON :: ULID -> Value
  toJSON = String . show

instance FromJSON ULID where
  parseJSON :: Value -> Parser ULID
  parseJSON = withText "ULID" $ \t ->
    case readMaybe @ULID t of
      Just ulid -> pure ulid
      Nothing -> fail "Invalid ulid"

instance ToJSONKey ULID

instance FromJSONKey ULID

instance ToHttpApiData ULID where
  toQueryParam :: ULID -> Text
  toQueryParam = show

instance FromHttpApiData ULID where
  parseUrlPiece :: Text -> Either Text ULID
  parseUrlPiece t = do
    case readMaybe @ULID t of
      Just ulid -> pure ulid
      Nothing -> Left $ "cant parse a ulid from: " <> show t <> ""


instance IsElmDefinition ULID where
  compileElmDef :: Proxy ULID -> ETypeDef
  compileElmDef _ =
    ETypePrimAlias (EPrimAlias {epa_name = ETypeName {et_name = "ULID", et_args = []}, epa_type = ETyCon (ETCon {tc_name = "String"})})

nullUlid :: ULID
nullUlid = fromRight (error "impossible") $ ulidFromInteger 0
