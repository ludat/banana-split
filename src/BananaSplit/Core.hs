{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module BananaSplit.Core where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Function ((&))
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Proxy (Proxy)
import Data.Text
import Data.ULID

import Elm.Derive qualified as Elm
import Elm.TyRep (EPrimAlias (..), ETCon (..), EType (..), ETypeDef (..), ETypeName (..),
                  IsElmDefinition (..))

import GHC.Generics (Generic)

import Money qualified
import Money.Aeson ()

import Servant (FromHttpApiData (..), ToHttpApiData)
import Servant.API (ToHttpApiData (..))

import Text.Read (readMaybe)

data Grupo = Grupo
  { grupoId :: ULID
  , grupoNombre :: Text
  , pagos :: [Pago]
  , participantes :: [Participante]
  } deriving (Show, Eq, Generic)

newtype Monto = Monto (Money.Dense "ARS")
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Real, ToJSON, FromJSON)

monto2Text :: Monto -> Text
monto2Text (Monto m) =
  Money.denseToDecimal Money.defaultDecimalConf Money.Round m

text2Monto :: Text -> Maybe Monto
text2Monto rawNumber =
  rawNumber
  & Money.denseFromDecimal Money.defaultDecimalConf
  & fmap Monto

monto2Dense :: Monto -> Money.Dense "ARS"
monto2Dense (Monto m) = m

data Participante = Participante
  { participanteId :: ULID
  , participanteNombre :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ULID where
  toJSON :: ULID -> Value
  toJSON = String . pack . show

instance FromJSON ULID where
  parseJSON :: Value -> Parser ULID
  parseJSON = withText "ULID" $ \t ->
    case readMaybe @ULID $ unpack t of
      Just ulid -> pure ulid
      Nothing -> fail "Invalid ulid"

instance ToJSONKey ULID

instance FromJSONKey ULID

instance ToHttpApiData ULID where
  toQueryParam :: ULID -> Text
  toQueryParam = pack . show

instance FromHttpApiData ULID where
  parseUrlPiece :: Text -> Either Text ULID
  parseUrlPiece t = do
    case readMaybe @ULID $ unpack t of
      Just ulid -> pure ulid
      Nothing -> Left $ "cant parse a ulid from: " <> pack (show t) <> ""

newtype ParticipanteId = ParticipanteId ULID
  deriving (Generic)
  deriving newtype (Show, Eq, ToJSONKey, FromJSONKey)

instance Ord ParticipanteId where
  compare (ParticipanteId ulid1) (ParticipanteId ulid2) =
    ulidToInteger ulid1 `compare` ulidToInteger ulid2

participanteId2ULID :: ParticipanteId -> ULID
participanteId2ULID (ParticipanteId ulid) = ulid

data Pago = Pago
  { pagoId :: ULID
  , monto :: Monto
  , nombre :: Text
  , deudores :: [Parte]
  , pagadores :: [Parte]
  } deriving (Show, Eq, Generic)

data Parte
  = MontoFijo Monto ParticipanteId
  | Ponderado Integer ParticipanteId
  deriving (Show, Eq, Generic)

parteParticipante :: Parte -> ParticipanteId
parteParticipante (Ponderado _ p) = p
parteParticipante (MontoFijo _ p) = p

instance FromHttpApiData Monto where
  parseUrlPiece :: Text -> Either Text Monto
  parseUrlPiece t = do
    rawNumber <- parseUrlPiece @Text t
    case Money.denseFromDecimal Money.defaultDecimalConf rawNumber of
      Just n -> pure $ Monto n
      Nothing -> Left $ "monto invalido: " <> pack (show t)
  parseQueryParam :: Text -> Either Text Monto
  parseQueryParam t = do
    rawNumber <- parseQueryParam @Text t
    case Money.denseFromDecimal Money.defaultDecimalConf rawNumber of
      Just n -> pure $ Monto n
      Nothing -> Left $ "monto invalido: " <> pack (show t)

buscarParticipante :: Grupo -> ParticipanteId -> Participante
buscarParticipante grupo (ParticipanteId pId) =
  grupo.participantes
  & List.find (\p -> p.participanteId == pId)
  & Maybe.fromMaybe (error $ "participante " <> show pId <> "not found")

nullUlid :: ULID
nullUlid = read @ULID "00000000000000000000000000"

instance IsElmDefinition ULID where
  compileElmDef :: Proxy ULID -> ETypeDef
  compileElmDef _ =
    ETypePrimAlias (EPrimAlias {epa_name = ETypeName {et_name = "ULID", et_args = []}, epa_type = ETyCon (ETCon {tc_name = "String"})})

instance IsElmDefinition Monto where
  compileElmDef :: Proxy Monto -> ETypeDef
  compileElmDef _ =
    ETypePrimAlias $ EPrimAlias
      { epa_name = ETypeName
          { et_name = "Monto"
          , et_args = []
          }
      , epa_type = ETyTuple 3
          & flip ETyApp (ETyCon (ETCon {tc_name = "String"}))
          & flip ETyApp (ETyCon (ETCon {tc_name = "Int"}))
          & flip ETyApp (ETyCon (ETCon {tc_name = "Int"}))
      }

Elm.deriveBoth Elm.defaultOptions ''ParticipanteId
Elm.deriveBoth Elm.defaultOptions ''Parte
Elm.deriveBoth Elm.defaultOptions ''Pago
Elm.deriveBoth Elm.defaultOptions ''Participante
Elm.deriveBoth Elm.defaultOptions ''Grupo
