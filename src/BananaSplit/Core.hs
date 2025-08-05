{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
module BananaSplit.Core
    ( Grupo (..)
    , Parte (Ponderado, MontoFijo)
    , Participante (..)
    , ParticipanteId (..)
    , nullUlid
    , participanteId2ULID
      -- Monto
    , Monto (..)
    , inMonto
    , monto2Text
      -- Pago
    , Pago (..)
    , getPagoErrors
    , isPagoValid
    ) where

import Control.Monad.Fail (fail)

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Decimal (Decimal)
import Data.Decimal qualified as Decimal
import Data.ULID

import Elm.Derive qualified as Elm
import Elm.TyRep (EAlias (..), EPrimAlias (..), ETCon (..), EType (..), ETypeDef (..),
                  ETypeName (..), IsElmDefinition (..))

import GHC.Generics

import Protolude
import Protolude.Error

import Servant (FromHttpApiData (..), ToHttpApiData)
import Servant.API (ToHttpApiData (..))

data Grupo = Grupo
  { grupoId :: ULID
  , grupoNombre :: Text
  , pagos :: [Pago]
  , participantes :: [Participante]
  } deriving (Show, Eq, Generic)

newtype Monto = Monto Decimal
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Real, Enum)
  deriving anyclass (ToJSON, FromJSON)

instance Generic Monto where
  type Rep Monto =
    D1 ('MetaData "Monto" "BananaSplit.Core" "banana-split" 'False)
      (C1 ('MetaCons "Monto" 'PrefixI 'False)
        ( S1 ('MetaSel ('Just "lugaresDespuesDeLaComa") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (Rec0 Word8)
         :*: S1 ('MetaSel ('Just "valor") 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy) (Rec0 Integer)
        )
      )

  from (Monto m) =
    let
      lugaresDespuesDeLaComa = Decimal.decimalPlaces m
      valor =  Decimal.decimalMantissa m
    in
    M1 (M1 (M1 (K1 lugaresDespuesDeLaComa) :*: M1 (K1 valor)))

  to (M1 (M1 (M1 (K1 lugaresDespuesDeLaComa) :*: M1 (K1 valor)))) =
    Monto $ Decimal.Decimal lugaresDespuesDeLaComa valor

monto2Text :: Monto -> Text
monto2Text (Monto m) =
  show m

-- | Conseguir decimal de adentro de un monto.
--
-- el patron de unCosa pero en español.
inMonto :: Monto -> Decimal
inMonto (Monto m) = m

data Participante = Participante
  { participanteId :: ULID
  , participanteNombre :: Text
  }
  deriving (Show, Eq, Generic)

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


isPagoValid :: Pago -> Bool
isPagoValid pago = null (getPagoErrors pago)

getPagoErrors :: Pago -> [Text]
getPagoErrors p = mconcat
  [ [ "Monto invalido. No puede ser negativo" | True <- [p.monto < 0]]
  , [ "Monto invalido. No puede ser 0" | True <- [p.monto == 0]]
  -- Deudores
  , [ "Deudores invalidos. No puede ser vacío" | True <- [null p.deudores]]
  -- Pagadores
  , [ "Pagadores invalidos. No puede ser vacío" | True <- [null p.pagadores]]
  ]

data Parte
  = MontoFijo Monto ParticipanteId
  | Ponderado Integer ParticipanteId
  deriving (Show, Eq, Generic)

instance FromHttpApiData Monto where
  parseUrlPiece :: Text -> Either Text Monto
  parseUrlPiece t = do
    rawNumber <- parseUrlPiece @Text t
    case readMaybe rawNumber of
      Just n -> pure $ Monto n
      Nothing -> Left $ "monto invalido: " <> show t
  parseQueryParam :: Text -> Either Text Monto
  parseQueryParam t = do
    rawNumber <- parseQueryParam @Text t
    case readMaybe rawNumber of
      Just n -> pure $ Monto n
      Nothing -> Left $ "monto invalido: " <> show t

nullUlid :: ULID
nullUlid = fromRight (error "impossible") $ ulidFromInteger 0

instance IsElmDefinition ULID where
  compileElmDef :: Proxy ULID -> ETypeDef
  compileElmDef _ =
    ETypePrimAlias (EPrimAlias {epa_name = ETypeName {et_name = "ULID", et_args = []}, epa_type = ETyCon (ETCon {tc_name = "String"})})

instance IsElmDefinition Monto where
  compileElmDef :: Proxy Monto -> ETypeDef
  compileElmDef _ =
    ETypeAlias (EAlias {
      ea_name = ETypeName {et_name = "Monto", et_args = []},
      ea_fields =
        [ ("lugaresDespuesDeLaComa", ETyCon (ETCon {tc_name = "Int"}))
        , ("valor", ETyCon (ETCon {tc_name = "Int"}))
        ],
      ea_omit_null = False
      , ea_newtype = False
      , ea_unwrap_unary = True
      })

-- >>> toJSON (Monto $ Decimal.roundTo 2 1000 :: Monto)
-- Object (fromList [("lugaresDespuesDeLaComa",Number 2.0),("valor",Number 100000.0)])

-- >>> compileElmDef (Proxy :: Proxy Pago)
-- ETypeAlias (EAlias {ea_name = ETypeName {et_name = "Pago", et_args = []}, ea_fields = [("pagoId",ETyCon (ETCon {tc_name = "ULID"})),("monto",ETyCon (ETCon {tc_name = "Monto"})),("nombre",ETyCon (ETCon {tc_name = "Text"})),("deudores",ETyApp (ETyCon (ETCon {tc_name = "List"})) (ETyCon (ETCon {tc_name = "Parte"}))),("pagadores",ETyApp (ETyCon (ETCon {tc_name = "List"})) (ETyCon (ETCon {tc_name = "Parte"})))], ea_omit_null = False, ea_newtype = False, ea_unwrap_unary = True})

-- Elm.deriveElmDef Elm.defaultOptions ''Monto
Elm.deriveBoth Elm.defaultOptions ''ParticipanteId
Elm.deriveBoth Elm.defaultOptions ''Parte
Elm.deriveBoth Elm.defaultOptions ''Pago
Elm.deriveBoth Elm.defaultOptions ''Participante
Elm.deriveBoth Elm.defaultOptions ''Grupo
