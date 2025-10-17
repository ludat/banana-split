{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
module BananaSplit.Monto
    ( Monto (..)
    , getLugaresDespuesDeLaComa
    , inMonto
    , mkMonto
    , monto2Text
    , times
    ) where


import Data.Aeson
import Data.Decimal as Decimal

import Elm.TyRep (EAlias (..), ETCon (..), EType (..), ETypeDef (..), ETypeName (..),
                  IsElmDefinition (..))

import GHC.Generics

import Protolude

import Servant (FromHttpApiData (..))

newtype Monto = Monto Decimal
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, Real, Enum)
  deriving anyclass (ToJSON, FromJSON)

mkMonto :: Word8 -> Integer -> Monto
mkMonto lugaresDespuesDeLaComa n =
  Monto $ Decimal.Decimal lugaresDespuesDeLaComa n

getLugaresDespuesDeLaComa :: Monto -> Word8
getLugaresDespuesDeLaComa (Monto (Decimal.Decimal lugaresDespuesDeLaComa _)) =
  lugaresDespuesDeLaComa

times :: Integral n => Monto -> n -> Monto
(Monto (Decimal.Decimal lugaresDespuesDeLaComa decimal)) `times` n =
  Monto $ Decimal lugaresDespuesDeLaComa (decimal * fromIntegral n)

instance Generic Monto where
  type Rep Monto =
    D1 ('MetaData "Monto" "BananaSplit.Monto" "banana-split" 'False)
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
