{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module BananaSplit
    ( Deudas (..)
    , Grupo (..)
    , Monto (..)
    , Pago (..)
    , Parte (..)
    , Participante (..)
    , ParticipanteId (..)
    , Repartija (..)
    , RepartijaItem (..)
    , Transaccion (..)
    , buscarParticipante
    , calcularDeudasPago
    , calcularDeudasTotales
    , deudasToPairs
    , monto2Dense
    , monto2Text
    , nullUlid
    , parteParticipante
    , participanteId2ULID
    , resolverDeudasNaif
    , text2Monto
    ) where

import Data.Aeson
import Data.Aeson.Types
import Data.Either (partitionEithers)
import Data.Function
import Data.List (maximumBy, sortOn)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Data.Ord (Down (..))
import Data.Text (Text, pack, unpack)
import Data.ULID (ULID, ulidToInteger)

import Elm.Derive qualified as Elm
import Elm.TyRep (EPrimAlias (..), ETCon (..), EType (..), ETypeDef (..), ETypeName (..),
                  IsElmDefinition (..))

import GHC.Generics (Generic)

import Money qualified
import Money.Aeson ()

import Servant

import Text.Read (readMaybe)

data Grupo = Grupo
  { grupoId :: ULID
  , grupoNombre :: Text
  , pagos :: [Pago]
  , participantes :: [Participante]
  } deriving (Show, Eq, Generic)

newtype Monto = Monto (Money.Dense "ARS")
  deriving stock (Show, Eq, Ord)
  deriving newtype (Num, ToJSON, FromJSON)

data Repartija = Repartija
  { repartijaId :: ULID
  , repartijaNombre :: Text
  , repartijaExtra :: Monto
  , repartijaItems :: [RepartijaItem]
  } deriving (Show, Eq, Generic)

data RepartijaItem = RepartijaItem
  { repartijaItemId :: ULID
  , repartijaItemNombre :: Text
  , repartijaItemMonto :: Monto
  , repartijaItemCantidad :: Maybe Int
  } deriving (Show, Eq, Generic)

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
  & List.find (\p -> participanteId p == pId)
  & Maybe.fromMaybe (error $ "participante " <> show pId <> "not found")

nullUlid :: ULID
nullUlid = read @ULID "00000000000000000000000000"

newtype Deudas a = Deudas (Map ParticipanteId a)
  deriving newtype (Show, Eq, Functor)

deudasToPairs :: Ord a => Deudas a -> [(ParticipanteId, a)]
deudasToPairs (Deudas deudasMap) =
  deudasMap
  & Map.toAscList
  & sortOn (\(p, m) -> (Down m, p))

instance Num a => Monoid (Deudas a) where
  mempty = Deudas Map.empty

instance Num a => Semigroup (Deudas a) where
  Deudas d1 <> Deudas d2 = Deudas $ Map.unionWith (+) d1 d2

totalDeudas :: Num a => Deudas a -> a
totalDeudas (Deudas deudasMap) =
  deudasMap
  & Map.elems
  & sum

calcularDeudasTotales :: Grupo -> Deudas Monto
calcularDeudasTotales grupo =
  grupo.pagos
  & fmap calcularDeudasPago
  & mconcat

mkDeuda :: ParticipanteId -> a -> Deudas a
mkDeuda participanteId monto =
  Deudas $ Map.singleton participanteId monto

calcularDeudasPago :: Pago -> Deudas Monto
calcularDeudasPago pago =
  calcularDeudas pago.monto pago.pagadores <> fmap (* -1) (calcularDeudas pago.monto pago.deudores)
  where
    calcularDeudas montoOriginal partes =
      let
        (ponderados, fijos) =
          partes
          & fmap (\case
            Ponderado parte participante -> Left (participante, parte)
            MontoFijo monto participante -> Right (participante, monto)
          )
          & partitionEithers
        deudasFijos =
          fijos
          & fmap (uncurry mkDeuda)
          & mconcat
        totalFijo =
          totalDeudas deudasFijos
        deudasPonderados =
          ponderados
          & fmap (uncurry mkDeuda)
          & mconcat
      in deudasFijos <> distribuirEntrePonderados (montoOriginal - totalFijo) deudasPonderados

distribuirEntrePonderados :: Monto -> Deudas Integer -> Deudas Monto
distribuirEntrePonderados (Monto m) deudas =
  let
    parteTotal = totalDeudas deudas
  in deudas
      & fmap (\p -> Monto $ Money.dense' $ fromInteger p * toRational m / toRational parteTotal)

extraerMaximoDeudor :: Deudas Monto -> (ParticipanteId, Monto)
extraerMaximoDeudor (Deudas deudasMap) =
  deudasMap
  & Map.filter (< 0)
  & fmap (* -1)
  & Map.toList
  & maximumBy (compare `on` snd)

extraerMaximoPagador :: Deudas Monto -> (ParticipanteId, Monto)
extraerMaximoPagador (Deudas deudasMap) =
  deudasMap
  & Map.filter (> 0)
  & Map.toList
  & maximumBy (compare `on` snd)

removerDeudor :: ParticipanteId -> Deudas m -> Deudas m
removerDeudor participanteId (Deudas deudasMap) =
  Deudas $ Map.delete participanteId deudasMap

data Transaccion =
  Transaccion ParticipanteId ParticipanteId Monto
  deriving (Show, Eq, Generic)

resolverDeudasNaif :: Deudas Monto -> [Transaccion]
resolverDeudasNaif deudas
  | deudoresNoNulos deudas == 0 = []
  | deudoresNoNulos deudas == 1 = error $ show deudas
  | otherwise =
      let
        (mayorDeudor, mayorDeuda) = extraerMaximoDeudor deudas
        deudas' = removerDeudor mayorDeudor deudas

        (mayorPagador, mayorPagado) = extraerMaximoPagador deudas'
        deudas'' = removerDeudor mayorPagador deudas'

      in case compare mayorDeuda mayorPagado of
          LT -> Transaccion mayorDeudor mayorPagador mayorDeuda
            : resolverDeudasNaif (deudas'' <> mkDeuda mayorPagador (mayorPagado - mayorDeuda))
          GT -> Transaccion mayorDeudor mayorPagador mayorPagado
            : resolverDeudasNaif (deudas'' <> mkDeuda mayorDeudor (-mayorDeuda + mayorPagado))
          EQ -> Transaccion mayorDeudor mayorPagador mayorPagado
            : resolverDeudasNaif deudas''

deudoresNoNulos :: Deudas Monto -> Int
deudoresNoNulos (Deudas deudasMap) =
  deudasMap
  & Map.toList
  & filter (\(_p, m) -> m /= 0)
  & length

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
Elm.deriveBoth Elm.defaultOptions ''Transaccion
Elm.deriveBoth Elm.defaultOptions ''Deudas
Elm.deriveBoth Elm.defaultOptions ''RepartijaItem
Elm.deriveBoth Elm.defaultOptions ''Repartija
Elm.deriveBoth Elm.defaultOptions ''Grupo
