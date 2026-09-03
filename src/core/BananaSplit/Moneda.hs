{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module BananaSplit.Moneda (
  Moneda (..),
  PorMoneda (..),
  escalaDe,
  todasLasMonedas,
  enMoneda,
  filterConMoneda,
  filterPorMoneda,
  forMonedaM,
) where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Elm.Derive qualified as Elm
import GHC.Generics (Generically (..))
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

import Preludat

data Moneda
  = ARS
  | USD
  | EUR
  | BRL
  | UYU
  | CLP
  | GBP
  deriving stock (Show, Read, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (ToJSONKey, FromJSONKey)
  deriving (ToJSON, FromJSON) via (Generically Moneda)

instance ToHttpApiData Moneda where
  toUrlPiece :: Moneda -> Text
  toUrlPiece = show

instance FromHttpApiData Moneda where
  parseUrlPiece :: Text -> Either Text Moneda
  parseUrlPiece t =
    case readMaybe t of
      Just moneda -> Right moneda
      Nothing -> Left $ "no existe la moneda " <> t

escalaDe :: Moneda -> Word8
escalaDe = \case
  ARS -> 2
  USD -> 2
  EUR -> 2
  BRL -> 2
  UYU -> 2
  CLP -> 2
  GBP -> 2

newtype PorMoneda a
  = PorMoneda (Map Moneda a)
  deriving (Show, Eq, Generic, Functor, Foldable)

instance (Semigroup a) => Semigroup (PorMoneda a) where
  PorMoneda map1 <> PorMoneda map2 = PorMoneda $ Map.unionWith (<>) map1 map2

instance (Semigroup a) => Monoid (PorMoneda a) where
  mempty = PorMoneda mempty

forMonedaM :: (Monad m) => PorMoneda a -> (Moneda -> a -> m [b]) -> m [b]
forMonedaM (PorMoneda m) f =
  fmap concat $ traverse (uncurry f) $ Map.toList m

filterPorMoneda :: (a -> Bool) -> PorMoneda a -> PorMoneda a
filterPorMoneda f (PorMoneda m) =
  PorMoneda $ Map.filter f m

-- | Como 'filterPorMoneda' pero mirando también de qué moneda se trata.
filterConMoneda :: (Moneda -> a -> Bool) -> PorMoneda a -> PorMoneda a
filterConMoneda f (PorMoneda m) =
  PorMoneda $ Map.filterWithKey f m

enMoneda :: a -> Moneda -> PorMoneda a
enMoneda a moneda =
  PorMoneda (Map.singleton moneda a)

todasLasMonedas :: [Moneda]
todasLasMonedas = [minBound .. maxBound]

Elm.deriveBoth Elm.defaultOptions ''PorMoneda
Elm.deriveElmDef Elm.defaultOptions ''Moneda
