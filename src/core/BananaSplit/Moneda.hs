{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}

module BananaSplit.Moneda (
  Moneda (..),
  PorMoneda (..),
  todasLasMonedas,
  enMoneda,
  forMonedaM,
) where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Elm.Derive qualified as Elm
import GHC.Generics (Generically (..))

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

newtype PorMoneda a
  = PorMoneda (Map Moneda a)
  deriving (Show, Eq, Generic, Functor)

instance (Semigroup a) => Semigroup (PorMoneda a) where
  PorMoneda map1 <> PorMoneda map2 = PorMoneda $ Map.unionWith (<>) map1 map2

instance (Semigroup a) => Monoid (PorMoneda a) where
  mempty = PorMoneda mempty

forMonedaM :: (Monad m) => PorMoneda a -> (Moneda -> a -> m [b]) -> m [b]
forMonedaM (PorMoneda m) f =
  fmap concat $ traverse (uncurry f) $ Map.toList m

enMoneda :: a -> Moneda -> PorMoneda a
enMoneda a moneda =
  PorMoneda (Map.singleton moneda a)

todasLasMonedas :: [Moneda]
todasLasMonedas = [minBound .. maxBound]

Elm.deriveBoth Elm.defaultOptions ''PorMoneda
Elm.deriveElmDef Elm.defaultOptions ''Moneda
