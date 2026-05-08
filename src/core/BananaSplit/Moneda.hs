{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module BananaSplit.Moneda (
  Moneda (..),
  PorMoneda (..),
  monedaFromText,
  monedaToText,
  todasLasMonedas,
  enMoneda,
  forMonedaM,
) where

import Data.Aeson
import Data.Map.Strict qualified as Map
import Elm.Derive qualified as Elm
import Servant (FromHttpApiData (..))

import Preludat

data Moneda
  = ARS
  | USD
  | EUR
  | BRL
  | UYU
  | CLP
  | GBP
  deriving (Show, Eq, Ord, Generic, Enum, Bounded)

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

monedaToText :: Moneda -> Text
monedaToText = show

monedaFromText :: Text -> Either Text Moneda
monedaFromText = \case
  "ARS" -> Right ARS
  "USD" -> Right USD
  "EUR" -> Right EUR
  "BRL" -> Right BRL
  "UYU" -> Right UYU
  "CLP" -> Right CLP
  "GBP" -> Right GBP
  other -> Left $ "moneda invalida: " <> other

instance ToJSONKey Moneda

instance FromJSONKey Moneda

instance ToJSON Moneda where
  toJSON = toJSON . monedaToText

instance FromJSON Moneda where
  parseJSON = withText "Moneda" $ \t ->
    case monedaFromText t of
      Right m -> pure m
      Left e -> fail $ toS e

instance FromHttpApiData Moneda where
  parseUrlPiece = monedaFromText
  parseQueryParam = monedaFromText

Elm.deriveBoth Elm.defaultOptions ''PorMoneda
Elm.deriveElmDef Elm.defaultOptions ''Moneda
