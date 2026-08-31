{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module BananaSplit.TasaDeCambio (
  -- * Lo que cruza el borde (JSON, Elm, base de datos)
  TasaDeCambio (..),

  -- * Llevar todo a una sola moneda
  TablaDeTasas,
  tablaDeTasas,
  cantidadDeTasas,
  Factor,
  unFactor,
  factorEntre,
  convertirMonto,

  -- * Los netos del grupo en la moneda de la tabla
  ConsolidadoNetos (..),
  consolidarNetos,
) where

import Data.Decimal qualified as Decimal
import Data.Map.Strict qualified as Map
import Elm.Derive qualified as Elm

import BananaSplit.Deudas (
  Netos,
  distribuirEntrePonderados,
  filterNetos,
  totalNetos,
 )
import BananaSplit.Moneda (Moneda, PorMoneda (..))
import BananaSplit.Monto (Monto (..), inMonto)
import BananaSplit.ULID (ULID)
import Preludat

-- | Una tasa como llega del formulario o como está guardada en la base: dos
-- monedas y cuánto de cada una, sin ninguna garantía. Es el único formato que
-- viaja en JSON, porque es el que entiende el front.
--
-- Las monedas son "una" y "la otra" y no "desde" y "hasta" porque una tasa no
-- tiene sentido: decir que 1 USD son 1000 ARS es exactamente lo mismo que decir
-- que 1000 ARS son 1 USD. El sentido lo pone quien convierte, en 'factorEntre'.
data TasaDeCambio = TasaDeCambio
  { id :: ULID
  , unaMoneda :: Moneda
  , otraMoneda :: Moneda
  , unMonto :: Monto
  , otroMonto :: Monto
  }
  deriving (Show, Eq, Generic)

data ConsolidadoNetos = ConsolidadoNetos
  { moneda :: Moneda
  , netos :: Netos Monto
  , monedasConvertidas :: [Moneda]
  , monedasSinTasa :: [Moneda]
  }
  deriving (Show, Eq, Generic)

newtype Factor = Factor Rational
  deriving (Show, Eq, Ord)

instance Semigroup Factor where
  (<>) :: Factor -> Factor -> Factor
  Factor uno <> Factor otro = Factor $ uno * otro

instance Monoid Factor where
  mempty :: Factor
  mempty = Factor 1

unFactor :: Factor -> Rational
unFactor (Factor factor) = factor

data TablaDeTasas = TablaDeTasas
  { base :: Moneda
  , equivalencias :: Map Moneda Equivalencia
  }
  deriving (Show, Eq)

data Equivalencia = Equivalencia
  { deLaOtra :: Monto
  , deLaBase :: Monto
  }
  deriving (Show, Eq)

tablaDeTasas :: Moneda -> [TasaDeCambio] -> TablaDeTasas
tablaDeTasas base tasas =
  TablaDeTasas
    { base = base
    , equivalencias =
        tasas
          & mapMaybe (equivalenciaCon base)
          & Map.fromList
    }

-- | Cuántas tasas terminó usando la tabla: una por cada moneda que no es la
-- base. Como la que no le sirve no deja rastro, comparar contra el largo de la
-- lista es lo que dice si entraron todas.
cantidadDeTasas :: TablaDeTasas -> Int
cantidadDeTasas tabla = Map.size tabla.equivalencias

equivalenciaCon :: Moneda -> TasaDeCambio -> Maybe (Moneda, Equivalencia)
equivalenciaCon base tasa = do
  guard $ tasa.unaMoneda /= tasa.otraMoneda
  guard $ tasa.unMonto > 0 && tasa.otroMonto > 0
  case (tasa.unaMoneda == base, tasa.otraMoneda == base) of
    (True, _) ->
      Just (tasa.otraMoneda, Equivalencia{deLaOtra = tasa.otroMonto, deLaBase = tasa.unMonto})
    (_, True) ->
      Just (tasa.unaMoneda, Equivalencia{deLaOtra = tasa.unMonto, deLaBase = tasa.otroMonto})
    _ -> Nothing

-- | Por cuánto multiplicar un monto de esa moneda para expresarlo en la base de
-- la tabla.
factorEntre :: TablaDeTasas -> Moneda -> Maybe Factor
factorEntre tabla desde
  | desde == tabla.base = Just mempty
  | otherwise = do
      equivalencia <- Map.lookup desde tabla.equivalencias
      pure
        $ Factor
        $ toRational (inMonto equivalencia.deLaBase)
        / toRational (inMonto equivalencia.deLaOtra)

convertirMonto :: Factor -> Monto -> Monto
convertirMonto factor monto =
  Monto $ Decimal.realFracToDecimal 2 (toRational (inMonto monto) * unFactor factor)

-- | Qué pasó con los netos de una moneda al llevarlos a la moneda destino.
data ConversionDeMoneda
  = Convertida Moneda (Netos Monto)
  | SinTasa Moneda

consolidarNetos :: TablaDeTasas -> PorMoneda (Netos Monto) -> ConsolidadoNetos
consolidarNetos tabla (PorMoneda netosPorMoneda) =
  let conversiones =
        netosPorMoneda
          & Map.toList
          & fmap
            ( \(moneda, netos) ->
                case factorEntre tabla moneda of
                  Nothing -> SinTasa moneda
                  Just factor -> Convertida moneda $ convertirNetos factor netos
            )
  in ConsolidadoNetos
       { moneda = tabla.base
       , netos = foldMap (\case Convertida _ netos -> netos; SinTasa _ -> mempty) conversiones
       , monedasConvertidas = [moneda | Convertida moneda _ <- conversiones]
       , monedasSinTasa = [moneda | SinTasa moneda <- conversiones]
       }

convertirNetos :: Factor -> Netos Monto -> Netos Monto
convertirNetos factor netos =
  let
    acreedores = filterNetos (> 0) netos
    deudores = fmap negate $ filterNetos (< 0) netos

    totalConvertido = convertirMonto factor (totalNetos acreedores)
  in
    repartirEntre totalConvertido acreedores
      <> (negate <$> repartirEntre totalConvertido deudores)
  where
    repartirEntre total lado
      | totalNetos lado == 0 = mempty
      | otherwise =
          lado
            & fmap inMonto
            & distribuirEntrePonderados total

Elm.deriveBoth Elm.defaultOptions ''TasaDeCambio
Elm.deriveBoth Elm.defaultOptions ''ConsolidadoNetos
