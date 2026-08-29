{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module BananaSplit.TasaDeCambio (
  ConsolidadoNetos (..),
  TasaDeCambio (..),
  consolidarNetos,
  convertirMonto,
  factorEntre,
  normalizarTasa,
  validarTasas,
) where

import Data.Decimal qualified as Decimal
import Data.Map.Strict qualified as Map
import Elm.Derive qualified as Elm

import BananaSplit.Deudas (Netos (..), totalNetos)
import BananaSplit.Moneda (Moneda, PorMoneda (..))
import BananaSplit.Monto (Monto (..), inMonto)
import BananaSplit.ULID (ULID)
import Preludat

data TasaDeCambio = TasaDeCambio
  { id :: ULID
  , monedaFrom :: Moneda
  , monedaTo :: Moneda
  , montoFrom :: Monto
  , montoTo :: Monto
  }
  deriving (Show, Eq, Generic)

data ConsolidadoNetos = ConsolidadoNetos
  { moneda :: Moneda
  , netos :: Netos Monto
  , monedasConvertidas :: [Moneda]
  , monedasSinTasa :: [Moneda]
  }
  deriving (Show, Eq, Generic)

factorEntre :: [TasaDeCambio] -> Moneda -> Moneda -> Maybe Rational
factorEntre tasas desde hasta
  | desde == hasta = Just 1
  | otherwise =
      tasas
        & mapMaybe (factorDeTasa desde hasta)
        & head

factorDeTasa :: Moneda -> Moneda -> TasaDeCambio -> Maybe Rational
factorDeTasa desde hasta tasa
  | desdeUno == 0 || hastaUno == 0 = Nothing
  | tasa.monedaFrom == desde && tasa.monedaTo == hasta = Just $ hastaUno / desdeUno
  | tasa.monedaFrom == hasta && tasa.monedaTo == desde = Just $ desdeUno / hastaUno
  | otherwise = Nothing
  where
    desdeUno = toRational $ inMonto tasa.montoFrom
    hastaUno = toRational $ inMonto tasa.montoTo

validarTasas :: Moneda -> [TasaDeCambio] -> Either Text [TasaDeCambio]
validarTasas moneda tasas
  | any (\tasa -> tasa.monedaFrom /= moneda && tasa.monedaTo /= moneda) tasas =
      Left "Todas las tasas tienen que involucrar a la moneda que se está guardando"
  | any (\tasa -> tasa.monedaFrom == tasa.monedaTo) tasas =
      Left "Una tasa de cambio tiene que ser entre dos monedas distintas"
  | any (\tasa -> inMonto tasa.montoFrom <= 0 || inMonto tasa.montoTo <= 0) tasas =
      Left "Los montos de una tasa de cambio tienen que ser mayores a cero"
  | length (ordNub pares) /= length pares =
      Left "Hay más de una tasa de cambio para el mismo par de monedas"
  | otherwise = Right tasas
  where
    pares = tasas & fmap (\tasa -> sort [tasa.monedaFrom, tasa.monedaTo])

-- | El orden sale del código de la moneda y no de 'Ord' 'Moneda', que se
-- movería al agregar una moneda al medio del @data Moneda@ y desordenaría en
-- silencio lo que ya está guardado. La tabla tiene un CHECK con el mismo orden.
normalizarTasa :: TasaDeCambio -> TasaDeCambio
normalizarTasa tasa
  | (show tasa.monedaFrom :: Text) <= show tasa.monedaTo = tasa
  | otherwise =
      tasa
        { monedaFrom = tasa.monedaTo
        , monedaTo = tasa.monedaFrom
        , montoFrom = tasa.montoTo
        , montoTo = tasa.montoFrom
        }

convertirMonto :: Rational -> Monto -> Monto
convertirMonto factor monto =
  Monto $ Decimal.realFracToDecimal 2 (toRational (inMonto monto) * factor)

consolidarNetos :: Moneda -> [TasaDeCambio] -> PorMoneda (Netos Monto) -> ConsolidadoNetos
consolidarNetos monedaDestino tasas (PorMoneda netosPorMoneda) =
  let convertidos =
        netosPorMoneda
          & Map.toList
          & fmap
            ( \(moneda, netos) ->
                ( moneda
                , factorEntre tasas moneda monedaDestino
                    & fmap (\factor -> balancear $ fmap (convertirMonto factor) netos)
                )
            )
  in ConsolidadoNetos
       { moneda = monedaDestino
       , netos = convertidos & mapMaybe snd & mconcat
       , monedasConvertidas = convertidos & filter (isJust . snd) & fmap fst
       , monedasSinTasa = convertidos & filter (isNothing . snd) & fmap fst
       }

-- | Redondear cada neto por separado los deja de sumar cero, y 'minimizeTransactions'
-- explota con netos que no cierran.
balancear :: Netos Monto -> Netos Monto
balancear netos@(Netos netosMap)
  | residuo == 0 = netos
  | otherwise =
      case netosMap & Map.toList & sortOn (Down . abs . snd) & head of
        Nothing -> netos
        Just (participanteId, _) ->
          Netos $ Map.adjust (subtract residuo) participanteId netosMap
  where
    residuo = totalNetos netos

Elm.deriveBoth Elm.defaultOptions ''TasaDeCambio
Elm.deriveBoth Elm.defaultOptions ''ConsolidadoNetos
