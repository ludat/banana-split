{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module BananaSplit.Metricas (
  BigOne (..),
  BusiestDia (..),
  BusiestMes (..),
  MetricasGrupo (..),
  MetricasPorMoneda (..),
  calcularMetricas,
  esLiquidacion,
) where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Time (Day, toGregorian)
import Elm.Derive qualified as Elm

import BananaSplit.Core
import BananaSplit.Deudas
import BananaSplit.Moneda (Moneda, PorMoneda (..))
import BananaSplit.Monto (Monto)
import BananaSplit.Participante (ParticipanteId)
import BananaSplit.ULID
import Preludat

-- | El pago más grande que no sea una liquidación.
data BigOne = BigOne
  { pagoId :: ULID
  , monto :: Monto
  , nombre :: Text
  , fecha :: Day
  }
  deriving (Show, Eq, Generic)

-- | El mes (año, número de mes) con más plata movida.
data BusiestMes = BusiestMes
  { anio :: Int
  , mes :: Int
  , total :: Monto
  }
  deriving (Show, Eq, Generic)

-- | El día con más plata movida.
data BusiestDia = BusiestDia
  { dia :: Day
  , total :: Monto
  }
  deriving (Show, Eq, Generic)

data MetricasPorMoneda = MetricasPorMoneda
  { totalPagadoPorParticipante :: Netos Monto
  -- ^ Suma (sin signo) de lo que pagó cada participante, sobre todos los pagos válidos.
  , totalPagosSaldadosPorParticipante :: Netos Monto
  -- ^ Suma (sin signo) de lo que pagó cada participante, sólo en pagos que son liquidaciones.
  , totalGastadoPorParticipante :: Netos Monto
  -- ^ Suma (sin signo) de lo que gastó cada participante, sólo en pagos que NO son liquidaciones.
  , generosidad :: Map ParticipanteId Double
  -- ^ totalPagado / totalGastado por participante. Se omite si totalGastado es 0.
  , busiestMes :: Maybe BusiestMes
  , busiestDia :: Maybe BusiestDia
  , theBigOne :: Maybe BigOne
  }
  deriving (Show, Eq, Generic)

data MetricasGrupo = MetricasGrupo
  { porMoneda :: PorMoneda MetricasPorMoneda
  }
  deriving (Show, Eq, Generic)

-- | Una liquidación es un pago donde tanto pagadores como deudores son una
-- transferencia simple: una única parte ponderada (i.e. "de fulano a mengano").
esLiquidacion :: Pago -> Bool
esLiquidacion pago =
  esTransferenciaSimple pago.pagadores && esTransferenciaSimple pago.deudores
  where
    esTransferenciaSimple d =
      case d.tipo of
        TipoDistribucionPartes (DistribucionPartes{partes = [Ponderado _ _]}) -> True
        _ -> False

calcularMetricas :: Grupo -> MetricasGrupo
calcularMetricas grupo =
  let
    pagosValidos = grupo.pagos & filter isValid
    pagosPorMoneda :: Map Moneda [Pago]
    pagosPorMoneda =
      pagosValidos
        & fmap (\pago -> (pago.moneda, [pago]))
        & Map.fromListWith (<>)
  in
    MetricasGrupo{porMoneda = PorMoneda $ fmap calcularMetricasPorMoneda pagosPorMoneda}

calcularMetricasPorMoneda :: [Pago] -> MetricasPorMoneda
calcularMetricasPorMoneda pagos =
  let
    pagosNoLiquidacion = pagos & filter (not . esLiquidacion)
    pagosLiquidacion = pagos & filter esLiquidacion

    netosPagador pago = (getResumen pago.monto pago.pagadores).netos
    netosDeudor pago = (getResumen pago.monto pago.deudores).netos

    totalPagadoPorParticipante = pagos & fmap netosPagador & mconcat
    totalPagosSaldadosPorParticipante = pagosLiquidacion & fmap netosPagador & mconcat
    totalGastadoPorParticipante = pagosNoLiquidacion & fmap netosDeudor & mconcat

    generosidad =
      case (totalPagadoPorParticipante, totalGastadoPorParticipante) of
        (Netos pagadoMap, Netos gastadoMap) ->
          gastadoMap
            & Map.filter (/= 0)
            & Map.mapWithKey
              ( \participanteId gastado ->
                  let pagado = Map.findWithDefault 0 participanteId pagadoMap
                  in realToFrac pagado / realToFrac gastado
              )

    busiestMes =
      pagos
        & fmap (\pago -> let (anio, mes, _) = toGregorian pago.fecha in ((fromIntegral anio, mes), pago.monto))
        & Map.fromListWith (+)
        & Map.toList
        & \case
          [] -> Nothing
          xs ->
            xs
              & List.maximumBy (comparing snd)
              & (\((anio, mes), total) -> Just BusiestMes{anio = anio, mes = mes, total = total})

    busiestDia =
      pagos
        & fmap (\pago -> (pago.fecha, pago.monto))
        & Map.fromListWith (+)
        & Map.toList
        & \case
          [] -> Nothing
          xs ->
            xs
              & List.maximumBy (comparing snd)
              & (\(dia, total) -> Just BusiestDia{dia = dia, total = total})

    theBigOne =
      case pagosNoLiquidacion of
        [] -> Nothing
        ps ->
          ps
            & List.maximumBy (comparing (.monto))
            & (\pago -> Just BigOne{pagoId = pago.pagoId, monto = pago.monto, nombre = pago.nombre, fecha = pago.fecha})
  in
    MetricasPorMoneda
      { totalPagadoPorParticipante = totalPagadoPorParticipante
      , totalPagosSaldadosPorParticipante = totalPagosSaldadosPorParticipante
      , totalGastadoPorParticipante = totalGastadoPorParticipante
      , generosidad = generosidad
      , busiestMes = busiestMes
      , busiestDia = busiestDia
      , theBigOne = theBigOne
      }

Elm.deriveBoth Elm.defaultOptions ''BigOne
Elm.deriveBoth Elm.defaultOptions ''BusiestMes
Elm.deriveBoth Elm.defaultOptions ''BusiestDia
Elm.deriveBoth Elm.defaultOptions ''MetricasPorMoneda
Elm.deriveBoth Elm.defaultOptions ''MetricasGrupo
