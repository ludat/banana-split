{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module BananaSplit.Core (
  Grupo (..),
  Parte (Ponderado, MontoFijo),
  ShallowGrupo (..),
  nullUlid,
  -- Pago
  Distribucion (..),
  DistribucionMontoEquitativo (..),
  DistribucionMontosEspecificos (..),
  Pago (..),
  ShallowPago (..),
  TipoDistribucion (..),
  addIsValidPago,
  calcularNetosPago,
  calcularNetosTotales,
  getResumenPago,
  isValid,
) where

import Elm.Derive qualified as Elm
import GHC.Generics
import Protolude

import BananaSplit.Deudas
import BananaSplit.Moneda (Moneda, PorMoneda, enMoneda)
import BananaSplit.Monto (Monto)
import BananaSplit.Participante (Participante, ParticipanteId)
import BananaSplit.ULID

data Grupo = Grupo
  { id :: ULID
  , nombre :: Text
  , pagos :: [Pago]
  , participantes :: [Participante]
  , monedaPorDefecto :: Moneda
  }
  deriving (Show, Eq, Generic)

data ShallowGrupo = ShallowGrupo
  { id :: ULID
  , nombre :: Text
  , participantes :: [Participante]
  , isFrozen :: Bool
  , monedaPorDefecto :: Moneda
  }
  deriving (Show, Eq, Generic)

data Pago = Pago
  { pagoId :: ULID
  , monto :: Monto
  , moneda :: Moneda
  , isValid :: Bool
  , nombre :: Text
  , pagadores :: Distribucion
  , deudores :: Distribucion
  }
  deriving (Show, Eq, Generic)

data ShallowPago = ShallowPago
  { pagoId :: ULID
  , isValid :: Bool
  , nombre :: Text
  , monto :: Monto
  , moneda :: Moneda
  }
  deriving (Show, Eq, Generic)

calcularNetosTotales :: Grupo -> PorMoneda (Netos Monto)
calcularNetosTotales grupo =
  grupo.pagos
    & filter isValid
    & fmap (\pago -> (calcularNetosPago pago) `enMoneda` pago.moneda)
    & mconcat

calcularNetosPago :: Pago -> Netos Monto
calcularNetosPago pago =
  fromMaybe mempty $ getNetosResumen $ getResumenPago pago

getResumenPago :: Pago -> ResumenNetos
getResumenPago pago =
  let
    resumenPagadores = getResumen pago.monto pago.pagadores
    resumenDeudores = getResumen pago.monto pago.deudores
    netos = resumenPagadores.netos <> fmap negate resumenDeudores.netos
    extraErrors = []
  in
    ResumenNetos pago.monto netos $
      fmap (relabelError "pagadores") resumenPagadores.errores
        <> fmap (relabelError "deudores") resumenDeudores.errores
        <> extraErrors

isValid :: Pago -> Bool
isValid pago =
  pago
    & getResumenPago
    & getNetosResumen
    & isJust

addIsValidPago :: Pago -> Pago
addIsValidPago pago =
  pago{isValid = isValid pago}

data Parte
  = MontoFijo Monto ParticipanteId
  | Ponderado Integer ParticipanteId
  deriving (Show, Eq, Generic)

Elm.deriveBoth Elm.defaultOptions ''Parte
Elm.deriveBoth Elm.defaultOptions ''Pago
Elm.deriveBoth Elm.defaultOptions ''ShallowPago
Elm.deriveBoth Elm.defaultOptions ''Grupo
Elm.deriveBoth Elm.defaultOptions ''ShallowGrupo
