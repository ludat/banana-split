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
import BananaSplit.Monto (Monto, monto2Text, montoDiffText)
import BananaSplit.Participante (Participante, ParticipanteId)
import BananaSplit.ULID

data Grupo = Grupo
  { id :: ULID
  , nombre :: Text
  , pagos :: [Pago]
  , participantes :: [Participante]
  }
  deriving (Show, Eq, Generic)

data ShallowGrupo = ShallowGrupo
  { id :: ULID
  , nombre :: Text
  , participantes :: [Participante]
  , isFrozen :: Bool
  }
  deriving (Show, Eq, Generic)

data Pago = Pago
  { pagoId :: ULID
  , monto :: Monto
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
  -- ^ this is total amount of money involved but its a bit of a cache
  -- from the Distribuciones to be able to show it on the UI.
  }
  deriving (Show, Eq, Generic)

calcularNetosTotales :: Grupo -> Netos Monto
calcularNetosTotales grupo =
  grupo.pagos
    & filter isValid
    & fmap calcularNetosPago
    & mconcat

calcularNetosPago :: Pago -> Netos Monto
calcularNetosPago pago =
  fromMaybe mempty $ getNetosResumen $ getResumenPago pago

getResumenPago :: Pago -> ResumenNetos
getResumenPago pago =
  let
    resumenPagadores = getResumen pago.monto pago.pagadores
    resumenDeudores = getResumen pago.monto pago.deudores
  in case (resumenPagadores, resumenDeudores) of
    (ResumenNetos _ netosPagadores erroresPagadores, ResumenNetos _ netosDeudores erroresDeudores) ->
      let
        netos = netosPagadores <> fmap negate netosDeudores
        total = totalNetos netos
        extraErrors = if
          | total /= 0 ->
                [ ErrorResumen mempty ("Las netos no estan balanceados, la suma debería dar 0 pero da: " <> monto2Text total)
                ]
          | otherwise -> []
      in ResumenNetos pago.monto netos $
        fmap (relabelError "pagadores") erroresPagadores <>
        fmap (relabelError "deudores") erroresDeudores <>
        extraErrors
  where
    relabelError :: Text -> ErrorResumen -> ErrorResumen
    relabelError scope (ErrorResumen objeto mensaje) = ErrorResumen (scope : objeto) mensaje

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

-- >>> toJSON (Monto $ Decimal.roundTo 2 1000 :: Monto)
-- Object (fromList [("lugaresDespuesDeLaComa",Number 2.0),("valor",Number 100000.0)])

-- >>> compileElmDef (Proxy :: Proxy Pago)
-- ETypeAlias (EAlias {ea_name = ETypeName {et_name = "Pago", et_args = []}, ea_fields = [("pagoId",ETyCon (ETCon {tc_name = "ULID"})),("monto",ETyCon (ETCon {tc_name = "Monto"})),("nombre",ETyCon (ETCon {tc_name = "Text"})),("deudores",ETyApp (ETyCon (ETCon {tc_name = "List"})) (ETyCon (ETCon {tc_name = "Parte"}))),("pagadores",ETyApp (ETyCon (ETCon {tc_name = "List"})) (ETyCon (ETCon {tc_name = "Parte"})))], ea_omit_null = False, ea_newtype = False, ea_unwrap_unary = True})

-- Elm.deriveElmDef Elm.defaultOptions ''Monto
Elm.deriveBoth Elm.defaultOptions ''Parte
Elm.deriveBoth Elm.defaultOptions ''Pago
Elm.deriveBoth Elm.defaultOptions ''ShallowPago
Elm.deriveBoth Elm.defaultOptions ''Grupo
Elm.deriveBoth Elm.defaultOptions ''ShallowGrupo
