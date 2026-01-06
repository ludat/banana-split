{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module BananaSplit.Core
    ( Grupo (..)
    , Parte (Ponderado, MontoFijo)
    , ShallowGrupo (..)
    , nullUlid
      -- Pago
    , Distribucion (..)
    , DistribucionMontoEquitativo (..)
    , DistribucionMontosEspecificos (..)
    , Pago (..)
    , ShallowPago (..)
    , TipoDistribucion (..)
    , addIsValidPago
    , calcularNetosPago
    , calcularNetosTotales
    , getResumenPago
    , isValid
    ) where

import BananaSplit.Deudas
import BananaSplit.Monto (Monto)
import BananaSplit.Participante (Participante, ParticipanteId)
import BananaSplit.ULID

import Elm.Derive qualified as Elm

import GHC.Generics

import Protolude


data Grupo = Grupo
  { id :: ULID
  , nombre :: Text
  , pagos :: [Pago]
  , participantes :: [Participante]
  } deriving (Show, Eq, Generic)

data ShallowGrupo = ShallowGrupo
  { id :: ULID
  , nombre :: Text
  , participantes :: [Participante]
  } deriving (Show, Eq, Generic)

data Pago = Pago
  { pagoId :: ULID
  , monto :: Monto
  , isValid :: Bool
  , nombre :: Text
  , pagadores :: Distribucion
  , deudores :: Distribucion
  } deriving (Show, Eq, Generic)

data ShallowPago = ShallowPago
  { pagoId :: ULID
  , isValid :: Bool
  , nombre :: Text
  , monto :: Monto
    -- ^ this is total amount of money involved but its a bit of a cache
    -- from the Distribuciones to be able to show it on the UI.
  } deriving (Show, Eq, Generic)

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
    case (resumenPagadores, resumenDeudores) of
      (NetosIncomputables _ errorPagadores, NetosIncomputables _ errorDeudores) ->
        NetosIncomputables (Just pago.monto) $ ErrorResumen Nothing [("pagadores", errorPagadores), ("deudores", errorDeudores)]
      (NetosIncomputables _ errorPagadores, ResumenNetos _ _netosDeudores) ->
        NetosIncomputables (Just pago.monto) $ ErrorResumen Nothing [("pagadores", errorPagadores)]

      (ResumenNetos _ _netosPagadores, NetosIncomputables _ errorDeudores) ->
        NetosIncomputables (Just pago.monto) $ ErrorResumen Nothing [("deudores", errorDeudores)]
      (ResumenNetos _ netosPagadores, ResumenNetos _ netosDeudores) ->
        let
          netos = netosPagadores <> fmap negate netosDeudores
          total = totalNetos netos
          totalPagadores = totalNetos netosPagadores
          totalDeudores = totalNetos $ fmap negate netosDeudores
        in
        if | totalPagadores /= negate totalDeudores -> NetosIncomputables (Just pago.monto) $
                ErrorResumen (Just $ "Las netos no estan balanceadas, el total de pagadores (" <> show totalPagadores <> ") y el total de deudores (" <> show totalDeudores <> ") deberían ser iguales") []
           | total /= 0 -> NetosIncomputables (Just pago.monto) $
                ErrorResumen (Just $ "Las netos no estan balanceadas, la suma debería dar 0 pero da: " <> show total) []
           | pago.monto /= totalPagadores -> NetosIncomputables (Just pago.monto) $
                     ErrorResumen (Just $ "Las netos no estan balanceadas, la suma debería dar 0 pero da: " <> show total) []
           | otherwise -> ResumenNetos (Just pago.monto) netos
    where
      resumenPagadores = getResumen pago.monto pago.pagadores
      resumenDeudores = getResumen pago.monto pago.deudores

isValid :: Pago -> Bool
isValid pago =
  pago
  & getResumenPago
  & getNetosResumen
  & isJust

addIsValidPago :: Pago -> Pago
addIsValidPago pago =
  pago { isValid = isValid pago }

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
