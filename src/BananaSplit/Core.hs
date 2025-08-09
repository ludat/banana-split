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
    , calcularDeudasPago
    , calcularDeudasTotales
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

calcularDeudasTotales :: Grupo -> Deudas Monto
calcularDeudasTotales grupo =
  grupo.pagos
  & filter isValid
  & fmap calcularDeudasPago
  & mconcat

calcularDeudasPago :: Pago -> Deudas Monto
calcularDeudasPago pago =
  fromMaybe mempty $ getDeudasResumen $ getResumenPago pago

getResumenPago :: Pago -> ResumenDeudas
getResumenPago pago =
    case (resumenPagadores, resumenDeudores) of
      (DeudasIncomputables _ errorPagadores, DeudasIncomputables _ errorDeudores) ->
        DeudasIncomputables (Just pago.monto) $ ErrorResumen Nothing [("pagadores", errorPagadores), ("deudores", errorDeudores)]
      (DeudasIncomputables _ errorPagadores, ResumenDeudas _ _deudasDeudores) ->
        DeudasIncomputables (Just pago.monto) $ ErrorResumen Nothing [("pagadores", errorPagadores)]

      (ResumenDeudas _ _deudasPagadores, DeudasIncomputables _ errorDeudores) ->
        DeudasIncomputables (Just pago.monto) $ ErrorResumen Nothing [("deudores", errorDeudores)]
      (ResumenDeudas _ deudasPagadores, ResumenDeudas _ deudasDeudores) ->
        let
          deudas = deudasPagadores <> fmap negate deudasDeudores
          total = totalDeudas deudas
          totalPagadores = totalDeudas deudasPagadores
          totalDeudores = totalDeudas $ fmap negate deudasDeudores
        in
        if | totalPagadores /= negate totalDeudores -> DeudasIncomputables (Just pago.monto) $
                ErrorResumen (Just $ "Las deudas no estan balanceadas, el total de pagadores (" <> show totalPagadores <> ") y el total de deudores (" <> show totalDeudores <> ") deberían ser iguales") []
           | total /= 0 -> DeudasIncomputables (Just pago.monto) $
                ErrorResumen (Just $ "Las deudas no estan balanceadas, la suma debería dar 0 pero da: " <> show total) []
           | pago.monto /= totalPagadores -> DeudasIncomputables (Just pago.monto) $
                     ErrorResumen (Just $ "Las deudas no estan balanceadas, la suma debería dar 0 pero da: " <> show total) []
           | otherwise -> ResumenDeudas (Just pago.monto) deudas
    where
      resumenPagadores = getResumen pago.monto pago.pagadores
      resumenDeudores = getResumen pago.monto pago.deudores

isValid :: Pago -> Bool
isValid pago =
  pago
  & getResumenPago
  & getDeudasResumen
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
