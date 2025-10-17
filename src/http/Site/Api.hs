{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Site.Api where


import BananaSplit

import Elm.Derive qualified as Elm

import GHC.Generics

import Protolude

import Servant


type HXTrigger = Header "HX-Trigger" Text
type HXRetarget = Header "HX-Retarget" Text
type HXReswap = Header "HX-Reswap" Text

data Api routes
  = Api
    { _routeGrupoPost ::
      routes :- "grupo" :> ReqBody '[JSON] CreateGrupoParams :> Post '[JSON] Grupo
    , _routeGrupoGet ::
      routes :- "grupo" :> Capture "id" ULID :> Get '[JSON] ShallowGrupo
    , _routeGrupoGetNetos ::
      routes :- "grupo" :> Capture "id" ULID :> "resumen" :> Get '[JSON] ResumenGrupo
    , _routeGrupoParticipanteAdd ::
      routes :- "grupo" :> Capture "id" ULID :> "participantes" :> ReqBody '[JSON] ParticipanteAddParams :> Post '[JSON] Participante
    , _routePagoPost ::
      routes :- "grupo" :> Capture "id" ULID :> "pagos" :> ReqBody '[JSON] Pago :> Post '[JSON] Pago
    , _routePagosGet ::
      routes :- "grupo" :> Capture "id" ULID :> "pagos" :> Get '[JSON] [ShallowPago]
    , _routePagoGet ::
      routes :- "grupo" :> Capture "id" ULID :> "pagos" :> Capture "pagoId" ULID :> Get '[JSON] Pago
    , _routePagoResumenPost ::
      routes :- "pagos" :> "resumen" :> ReqBody '[JSON] Pago :> Post '[JSON] ResumenPago
    , _routeGrupoParticipanteDelete ::
      routes :- "grupo" :> Capture "id" ULID :> "participantes" :> Capture "participanteId" ULID :> Delete '[JSON] ULID
    , _routeGrupoPagoDelete ::
      routes :- "grupo" :> Capture "id" ULID :> "pagos" :> Capture "pagoId" ULID :> Delete '[JSON] ULID
    , _routePagoUpdate ::
      routes :- "grupo" :> Capture "id" ULID :> "pagos" :> Capture "pagoId" ULID :> ReqBody '[JSON] Pago :> Put '[JSON] Pago
    -- Repartija
    -- , _routeRepartijaPost ::
    --   routes :- "grupo" :> Capture "id" ULID :> "repartijas" :> ReqBody '[JSON] Repartija :> Post '[JSON] Repartija
    , _routeRepartijaGet ::
      routes :- "repartijas" :> Capture "repartijaId" ULID :> Get '[JSON] Repartija
    , _routeRepartijaClaimPut ::
      routes :- "repartijas" :> Capture "repartijaId" ULID :> ReqBody '[JSON] RepartijaClaim :> Put '[JSON] RepartijaClaim
    , _routeRepartijaClaimDelete ::
      routes :- "repartijas" :> "claims" :> Capture "claimId" ULID :> Delete '[JSON] Text
    -- , _routeRepartijaToPago ::
    --   routes :- "repartijas" :> Capture "repartijaId" ULID :> Post '[JSON] Text
    , _routeReceiptImageParse ::
      routes :- "receipt" :> "parse-image" :> ReqBody '[JSON] ReceiptImageRequest :> Post '[JSON] ReceiptImageResponse
    , _routeHealth ::
      routes :- "health" :> Get '[JSON] Text
    -- , _routeRepartijaItemDesdoblar ::
    --   routes :- "repartijas" :> Capture "repartijaId" ULID :> "claim" :> Capture "claimId":> Post '[JSON] String
    }
  deriving (Generic)

type TheAPI routes = ToServant Api routes

data ParticipanteAddParams = ParticipanteAddParams
  { name :: Text
  } deriving (Show, Eq, Generic)

data CreateGrupoParams = CreateGrupoParams
  { grupoName :: Text
  , grupoParticipante :: Text
  } deriving (Show, Eq, Generic)

newtype Netos = Netos (Deudas Monto)
  deriving (Show, Eq, Generic)

data ResumenGrupo = ResumenGrupo
  { transaccionesParaSaldar :: [Transaccion]
  , netos :: Netos
  , cantidadPagosInvalidos :: Int
  } deriving (Show, Eq, Generic)

data ResumenPago = ResumenPago
  { resumen :: ResumenDeudas
  , resumenPagadores :: ResumenDeudas
  , resumenDeudores :: ResumenDeudas
  } deriving (Show, Eq, Generic)

data ReceiptImageRequest = ReceiptImageRequest
  { imageBase64 :: Text
  } deriving (Show, Eq, Generic)

data ParsedReceiptItem = ParsedReceiptItem
  { nombre :: Text
  , monto :: Monto
  , cantidad :: Int
  } deriving (Show, Eq, Generic)

data ReceiptImageResponse
  = ReceiptImageSuccess
      { items :: [ParsedReceiptItem]
      }
  | ReceiptImageError
      { error :: Text
      }
  deriving (Show, Eq, Generic)

Elm.deriveBoth Elm.defaultOptions ''ParticipanteAddParams
Elm.deriveBoth Elm.defaultOptions ''CreateGrupoParams
Elm.deriveBoth Elm.defaultOptions ''Netos
Elm.deriveBoth Elm.defaultOptions ''ResumenGrupo
Elm.deriveBoth Elm.defaultOptions ''ResumenPago
Elm.deriveBoth Elm.defaultOptions ''ReceiptImageRequest
Elm.deriveBoth Elm.defaultOptions ''ParsedReceiptItem
Elm.deriveBoth Elm.defaultOptions ''ReceiptImageResponse
