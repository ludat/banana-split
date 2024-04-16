{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Site.Api where



import BananaSplit (Deudas, Grupo, Monto, Pago, Participante, ParticipanteId, Transaccion)

import Data.Aeson
import Data.Text (Text)
import Data.ULID (ULID)

import Elm.Derive qualified as Elm
import Elm.Module qualified as Elm

import GHC.Generics

import Servant

import Site.HTML

import Web.FormUrlEncoded (Form)

type HXTrigger = Header "HX-Trigger" Text
type HXRetarget = Header "HX-Retarget" Text
type HXReswap = Header "HX-Reswap" Text

data Api routes
  = Api
    { _routeGrupoPost ::
      routes :- "api" :> "grupo" :> ReqBody '[JSON] CreateGrupoParams :> Post '[JSON] Grupo
    , _routeGrupoGet ::
      routes :- "api" :> "grupo" :> Capture "id" ULID :> Get '[JSON] Grupo
    , _routeGrupoGetNetos ::
      routes :- "api" :> "grupo" :> Capture "id" ULID :> "netos" :> Get '[JSON] Netos
    , _routeGrupoParticipanteAdd ::
      routes :- "api" :> "grupo" :> Capture "id" ULID :> "participantes" :> ReqBody '[JSON] ParticipanteAddParams :> Post '[JSON] Participante
    -- , _routePagosGet ::
    --   routes :- "grupo" :> Capture "id" ULID :> "pagos" :> Get '[HTML] RawHtml
    , _routePagoPost ::
      routes :- "api" :> "grupo" :> Capture "id" ULID :> "pagos" :> ReqBody '[JSON] Pago :> Post '[JSON] Pago
    -- , _routePagoNewPatch ::
    --   routes :- "grupo" :> Capture "id" ULID :> "pagos" :> QueryParam "pagoId" ULID :> ReqBody '[FormUrlEncoded] Form :> Patch '[HTML] RawHtml
    -- , _routeGrupoPagoAdd ::
    --   routes :- "grupo" :> Capture "id" ULID :> "pagos" :> ReqBody '[FormUrlEncoded] Form :> Post '[HTML] (Headers '[HXTrigger, HXRetarget, HXReswap] RawHtml)
    , _routeGrupoParticipanteDelete ::
      routes :- "api" :> "grupo" :> Capture "id" ULID :> "participantes" :> Capture "participanteId" ULID :> Delete '[JSON] ULID
    , _routeGrupoPagoDelete ::
      routes :- "api" :> "grupo" :> Capture "id" ULID :> "pagos" :> Capture "pagoId" ULID :> Delete '[JSON] ULID
    -- , _routePagoEdit ::
    --   routes :- "grupo" :> Capture "id" ULID :> "pagos" :> Capture "pagoId" ULID :> Get '[HTML] RawHtml
    -- , _routePagoUpdate ::
    --   routes :- "grupo" :> Capture "id" ULID :> "pagos" :> Capture "pagoId" ULID :> ReqBody '[FormUrlEncoded] Form :> Put '[HTML] (Headers '[HXTrigger, HXRetarget, HXReswap] RawHtml)
    -- , _routeStatic ::
    --   routes :- Raw
    }
  deriving (Generic)

type TheAPI routes = ToServant Api routes

data ParticipanteAddParams = ParticipanteAddParams
  { name :: Text
  } deriving (Show, Eq, Generic)

data CreateGrupoParams = CreateGrupoParams
  { grupoName :: Text
  } deriving (Show, Eq, Generic)

data Netos = Netos
  { transaccionesParaSaldar :: [Transaccion]
  , netos :: Deudas Monto
  } deriving (Show, Eq, Generic)

Elm.deriveBoth Elm.defaultOptions ''ParticipanteAddParams
Elm.deriveBoth Elm.defaultOptions ''CreateGrupoParams
Elm.deriveBoth Elm.defaultOptions ''Netos
