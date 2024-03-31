{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Site.Api where



import Data.Text (Text)
import Data.ULID (ULID)

import GHC.Generics

import Servant

import Site.HTML

import Web.FormUrlEncoded (Form)

type HXTrigger = Header "HX-Trigger" Text
type HXRetarget = Header "HX-Retarget" Text
type HXReswap = Header "HX-Reswap" Text

data Api routes
  = Api
    { _routeIndex ::
      routes :- Get '[HTML] RawHtml
    , _routeGrupoPost ::
      routes :- "grupo" :> ReqBody '[FormUrlEncoded] Form :> Post '[HTML] RawHtml
    , _routeGrupoGet ::
      routes :- "grupo" :> Capture "id" ULID :> Get '[HTML] RawHtml
    , _routeGrupoParticipantesShow ::
      routes :- "grupo" :> Capture "id" ULID :> "participantes" :> Get '[HTML] RawHtml
    , _routeGrupoParticipanteAdd ::
      routes :- "grupo" :> Capture "id" ULID :> "participantes" :> ReqBody '[FormUrlEncoded] Form :> Post '[HTML] RawHtml
    , _routePagosGet ::
      routes :- "grupo" :> Capture "id" ULID :> "pagos" :> Get '[HTML] RawHtml
    , _routePagoNew ::
      routes :- "grupo" :> Capture "id" ULID :> "pagos" :> "new" :> Get '[HTML] RawHtml
    , _routePagoNewPatch ::
      routes :- "grupo" :> Capture "id" ULID :> "pagos" :> QueryParam "pagoId" ULID :> ReqBody '[FormUrlEncoded] Form :> Patch '[HTML] RawHtml
    , _routeGrupoPagoAdd ::
      routes :- "grupo" :> Capture "id" ULID :> "pagos" :> ReqBody '[FormUrlEncoded] Form :> Post '[HTML] (Headers '[HXTrigger, HXRetarget, HXReswap] RawHtml)
    , _routePagoDelete ::
      routes :- "grupo" :> Capture "id" ULID :> "pagos" :> Capture "pagoId" ULID :> Delete '[HTML] (Headers '[HXTrigger] RawHtml)
    , _routePagoEdit ::
      routes :- "grupo" :> Capture "id" ULID :> "pagos" :> Capture "pagoId" ULID :> Get '[HTML] RawHtml
    , _routePagoUpdate ::
      routes :- "grupo" :> Capture "id" ULID :> "pagos" :> Capture "pagoId" ULID :> ReqBody '[FormUrlEncoded] Form :> Put '[HTML] (Headers '[HXTrigger, HXRetarget, HXReswap] RawHtml)
    , _routeStatic ::
      routes :- "static" :> Raw
    }
  deriving (Generic)

type TheAPI routes = ToServant Api routes
