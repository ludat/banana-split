{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Site.Api where

import Elm.Derive qualified as Elm
import GHC.Generics
import Protolude
import Servant

import BananaSplit

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
  , _routeGrupoUpdate ::
      routes :- "grupo" :> Capture "id" ULID :> ReqBody '[JSON] UpdateGrupoParams :> Put '[JSON] ShallowGrupo
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
  , -- Repartija
    -- , _routeRepartijaPost ::
    --   routes :- "grupo" :> Capture "id" ULID :> "repartijas" :> ReqBody '[JSON] Repartija :> Post '[JSON] Repartija
    _routeRepartijaGet ::
      routes :- "repartijas" :> Capture "repartijaId" ULID :> Get '[JSON] RepartijaForFrontend
  , _routeRepartijaClaimPut ::
      routes :- "repartijas" :> Capture "repartijaId" ULID :> ReqBody '[JSON] RepartijaClaim :> Put '[JSON] RepartijaClaim
  , _routeRepartijaClaimDelete ::
      routes :- "repartijas" :> "claims" :> Capture "claimId" ULID :> Delete '[JSON] Text
  , -- , _routeRepartijaToPago ::
    --   routes :- "repartijas" :> Capture "repartijaId" ULID :> Post '[JSON] Text
    _routeGrupoFreeze ::
      routes :- "grupo" :> Capture "id" ULID :> "freeze" :> Post '[JSON] ShallowGrupo
  , _routeGrupoUnfreeze ::
      routes :- "grupo" :> Capture "id" ULID :> "freeze" :> Delete '[JSON] ShallowGrupo
  , _routeGrupoSaldarTransaccion ::
      routes :- "grupo" :> Capture "id" ULID :> "transacciones-congeladas" :> Capture "transaccionId" ULID :> "saldar" :> ReqBody '[JSON] Pago :> Post '[JSON] Pago
  , _routeReceiptImageParse ::
      routes :- "receipt" :> "parse-image" :> ReqBody '[JSON] ReceiptImageRequest :> Post '[JSON] ReceiptImageResponse
  , -- Auth
    _routeAuthLogin ::
      routes :- "auth" :> "login" :> ReqBody '[JSON] LoginParams :> Post '[JSON] LoginChallenge
  , _routeAuthVerify ::
      routes :- "auth" :> "verify" :> ReqBody '[JSON] VerifyParams :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] User)
  , _routeAuthLogout ::
      routes :- "auth" :> "logout" :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] Text)
  , _routeMe ::
      routes :- AuthProtect "session" :> "me" :> Get '[JSON] User
  , _routeHealth ::
      routes :- "health" :> Get '[JSON] Text
      -- , _routeRepartijaItemDesdoblar ::
      --   routes :- "repartijas" :> Capture "repartijaId" ULID :> "claim" :> Capture "claimId":> Post '[JSON] String
  }
  deriving (Generic)

type TheAPI routes = ToServant Api routes

data ParticipanteAddParams = ParticipanteAddParams
  { name :: Text
  }
  deriving (Show, Eq, Generic)

data LoginParams = LoginParams
  { email :: Text
  }
  deriving (Show, Eq, Generic)

-- | Returned by @login@: the client holds this challenge and later submits it
-- back together with the emailed code. It commits to the code without
-- containing it (see "Site.Auth"), so it is safe to hand to the browser.
data LoginChallenge = LoginChallenge
  { challenge :: Text
  }
  deriving (Show, Eq, Generic)

data VerifyParams = VerifyParams
  { challenge :: Text
  , code :: Text
  }
  deriving (Show, Eq, Generic)

data CreateGrupoParams = CreateGrupoParams
  { grupoName :: Text
  , grupoParticipante :: Text
  }
  deriving (Show, Eq, Generic)

data UpdateGrupoParams = UpdateGrupoParams
  { nombre :: Text
  , monedaPorDefecto :: Moneda
  }
  deriving (Show, Eq, Generic)

data ResumenGrupo = ResumenGrupo
  { transaccionesParaSaldar :: PorMoneda [Transaccion]
  , netos :: PorMoneda (Netos Monto)
  , cantidadPagosInvalidos :: Int
  , cantidadPagos :: Int
  , isFrozen :: Bool
  }
  deriving (Show, Eq, Generic)

data ResumenPago = ResumenPago
  { resumen :: ResumenNetos
  , resumenPagadores :: ResumenNetos
  , resumenDeudores :: ResumenNetos
  }
  deriving (Show, Eq, Generic)

data ReceiptImageRequest = ReceiptImageRequest
  { imageBase64 :: Text
  }
  deriving (Show, Eq, Generic)

data ReceiptImageResponse
  = ReceiptImageSuccess
      { items :: [RepartijaItem]
      }
  | ReceiptImageError
      { error :: Text
      }
  deriving (Show, Eq, Generic)

Elm.deriveBoth Elm.defaultOptions ''ParticipanteAddParams
Elm.deriveBoth Elm.defaultOptions ''LoginParams
Elm.deriveBoth Elm.defaultOptions ''LoginChallenge
Elm.deriveBoth Elm.defaultOptions ''VerifyParams
Elm.deriveBoth Elm.defaultOptions ''CreateGrupoParams
Elm.deriveBoth Elm.defaultOptions ''UpdateGrupoParams
Elm.deriveBoth Elm.defaultOptions ''ResumenGrupo
Elm.deriveBoth Elm.defaultOptions ''ResumenPago
Elm.deriveBoth Elm.defaultOptions ''ReceiptImageRequest
Elm.deriveBoth Elm.defaultOptions ''ReceiptImageResponse
