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
import Site.Auth (Session)

type HXTrigger = Header "HX-Trigger" Text

type HXRetarget = Header "HX-Retarget" Text

type HXReswap = Header "HX-Reswap" Text

data Api routes
  = Api
  { _routeGrupoPost ::
      routes :- "grupo" :> ReqBody '[JSON] CreateGrupoParams :> Post '[JSON] Grupo
  , _routeMeGrupoPost ::
      routes :- AuthProtect User :> "me" :> "grupos" :> ReqBody '[JSON] CreateGrupoAsUserParams :> Post '[JSON] Grupo
  , _routeGrupoGet ::
      routes :- "grupo" :> Capture "id" ULID :> Get '[JSON] Grupo
  , _routeGrupoGetNetos ::
      routes :- "grupo" :> Capture "id" ULID :> "resumen" :> Get '[JSON] ResumenGrupo
  , _routeGrupoUpdate ::
      routes :- "grupo" :> Capture "id" ULID :> ReqBody '[JSON] UpdateGrupoParams :> Put '[JSON] Grupo
  , _routeGrupoParticipanteAdd ::
      routes :- "grupo" :> Capture "id" ULID :> "participantes" :> ReqBody '[JSON] ParticipanteAddParams :> Post '[JSON] Participante
  , _routeGrupoParticipanteUpdate ::
      routes :- "grupo" :> Capture "id" ULID :> "participantes" :> Capture "participanteId" ULID :> ReqBody '[JSON] ParticipanteUpdateParams :> Put '[JSON] Participante
  , _routePagoPost ::
      routes :- "grupo" :> Capture "id" ULID :> "pagos" :> ReqBody '[JSON] Pago :> Post '[JSON] Pago
  , -- Con @participante@ el resumen de cada gasto viene recortado a esa
    -- persona. La lista sólo muestra lo propio, así que es la diferencia entre
    -- mandar un par de netos por gasto o los de todo el grupo.
    _routePagosGet ::
      routes
        :- "grupo"
          :> Capture "id" ULID
          :> "pagos"
          :> QueryParam "participante" ULID
          :> Get '[JSON] [ShallowPago]
  , _routePagoGet ::
      routes :- "grupo" :> Capture "id" ULID :> "pagos" :> Capture "pagoId" ULID :> Get '[JSON] Pago
  , _routePagoResumenPost ::
      routes :- "pagos" :> "resumen" :> ReqBody '[JSON] Pago :> Post '[JSON] ResumenPago
  , _routeGrupoParticipanteDelete ::
      routes :- "grupo" :> Capture "id" ULID :> "participantes" :> Capture "participanteId" ULID :> Delete '[JSON] DeleteParticipanteResult
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
      routes :- "repartijas" :> Capture "repartijaId" ULID :> ReqBody '[JSON] RepartijaClaim :> "claims" :> Put '[JSON] RepartijaForFrontend
  , _routeRepartijaClaimDelete ::
      routes :- "repartijas" :> "claims" :> Capture "claimId" ULID :> Delete '[JSON] RepartijaForFrontend
  , _routeRepartijaAccionPreview ::
      routes
        :- "repartijas"
          :> "acciones"
          :> "preview"
          :> ReqBody '[JSON] PedidoDePreviewDeAccion
          :> Post '[JSON] ResultadoPreviewDeAccion
  , _routeRepartijaAccion ::
      routes
        :- "repartijas"
          :> Capture "repartijaId" ULID
          :> "acciones"
          :> ReqBody '[JSON] AccionDeRepartija
          :> Post '[JSON] ResultadoAccionDeRepartija
  , -- , _routeRepartijaToPago ::
    --   routes :- "repartijas" :> Capture "repartijaId" ULID :> Post '[JSON] Text
    _routeGrupoFreeze ::
      routes :- "grupo" :> Capture "id" ULID :> "freeze" :> Post '[JSON] Grupo
  , _routeGrupoUnfreeze ::
      routes :- "grupo" :> Capture "id" ULID :> "freeze" :> Delete '[JSON] Grupo
  , _routeGrupoTasasDeCambioPut ::
      routes :- "grupo" :> Capture "id" ULID :> "tasas-de-cambio" :> Capture "moneda" Moneda :> ReqBody '[JSON] [TasaDeCambio] :> Put '[JSON] [TasaDeCambio]
  , -- Hechas y pendientes, en orden de creación.
    _routeTransferenciasGet ::
      routes :- "grupo" :> Capture "id" ULID :> "transferencias" :> Get '[JSON] [Transferencia]
  , _routeTransferenciaSaldar ::
      routes :- "grupo" :> Capture "id" ULID :> "transferencias" :> Capture "transferenciaId" ULID :> "saldar" :> Post '[JSON] Transferencia
  , _routeTransferenciaDesmarcar ::
      routes :- "grupo" :> Capture "id" ULID :> "transferencias" :> Capture "transferenciaId" ULID :> "saldar" :> Delete '[JSON] Transferencia
  , _routeTransferenciaPost ::
      routes :- "grupo" :> Capture "id" ULID :> "transferencias" :> ReqBody '[JSON] NuevaTransferenciaParams :> Post '[JSON] Transferencia
  , _routeTransferenciaUpdate ::
      routes :- "grupo" :> Capture "id" ULID :> "transferencias" :> Capture "transferenciaId" ULID :> ReqBody '[JSON] NuevaTransferenciaParams :> Put '[JSON] Transferencia
  , _routeTransferenciaDelete ::
      routes :- "grupo" :> Capture "id" ULID :> "transferencias" :> Capture "transferenciaId" ULID :> Delete '[JSON] ULID
  , _routeReceiptImageParse ::
      routes :- "receipt" :> "parse-image" :> ReqBody '[JSON] ReceiptImageRequest :> Post '[JSON] ReceiptImageResponse
  , -- Auth. A single email-first flow: request a code, prove ownership by
    -- verifying it, then either log in (existing account) or register (new one).
    _routeAuthRequestCode ::
      routes :- "auth" :> "request-code" :> ReqBody '[JSON] RequestCodeParams :> Post '[JSON] LoginChallenge
  , _routeAuthVerify ::
      routes :- "auth" :> "verify" :> ReqBody '[JSON] VerifyParams :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] VerifyResult)
  , _routeAuthRegister ::
      routes :- "auth" :> "register" :> ReqBody '[JSON] RegisterParams :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] User)
  , _routeAuthLogout ::
      routes :- "auth" :> "logout" :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] Text)
  , -- Sliding session: called on every app load; re-issues the cookie only when
    -- the token is past half its life, otherwise just returns the user.
    _routeAuthRefresh ::
      routes :- AuthProtect Session :> "auth" :> "refresh" :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] User)
  , _routeMe ::
      routes :- AuthProtect User :> "me" :> Get '[JSON] User
  , _routeMeUpdate ::
      routes :- AuthProtect User :> "me" :> ReqBody '[JSON] UpdateMeParams :> Put '[JSON] User
  , -- The grupos where the signed-in user has claimed a participante, i.e.
    -- "my groups" for the home screen.
    _routeMeGruposGet ::
      routes :- AuthProtect User :> "me" :> "grupos" :> Get '[JSON] [GrupoParaUsuario]
  , _routeParticipanteClaim ::
      routes :- AuthProtect User :> "grupo" :> Capture "id" ULID :> "participantes" :> Capture "participanteId" ULID :> "claim" :> Put '[JSON] ClaimParticipanteResult
  , -- "I'm new here": add a participante for the signed-in user, already
    -- claimed. Refused like a claim when the user already owns one.
    _routeParticipanteAddMe ::
      routes :- AuthProtect User :> "grupo" :> Capture "id" ULID :> "participantes" :> "me" :> ReqBody '[JSON] ParticipanteAddParams :> Post '[JSON] ClaimParticipanteResult
  , _routeParticipanteUnclaim ::
      routes :- AuthProtect User :> "grupo" :> Capture "id" ULID :> "participantes" :> Capture "participanteId" ULID :> "claim" :> Delete '[JSON] Participante
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

data ParticipanteUpdateParams = ParticipanteUpdateParams
  { nombre :: Text
  , datosDeCobro :: Maybe Text
  }
  deriving (Show, Eq, Generic)

-- | Step 1: ask for a login code by email. The same request whether or not an
-- account exists, so it never reveals which — existence is only disclosed at
-- verify time, to the proven owner of the address.
data RequestCodeParams = RequestCodeParams
  { email :: Email
  }
  deriving (Show, Eq, Generic)

-- | Returned by @request-code@: the client holds this challenge and later
-- submits it back together with the emailed code. It commits to the code
-- without containing it (see "Site.Auth"), so it is safe to hand to the browser.
data LoginChallenge = LoginChallenge
  { challenge :: Text
  }
  deriving (Show, Eq, Generic)

data VerifyParams = VerifyParams
  { challenge :: Text
  , code :: Text
  }
  deriving (Show, Eq, Generic)

-- | Step 2 outcome, once email ownership is proven: either the account already
-- existed (now logged in — a session cookie rides on the response) or it does
-- not yet, in which case the caller gets a short-lived registration token to
-- exchange for a new account (see 'RegisterParams'). Delivered as a 200 so the
-- typed branch reaches the frontend (same pattern as 'ClaimParticipanteResult').
data VerifyResult
  = VerifyLoggedIn User
  | VerifyNeedsRegistration Text
  deriving (Show, Eq, Generic)

-- | Step 3 (new accounts only): exchange the registration token from
-- 'VerifyNeedsRegistration' plus a chosen display name for a created account.
data RegisterParams = RegisterParams
  { registrationToken :: Text
  , nombre :: Text
  }
  deriving (Show, Eq, Generic)

-- | Editable account details. Email is the login identity (changing it would
-- need the ownership-proof flow), so only the display name is editable here.
data UpdateMeParams = UpdateMeParams
  { nombre :: Text
  , datosDeCobro :: Maybe Text
  }
  deriving (Show, Eq, Generic)

data CreateGrupoParams = CreateGrupoParams
  { grupoName :: Text
  , grupoParticipante :: Text
  }
  deriving (Show, Eq, Generic)

-- | Params to create a grupo as the signed-in user ('_routeMeGrupoPost'): the
-- seeded participante comes from the account, so only the grupo name travels.
data CreateGrupoAsUserParams = CreateGrupoAsUserParams
  { grupoName :: Text
  }
  deriving (Show, Eq, Generic)

data UpdateGrupoParams = UpdateGrupoParams
  { nombre :: Text
  , monedaPorDefecto :: Moneda
  }
  deriving (Show, Eq, Generic)

-- | Las transferencias no viajan acá: tienen su propio endpoint, así que
-- saldar una no obliga a volver a sumar los netos de todos los gastos.
data ResumenGrupo
  = GrupoAbierto ResumenAbierto
  | -- | Congelado ya no hay netos que mostrar: las deudas quedaron fijadas en
    -- las transferencias.
    GrupoCongelado
  deriving (Show, Eq, Generic)

data ResumenAbierto = ResumenAbierto
  { netos :: PorMoneda (Netos Monto)
  -- ^ Incluyen las transferencias hechas.
  , consolidado :: ConsolidadoNetos
  , cantidadPagos :: Int
  , cantidadPagosInvalidos :: Int
  }
  deriving (Show, Eq, Generic)

data NuevaTransferenciaParams = NuevaTransferenciaParams
  { from :: ParticipanteId
  , to :: ParticipanteId
  , monto :: Monto
  , moneda :: Moneda
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

-- | The outcome of a claim ("this is me") on a participante. Delivered as a 200
-- so the typed rejection reaches the frontend (see 'ReceiptImageResponse' for
-- the same success|error pattern).
data ClaimParticipanteResult
  = ClaimAccepted Participante
  | ClaimRejected ParticipanteClaimError
  deriving (Show, Eq, Generic)

-- | Same pattern as 'ClaimParticipanteResult': a rejected delete says what still
-- references the participante, so it has to reach the frontend as a 200.
data DeleteParticipanteResult
  = DeleteAccepted ULID
  | DeleteRejected ParticipanteError
  deriving (Show, Eq, Generic)

-- | Same pattern as 'ClaimParticipanteResult': an action that can't be applied
-- says why, so it has to reach the frontend as a 200.
data ResultadoAccionDeRepartija
  = AccionDeRepartijaAceptada RepartijaForFrontend
  | AccionDeRepartijaRechazada AccionDeRepartijaInvalida
  deriving (Show, Eq, Generic)

data PedidoDePreviewDeAccion = PedidoDePreviewDeAccion
  { repartija :: Repartija
  , accion :: AccionDeRepartija
  }
  deriving (Show, Eq, Generic)

data PreviewDeAccion = PreviewDeAccion
  { repartija :: Repartija
  , detalle :: DetalleRepartija
  }
  deriving (Show, Eq, Generic)

data ResultadoPreviewDeAccion
  = PreviewDeAccionAceptada PreviewDeAccion
  | PreviewDeAccionRechazada AccionDeRepartijaInvalida
  deriving (Show, Eq, Generic)

Elm.deriveBoth Elm.defaultOptions ''ParticipanteAddParams
Elm.deriveBoth Elm.defaultOptions ''ParticipanteUpdateParams
Elm.deriveBoth Elm.defaultOptions ''RequestCodeParams
Elm.deriveBoth Elm.defaultOptions ''LoginChallenge
Elm.deriveBoth Elm.defaultOptions ''VerifyParams
Elm.deriveBoth Elm.defaultOptions ''VerifyResult
Elm.deriveBoth Elm.defaultOptions ''RegisterParams
Elm.deriveBoth Elm.defaultOptions ''UpdateMeParams
Elm.deriveBoth Elm.defaultOptions ''CreateGrupoParams
Elm.deriveBoth Elm.defaultOptions ''CreateGrupoAsUserParams
Elm.deriveBoth Elm.defaultOptions ''UpdateGrupoParams
Elm.deriveBoth Elm.defaultOptions ''ResumenAbierto

Elm.deriveBoth Elm.defaultOptions ''ResumenGrupo
Elm.deriveBoth Elm.defaultOptions ''NuevaTransferenciaParams
Elm.deriveBoth Elm.defaultOptions ''ResumenPago
Elm.deriveBoth Elm.defaultOptions ''ReceiptImageRequest
Elm.deriveBoth Elm.defaultOptions ''ReceiptImageResponse
Elm.deriveBoth Elm.defaultOptions ''ClaimParticipanteResult
Elm.deriveBoth Elm.defaultOptions ''DeleteParticipanteResult
Elm.deriveBoth Elm.defaultOptions ''ResultadoAccionDeRepartija
Elm.deriveBoth Elm.defaultOptions ''PedidoDePreviewDeAccion
Elm.deriveBoth Elm.defaultOptions ''PreviewDeAccion
Elm.deriveBoth Elm.defaultOptions ''ResultadoPreviewDeAccion
