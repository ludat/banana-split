{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Server (
  app,
  serverT,
) where

import Control.Monad.Reader
import Network.HTTP.Types.Status (ok200)
import Network.Wai
import Network.Wai.Application.Static (defaultWebAppSettings)
import Protolude hiding (Handler)
import Servant
import Servant.Server.Generic
import WaiAppStatic.Types (StaticSettings (..))

import Site.Api
import Site.Auth (AuthContext, authHandler, sessionAuthHandler)
import Site.Handler.Auth
import Site.Handler.Grupos
import Site.Handler.InboundEmail (WebhookApi, handleInboundEmail)
import Site.Handler.Pagos
import Site.Handler.Receipt
import Site.Handler.Repartijas
import Site.Types

-- | The full HTTP surface: the Elm-facing 'Api' plus the provider-facing
-- 'WebhookApi' (kept separate so servant-elm never sees it), both under @\/api@,
-- and the static site as a fallback.
type ServedApi =
  "api" :> (ToServantApi Api :<|> WebhookApi)
    :<|> Raw

serverT :: ServerT ServedApi AppHandler
serverT =
  ( genericServerT
      Api
        { _routeGrupoGet = handleShowGrupo
        , _routeGrupoPost = handleCreateGrupo
        , _routeGrupoGetNetos = handleGetNetos
        , _routeGrupoUpdate = handleUpdateGrupo
        , _routeGrupoParticipanteAdd = handleCreateParticipante
        , _routeGrupoParticipanteDelete = handleDeleteParticipante
        , _routeGrupoPagoDelete = handleDeletePago
        , _routePagoPost = handlePagoPost
        , _routePagoGet = handlePagoGet
        , _routePagosGet = handlePagosGet
        , _routePagoResumenPost = handlePagoResumenPost
        , -- , _routePagosGet = handlePagosGet
          -- , _routeGrupoPagoAdd = handlePagoCreate
          _routePagoUpdate = handlePagoUpdate
        , -- , _routeRepartijaPost = handleRepartijaPost
          _routeRepartijaGet = handleRepartijaGet
        , _routeRepartijaClaimPut = handleRepartijaClaimPut
        , _routeRepartijaClaimDelete = handleRepartijaClaimDelete
        , _routeGrupoFreeze = handleFreezeGrupo
        , _routeGrupoUnfreeze = handleUnfreezeGrupo
        , _routeGrupoSaldarTransaccion = handleSaldarTransaccion
        , _routeReceiptImageParse = handleReceiptImageParse
        , _routeAuthRequestCode = handleRequestCode
        , _routeAuthVerify = handleVerify
        , _routeAuthRegister = handleRegister
        , _routeAuthLogout = handleLogout
        , _routeAuthRefresh = handleRefresh
        , _routeMe = handleMe
        , _routeMeUpdate = handleUpdateMe
        , _routeMeGrupoPost = handleCreateGrupoAsUser
        , _routeMeGruposGet = handleGetMisGrupos
        , _routeParticipanteClaim = handleClaimParticipante
        , _routeParticipanteUnclaim = handleUnclaimParticipante
        , _routeHealth = pure "ok"
        -- , _routePagoNew = handlePagoNew
        -- , _routePagoNewPatch = handlePagoNewPatch
        -- , _routePagoEdit = handlePagoEdit
        -- , _routePagoDelete = handlePagoDelete
        }
      :<|> handleInboundEmail
  )
    :<|> serveDirectoryWith
      (defaultWebAppSettings "./public")
        { ss404Handler = Just $ \_req cb -> do
            cb $ responseFile ok200 [("Content-Type", "text/html")] "./public/index.html" Nothing
        }

proxyApi :: Proxy ServedApi
proxyApi = Proxy

-- | Run a handler down to a real response, catching anything it throws.
-- Servant only turns 'throwError'd 'ServerError's into a response; a plain
-- Haskell exception (e.g. the UTF-8 decode that used to crash receipt
-- parsing) otherwise vanishes into whatever Warp's 'onException' does with
-- it, with no trace of what actually broke. Catching here, before the
-- action is handed back into Servant's own dispatch, guarantees every
-- unhandled exception gets logged with its message instead of disappearing.
nt :: App -> AppHandler a -> Handler a
nt s x = do
  result <- liftIO $ try $ runHandler (runReaderT x s)
  case result of
    Right (Right a) -> pure a
    Right (Left err) -> throwError err
    Left (e :: SomeException) -> do
      liftIO $ putText $ "[handler] unhandled exception: " <> show e
      throwError err500

authContext :: App -> Context AuthContext
authContext appState = authHandler appState :. sessionAuthHandler appState :. EmptyContext

app :: App -> Application
app appState =
  serveWithContext proxyApi (authContext appState) $
    hoistServerWithContext
      proxyApi
      (Proxy :: Proxy AuthContext)
      (nt appState)
      serverT
