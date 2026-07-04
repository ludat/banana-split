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
import Site.Auth (AuthContext, authHandler)
import Site.Handler.Auth
import Site.Handler.Grupos
import Site.Handler.Pagos
import Site.Handler.Receipt
import Site.Handler.Repartijas
import Site.Types

serverT :: ServerT ("api" :> ToServantApi Api :<|> Raw) AppHandler
serverT =
  genericServerT
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
      , _routeAuthLogin = handleLogin
      , _routeAuthVerify = handleVerify
      , _routeAuthLogout = handleLogout
      , _routeMe = handleMe
      , _routeHealth = pure "ok"
      -- , _routePagoNew = handlePagoNew
      -- , _routePagoNewPatch = handlePagoNewPatch
      -- , _routePagoEdit = handlePagoEdit
      -- , _routePagoDelete = handlePagoDelete
      }
    :<|> serveDirectoryWith
      (defaultWebAppSettings "./public")
        { ss404Handler = Just $ \_req cb -> do
            cb $ responseFile ok200 [("Content-Type", "text/html")] "./public/index.html" Nothing
        }

proxyApi :: Proxy ("api" :> ToServantApi Api :<|> Raw)
proxyApi = Proxy

nt :: App -> AppHandler a -> Handler a
nt s x = runReaderT x s

authContext :: App -> Context AuthContext
authContext appState = authHandler appState :. EmptyContext

app :: App -> Application
app appState =
  serveWithContext proxyApi (authContext appState) $
    hoistServerWithContext
      proxyApi
      (Proxy :: Proxy AuthContext)
      (nt appState)
      serverT
