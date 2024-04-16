{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Server where

import Control.Monad.Reader

import Data.FileEmbed

import Network.Wai

import Servant
import Servant.Server.Generic

import Site.Api
import Site.Handler.Grupos
import Site.Handler.Pagos

import Types

serverT :: ServerT (ToServantApi Api :<|> Raw) AppHandler
serverT =
  genericServerT Api
    { _routeGrupoGet = handleShowGrupo
    , _routeGrupoPost = handleCreateGrupo
    , _routeGrupoGetNetos = handleGetNetos

    , _routeGrupoParticipanteAdd = handleCreateParticipante
    , _routeGrupoParticipanteDelete = handleDeleteParticipante
    , _routeGrupoPagoDelete = handleDeletePago

    , _routePagoPost = handlePagoPost

    -- , _routePagosGet = handlePagosGet
    -- , _routeGrupoPagoAdd = handlePagoCreate
    -- , _routePagoUpdate = handlePagoUpdate
    -- , _routePagoNew = handlePagoNew
    -- , _routePagoNewPatch = handlePagoNewPatch
    -- , _routePagoEdit = handlePagoEdit
    -- , _routePagoDelete = handlePagoDelete
    } :<|> serveDirectoryWebApp "/public"

proxyApi :: Proxy (ToServantApi Api :<|> Raw)
proxyApi = Proxy

nt :: App -> AppHandler a -> Handler a
nt s x = runReaderT x s

app :: App -> Application
app appState =
  serve proxyApi $ hoistServer proxyApi (nt appState) serverT
