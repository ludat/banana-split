{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Server where

import Control.Monad.Reader



import Network.Wai

import Servant
import Servant.Server.Generic


import Types
import Site.Api
import Site.Handler.Grupos
import Site.Handler.Pagos

serverT :: Api (AsServerT AppHandler)
serverT =
  Api
    { _routeIndex = handleIndex
    , _routeGrupoGet = handleShowGrupo
    , _routeGrupoPost = handleCreateGrupo
    , _routeGrupoParticipanteAdd = handleCreateParticipante
    , _routeGrupoParticipantesShow = handleShowParticipantes

    , _routePagosGet = handlePagosGet
    , _routeGrupoPagoAdd = handlePagoCreate
    , _routePagoUpdate = handlePagoUpdate
    , _routePagoNew = handlePagoNew
    , _routePagoNewPatch = handlePagoNewPatch
    , _routePagoEdit = handlePagoEdit
    , _routePagoDelete = handlePagoDelete

    , _routeStatic =
      -- serveDirectoryWebApp "./public"
      serveDirectoryFileServer "./public"
    }

nt :: App -> AppHandler a -> Handler a
nt s x = runReaderT x s

app :: App -> Application
app appState =
  genericServeTWithContext
    (nt appState)
    serverT
    EmptyContext
