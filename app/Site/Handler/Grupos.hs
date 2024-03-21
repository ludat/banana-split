{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Site.Handler.Grupos
  ( handleCreateGrupo
  , handleCreateParticipante
  , handleIndex
  , handleShowGrupo
  , handleShowParticipantes
  ) where

import Control.Monad.Reader

import Data.Function ((&))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import GHC.Generics


import Servant

import Site.Handler.Utils (orElseMay, redirect, throwHtml, htmlLayout, renderHtml, postForm, orElse)
import Lucid
import Lucid.Htmx

import Types
import Site.HTML
import BananaSplit.Persistence (createGrupo, fetchGrupo, addParticipante)
import BananaSplit
import Web.FormUrlEncoded (FromForm (..), Form)
import Data.ULID (ULID)
import Database.Selda.Backend
import Database.Selda.PostgreSQL (PG)
import Text.Digestive qualified as Digestive
import Data.Text qualified as Text
import Site.Api
import Site.Layout (navBarItemsForGrupo)
import Control.Monad (forM_)
import Lucid.Base (makeAttributes)

handleIndex :: AppHandler RawHtml
handleIndex = do
  view <- Digestive.getForm "grupo" createGrupoForm

  renderHtml $ htmlLayout mempty $ do
    main_ [class_ "container"] $ createGrupoView view

handleCreateGrupo :: Form -> AppHandler RawHtml
handleCreateGrupo form = do
  (view, maybeGrupoParams) <- postForm "grupo" form createGrupoForm

  case maybeGrupoParams of
    Just (CreateGrupoParams{name}) -> do
      grupo <- runSelda (createGrupo name)

      redirect $ encodeUtf8 $ ("/" <>) $ toUrlPiece $ fieldLink _routeGrupoGet grupo.grupoId

    Nothing -> do
      renderHtml $ createGrupoView view

newtype CreateGrupoParams = CreateGrupoParams
  { name :: Text
  } deriving (Show, Eq, Generic)

createGrupoForm :: Monad m => Digestive.Form Text m CreateGrupoParams
createGrupoForm =
  CreateGrupoParams
  <$> "name" Digestive..: (
    Digestive.text Nothing
    & Digestive.check "No puede ser vacio" (not . Text.null)
  )

createGrupoView :: Monad m => Digestive.View Text -> HtmlT m ()
createGrupoView view = do
  form_
    [ hxPost_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeGrupoPost
    ] $ do
    fieldset_ $ do
      label_ $ do
        "Nombre"
        input_
          [ value_ $ Digestive.fieldInputText "name" view
          , name_ $ Digestive.absoluteRef "name" view
          , id_ $ Digestive.absoluteRef "name" view
          , type_ "text"
          , placeholder_ "After del viernes"
          , if Digestive.errors "name" view & null
            then mempty
            else makeAttributes "aria-invalid" "true"
          ]

        forM_ (Digestive.errors "name" view) $ \t -> do
          small_ $ toHtml t
    button_ "Crear nuevo grupo"

handleShowGrupo :: ULID -> AppHandler RawHtml
handleShowGrupo grupoId = do
  grupo <- runSelda (fetchGrupo grupoId)
    `orElseMay` throwHtml (h1_ $ toHtml @Text "grupo no encontrado")

  renderHtml $ htmlLayout (navBarItemsForGrupo grupo) $ do
      section_ [id_ "pagos"] $ do
        h3_ "Netos:"
        forM_ (calcularDeudasTotales grupo & deudasToPairs) $ \(participante, monto) ->
          p_ $ do
            toHtml $ buscarParticipante grupo participante & participanteNombre
            ": "
            toHtml monto

        h3_ "Transferencias para saldar deudas"
        forM_ (grupo & calcularDeudasTotales & resolverDeudasNaif) $ \(Transaccion pagador deudor monto) -> do
          p_ $ do
            toHtml $ buscarParticipante grupo pagador & participanteNombre
            " -> "
            toHtml monto
            " -> "
            toHtml $ buscarParticipante grupo deudor & participanteNombre

handleCreateParticipante :: ULID -> Form -> AppHandler RawHtml
handleCreateParticipante grupoId form = do
  (view, maybeAddParticipante) <- postForm "participante" form newParticipanteForm

  case maybeAddParticipante of
    Just (ParticipanteAddParams {name}) -> do
      participante <- runSelda (addParticipante grupoId name)
        `orElse` (\_e -> throwHtml $ newParticipanteView grupoId view)
      view' <- Digestive.getForm "participante" newParticipanteForm
      renderHtml $ do
        newParticipanteView grupoId view'
        div_
          [ hxSwapOob_ "beforeend:#participantes"
          ] $ do
          p_ $ toHtml participante.participanteNombre
    Nothing -> do
      renderHtml $ do
        newParticipanteView grupoId view

handleShowParticipantes :: ULID -> AppHandler RawHtml
handleShowParticipantes grupoId = do
  grupo <- runSelda (fetchGrupo grupoId)
    `orElseMay` throwHtml "ese grupo no existe lol"

  view <- Digestive.getForm "participante" newParticipanteForm

  renderHtml $ htmlLayout (navBarItemsForGrupo grupo) $ do
    main_ [class_ "container"] $ newParticipanteView grupoId view
    div_ [id_ "participantes", class_ "container"] $ do
      forM_ grupo.participantes $ \p ->
        p_ $ toHtml p.participanteNombre

newtype ParticipanteAddParams = ParticipanteAddParams
  { name :: Text
  } deriving (Show, Eq, Generic)

instance FromForm ParticipanteAddParams

newParticipanteForm :: Monad m => Digestive.Form Text m ParticipanteAddParams
newParticipanteForm =
  ParticipanteAddParams
  <$> "name" Digestive..: (
    Digestive.text Nothing
    & Digestive.check "No puede ser vacio" (not . Text.null)
  )

newParticipanteView :: Monad m => ULID -> Digestive.View Text -> HtmlT m ()
newParticipanteView grupoId view =
  form_
    [ hxPost_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeGrupoParticipanteAdd grupoId
    ] $ do
    fieldset_ $ do
      label_ $ do
        "Nombre"
        input_
          [ value_ $ Digestive.fieldInputText "name" view
          , name_ $ Digestive.absoluteRef "name" view
          , id_ $ Digestive.absoluteRef "name" view
          , type_ "text"
          , placeholder_ "Juan Perez"
          , if Digestive.errors "name" view & null
            then mempty
            else makeAttributes "aria-invalid" "true"
          ]

        forM_ (Digestive.errors "name" view) $ \t -> do
          small_ $ toHtml t
    button_ [class_ "button is-primary"] "Agregar Participante"

runSelda :: SeldaT PG AppHandler a -> AppHandler a
runSelda dbAction = do
  seldaConn <- asks (.connection)
  runSeldaT dbAction seldaConn

renderPago :: Monad m => Grupo -> ULID -> Pago -> HtmlT m ()
renderPago grupo grupoId pago = do
  p_ [hxTarget_ "this"] $ do
    toHtml $ nombre pago
    " ("
    toHtml $ monto pago
    ") pagado por "
    case pagadores pago of
      [] -> "nadie (?"
      [pagador] ->
        toHtml $ pagador & parteParticipante & buscarParticipante grupo & participanteNombre
      pagador:rest -> do
        toHtml $ pagador & parteParticipante & buscarParticipante grupo & participanteNombre
        if length rest == 1
          then " y alguien mas"
          else do
            " y otras "
            toHtml $ show $ length rest
            " personas"
    button_
      [ hxGet_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagoEdit grupoId $ pagoId pago
      , hxSwap_ "outerHTML"
      ] "editar"
    button_
      [ hxDelete_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagoDelete grupoId $ pagoId pago
      , hxSwap_ "outerHTML"
      ] "borrar"