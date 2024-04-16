{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeFamilies #-}

module Site.Handler.Grupos
    ( CreateGrupoParams
    , handleCreateGrupo
      -- , handleCreateParticipante
      -- , handleIndex
    , handleCreateParticipante
    , handleDeleteParticipante
    , handleGetNetos
    , handleShowGrupo
    ) where

import BananaSplit (Grupo, Participante, ParticipanteId (..), calcularDeudasTotales,
                    resolverDeudasNaif)
import BananaSplit.Persistence (addParticipante, createGrupo, deleteShallowParticipante, fetchGrupo)

import Control.Monad (forM_)
import Control.Monad.Reader

import Data.Function ((&))
import Data.Pool qualified as Pool
import Data.Text (Text)
import Data.Text qualified as Text
import Data.ULID (ULID)

import Database.Selda.Backend
import Database.Selda.PostgreSQL (PG)

import GHC.Generics

import Lucid
import Lucid.Base (makeAttributes)
import Lucid.Htmx

import Servant

import Site.Api
import Site.Handler.Utils (htmlLayout, orElse, orElseMay, postForm, redirect, renderHtml, throwHtml,
                           throwJsonError)
import Site.HTML
import Site.Layout (navBarItemsForGrupo)

import Text.Digestive qualified as Digestive

import Types

import Web.FormUrlEncoded (Form, FromForm (..))

-- handleIndex :: AppHandler RawHtml
-- handleIndex = do
--   view <- Digestive.getForm "grupo" createGrupoForm

--   renderHtml $ htmlLayout mempty $ do
--     main_ [class_ "container"] $ createGrupoView view

handleCreateGrupo :: CreateGrupoParams -> AppHandler Grupo
handleCreateGrupo CreateGrupoParams{grupoName} = do
  runSelda $ createGrupo grupoName

handleGetNetos :: ULID -> AppHandler Netos
handleGetNetos grupoId = do
  grupo <- runSelda (fetchGrupo grupoId)
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

  let deudas = calcularDeudasTotales grupo
  pure $ Netos
    { netos = deudas
    , transaccionesParaSaldar = resolverDeudasNaif deudas
    }


-- createGrupoForm :: Monad m => Digestive.Form Text m CreateGrupoParams
-- createGrupoForm =
--   CreateGrupoParams
--   <$> "name" Digestive..: (
--     Digestive.text Nothing
--     & Digestive.check "No puede ser vacio" (not . Text.null)
--   )
handleDeleteParticipante :: ULID -> ULID -> AppHandler ULID
handleDeleteParticipante grupoId participanteId = do
  runSelda (deleteShallowParticipante grupoId participanteId)
  pure participanteId

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

handleShowGrupo :: ULID -> AppHandler Grupo
handleShowGrupo grupoId = do
  runSelda (fetchGrupo grupoId)
    `orElseMay` throwJsonError err404 "Grupo no encontrado"

handleCreateParticipante :: ULID -> ParticipanteAddParams -> AppHandler Participante
handleCreateParticipante grupoId ParticipanteAddParams{name} = do
  runSelda (addParticipante grupoId name)
    `orElse` (\_e -> throwJsonError err400 "falle")

--   case maybeAddParticipante of
--     Just (ParticipanteAddParams {name}) -> do
--       view' <- Digestive.getForm "participante" newParticipanteForm
--       renderHtml $ do
--         newParticipanteView grupoId view'
--         div_
--           [ hxSwapOob_ "beforeend:#participantes"
--           ] $ do
--           p_ $ toHtml participante.participanteNombre
--     Nothing -> do
--       renderHtml $ do
--         newParticipanteView grupoId view

-- handleShowParticipantes :: ULID -> AppHandler RawHtml
-- handleShowParticipantes grupoId = do
--   grupo <- runSelda (fetchGrupo grupoId)
--     `orElseMay` throwHtml "ese grupo no existe lol"

--   view <- Digestive.getForm "participante" newParticipanteForm

--   renderHtml $ htmlLayout (navBarItemsForGrupo grupo) $ do
--     main_ [class_ "container"] $ newParticipanteView grupoId view
--     div_ [id_ "participantes", class_ "container"] $ do
--       forM_ grupo.participantes $ \p ->
--         p_ $ toHtml p.participanteNombre

-- newtype ParticipanteAddParams = ParticipanteAddParams
--   { name :: Text
--   } deriving (Show, Eq, Generic)

-- instance FromForm ParticipanteAddParams

newParticipanteForm :: Monad m => Digestive.Form Text m ParticipanteAddParams
newParticipanteForm =
  ParticipanteAddParams
  <$> "name" Digestive..: (
    Digestive.text Nothing
    & Digestive.check "No puede ser vacio" (not . Text.null)
  )

-- newParticipanteView :: Monad m => ULID -> Digestive.View Text -> HtmlT m ()
-- newParticipanteView grupoId view =
--   form_
--     [ hxPost_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeGrupoParticipanteAdd grupoId
--     ] $ do
--     fieldset_ $ do
--       label_ $ do
--         "Nombre"
--         input_
--           [ value_ $ Digestive.fieldInputText "name" view
--           , name_ $ Digestive.absoluteRef "name" view
--           , id_ $ Digestive.absoluteRef "name" view
--           , type_ "text"
--           , placeholder_ "Juan Perez"
--           , if Digestive.errors "name" view & null
--             then mempty
--             else makeAttributes "aria-invalid" "true"
--           ]

--         forM_ (Digestive.errors "name" view) $ \t -> do
--           small_ $ toHtml t
--     button_ [class_ "button is-primary"] "Agregar Participante"

runSelda :: SeldaT PG IO a -> AppHandler a
runSelda dbAction = do
  pool <- asks (.connection)

  liftIO $ Pool.withResource pool $ \seldaConn -> do
    runSeldaT dbAction seldaConn

-- renderPago :: Monad m => Grupo -> ULID -> Pago -> HtmlT m ()
-- renderPago grupo grupoId pago = do
--   p_ [hxTarget_ "this"] $ do
--     toHtml $ nombre pago
--     " ("
--     toHtml $ monto pago
--     ") pagado por "
--     case pagadores pago of
--       [] -> "nadie (?"
--       [pagador] ->
--         toHtml $ pagador & parteParticipante & buscarParticipante grupo & participanteNombre
--       pagador:rest -> do
--         toHtml $ pagador & parteParticipante & buscarParticipante grupo & participanteNombre
--         if length rest == 1
--           then " y alguien mas"
--           else do
--             " y otras "
--             toHtml $ show $ length rest
--             " personas"
--     button_
--       [ hxGet_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagoEdit grupoId $ pagoId pago
--       , hxSwap_ "outerHTML"
--       ] "editar"
--     button_
--       [ hxDelete_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagoDelete grupoId $ pagoId pago
--       , hxSwap_ "outerHTML"
--       ] "borrar"
