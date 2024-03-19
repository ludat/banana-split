{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Site.Handler.Pagos
  ( handlePagoCreate
  , handlePagoDelete
  , handlePagoEdit
  , handlePagoNewPatch
  , handlePagoUpdate
  , handlePagoNew
  , handlePagosGet
  ) where

import Control.Monad.Reader

import Data.Function ((&))
import Data.Text (Text, pack)


import Servant

import Site.Handler.Utils (renderHtml, postForm, orElseMay, throwHtml, htmlLayout)
import Lucid
import Lucid.Htmx

import Types
import Site.HTML
import Data.String.Interpolate
import BananaSplit.Persistence (fetchGrupo, fetchParticipantes, savePago, deletePago, updatePago)
import BananaSplit
import Web.FormUrlEncoded (FromForm (..), Form, parseUnique, lookupAll)
import Data.Maybe (listToMaybe, fromJust, fromMaybe)
import Data.ULID (ULID)
import Database.Selda.Backend
import Database.Selda.PostgreSQL (PG)
import qualified Data.List as List
import Text.Pretty.Simple
import qualified Text.Digestive as Digestive
import Web.Internal.FormUrlEncoded (Form(..))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import Data.String.Conversions (cs)
import qualified Text.Digestive.Form.List as Digestive
import Text.Read (readMaybe)
import Site.Api
import Site.Layout (navBarItemsForGrupo)
import Lucid.Base (makeAttributes)
import Control.Monad (forM_)

_pagosUpdatedEvent :: Text
_pagosUpdatedEvent = "pagos-updated"

handlePagoCreate :: ULID -> Form -> AppHandler (Headers '[HXTrigger, HXRetarget, HXReswap] RawHtml)
handlePagoCreate grupoId form = do
  (view, maybePago) <- postForm "pago" form (pagosForm Nothing)

  case maybePago of
    Just pago -> do
      _ <- runSelda (savePago grupoId pago)
      grupo <- fromJust <$> runSelda (fetchGrupo grupoId)

      fmap (noHeader . addHeader "#pagos" . addHeader "afterbegin") $ renderHtml $ do
        renderPago grupo grupoId pago

    Nothing -> do
      grupo <- fromJust <$> runSelda (fetchGrupo grupoId)

      fmap (noHeader . noHeader . noHeader) $ renderHtml $ do
        pagosView grupo.grupoId Nothing grupo.participantes view

handlePagoUpdate :: ULID -> ULID -> Form -> AppHandler (Headers '[HXTrigger, HXRetarget, HXReswap] RawHtml)
handlePagoUpdate grupoId pagoId form = do
  (view, maybePago) <- postForm "pago" form (pagosForm Nothing)

  case maybePago of
    Just pagoWithoutId -> do
      let pago = pagoWithoutId { pagoId = pagoId }
      runSelda $ updatePago grupoId pagoId pago
      grupo <- fromJust <$> runSelda (fetchGrupo grupoId)

      fmap (noHeader . addHeader [i|\#p-#{pagoId}|] . addHeader "outerHTML") $ renderHtml $ do
            -- ^ necesito triggerear el refresh si actualizo el pago justo?
        renderPago grupo grupoId pago
    Nothing -> do
      grupo <- fromJust <$> runSelda (fetchGrupo grupoId)

      fmap (noHeader . noHeader . noHeader) $ renderHtml $ do
        pagosView grupo.grupoId (Just pagoId) grupo.participantes view

handlePagoNew :: ULID -> AppHandler RawHtml
handlePagoNew grupoId = do
  participantes <- runSelda (fetchParticipantes grupoId)

  let fakeDeudores :: [Parte] =
        participantes
        & fmap participanteId
        & fmap (\pId -> Ponderado 1 pId)

  let fakePagadores :: [Parte] =
        participantes
        & fmap participanteId
        & listToMaybe
        & maybe mempty (\pId -> [Ponderado 1 pId])

  let pago = Pago nullUlid 0 "" fakeDeudores fakePagadores

  view <- Digestive.getForm "pago" $ pagosForm (Just pago)

  renderHtml $ do
    div_ [class_ "modal is-active"] $ do
      div_
        [ class_ "modal-background"
        , makeAttributes "hx-on:click" "this.closest('.modal').remove()"
        ] mempty
      div_ [ class_ "modal-content" ] $ do
        div_ [ class_ "box" ] $ do
          pagosView grupoId Nothing participantes view
      button_
        [ class_ "modal-close is-large"
        , makeAttributes "hx-on:click" "this.closest('.modal').remove()"
        ] mempty

handlePagoNewPatch :: ULID -> Maybe ULID -> Form -> AppHandler RawHtml
handlePagoNewPatch grupoId pagoId form = do
  participantes <- runSelda (fetchParticipantes grupoId)

  let event = fromForm @PagoFormEvent form

  newForm <- case event of
    Left e -> do
      pPrint e
      pure form
    Right (RemoveParte tipoParte index) -> do
      let
        seccion :: Text = case tipoParte of
          Pagador -> "deudores"
          Deudor -> "pagadores"
        oldIndicies =
          form
          & lookupAll [i|pago.#{seccion}.indices|]
          & listToMaybe
          & fromMaybe ""
          & Digestive.parseIndices
        newIndices =
          oldIndicies
          & filter (/= index)
      pure $ form
        & insertForm [i|pago.#{seccion}.indices|] (newIndices & Digestive.unparseIndices)
    Right (AgregarParte tipoParte) -> do
      let
        seccion :: Text = case tipoParte of
          Pagador -> "deudores"
          Deudor -> "pagadores"
        oldIndicies =
          form
          & lookupAll [i|pago.#{seccion}.indices|]
          & listToMaybe
          & fromMaybe ""
          & Digestive.parseIndices
        nextIndex =
          oldIndicies
          & (\case
              [] -> [0]
              x -> x)
          & maximum
          & (+1)
        newIndices =
          oldIndicies ++ [nextIndex]
      pure $ form
        & insertForm [i|pago.#{seccion}.indices|] (newIndices & Digestive.unparseIndices)
        & insertForm [i|pago.#{seccion}.#{nextIndex}.tipo|] "Ponderado"
        & insertForm [i|pago.#{seccion}.#{nextIndex}.cuota|] "1"
        & insertForm [i|pago.#{seccion}.#{nextIndex}.participante|] (participantes & head & participanteId & show & Text.pack)

  (view, _maybePago) <- postForm "pago" newForm (pagosForm Nothing)

  renderHtml $ do
    pagosView grupoId pagoId participantes view

insertForm :: Text -> Text -> Form -> Form
insertForm key value (Form formMap) =
  Form $ HashMap.insert key [value] formMap

handlePagoEdit :: ULID -> ULID -> AppHandler RawHtml
handlePagoEdit grupoId pId = do
  grupo <- fromJust <$> runSelda (fetchGrupo grupoId)

  let
    pago =
      grupo.pagos
      & List.find (\p -> pagoId p == pId)

  view <- Digestive.getForm "pago" $ pagosForm pago

  renderHtml $ do
    div_ [class_ "modal is-active"] $ do
      div_
        [ class_ "modal-background"
        , makeAttributes "hx-on:click" "this.closest('.modal').remove()"
        ] mempty
      div_ [ class_ "modal-content" ] $ do
        div_ [ class_ "box" ] $ do
          pagosView grupoId (Just pId) grupo.participantes view
      button_
        [ class_ "modal-close is-large"
        , makeAttributes "hx-on:click" "this.closest('.modal').remove()"
        ] mempty

handlePagoDelete :: ULID -> ULID -> AppHandler (Headers '[HXTrigger] RawHtml)
handlePagoDelete _grupoId pagoId = do
  runSelda $ deletePago pagoId

  addHeader _pagosUpdatedEvent <$> renderHtml mempty

handlePagosGet :: ULID -> AppHandler RawHtml
handlePagosGet grupoId = do
  grupo <- runSelda (fetchGrupo grupoId)
    `orElseMay` throwHtml (h1_ $ toHtml @Text "grupo no encontrado")

  renderHtml $ htmlLayout (navBarItemsForGrupo grupo) $ do
    section_ [class_ "section"] $ do
      div_ [class_ "container"] $ do
        h1_ [class_ "title"] "Pagos"
      div_ [class_ "container"] $ do
        button_
          [ hxGet_  $ ("/" <>) $ toUrlPiece $ fieldLink _routePagoNew grupo.grupoId
          , hxSwap_ "beforeend"
          , hxTarget_ "body"
          ]
          "Agregar pago"

        button_
          [ hxGet_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagosGet grupo.grupoId
          , hxTrigger_ [i|click, #{_pagosUpdatedEvent} from:body|]
          , hxSelect_ "#pagos"
          , hxTarget_ "#pagos"
          , hxSwap_ "outerHTML"
          ] "Refrescar pagos"
      div_
        [ id_ "pagos"
        , class_ "columns"
        ] $ do
        forM_ grupo.pagos $ \pago -> do
          div_ [class_ "column"] $ renderPago grupo grupoId pago

data PagoFormEvent
  = RemoveParte TipoParte Int
  | AgregarParte TipoParte
  deriving (Show, Read, Eq)

data TipoParte = Deudor | Pagador deriving (Show, Read, Eq)

instance FromHttpApiData TipoParte where
  parseQueryParam t =
    case t of
      "deudor" -> Right Pagador
      "pagador" -> Right Deudor
      _ -> Left $ "invalid tipo parte: " <> t

instance FromForm PagoFormEvent where
  fromForm f = do
    event <- parseUnique @Text "event" f
    case event of
      "BORRAR_PARTE" -> do
        RemoveParte
        <$> parseUnique "tipoParte" f
        <*> parseUnique "index" f
      "AGREGAR_PARTE" -> do
        AgregarParte
        <$> parseUnique "tipoParte" f
      _ -> Left $ "Unknown event type: " <> event

pagosView :: Monad m => ULID -> Maybe ULID -> [Participante] -> Digestive.View Text -> HtmlT m ()
pagosView grupoId existentPago participantes view =
  form_
    [ mconcat $ maybe
      [ hxPost_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeGrupoPagoAdd grupoId
      ]
      (\pagoId ->
        [ hxPut_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagoUpdate grupoId pagoId
        ]
      )
      existentPago
    , hxTarget_ "this"
    , hxSwap_ "outerHTML"
    , makeAttributes "hx-on:htmx:after-request" "this.closest('.modal').remove()"
    , id_ "pagos-form"
    ] $ do
    p_ $ do
      label_ "nombre"
      input_
        [ name_ $ Digestive.absoluteRef "nombre" view
        , value_ $ Digestive.fieldInputText "nombre" view
        ]
      forM_ (Digestive.errors "nombre" view) $ \err -> do
        span_ $ toHtml err

    p_ $ do
      label_ "monto"
      input_
        [ name_ $ Digestive.absoluteRef "monto" view
        , value_ $ Digestive.fieldInputText "monto" view
        , placeholder_ "1000"
        ]
      forM_ (Digestive.errors "monto" view) $ \err -> do
        span_ $ toHtml err

    div_
      [ id_ "seccion-pagadores"
      , hxVals_ [i|{"tipoParte": "pagador"}|]
      ] $ do
      label_ "Pagadores"
      div_
        [ id_ "pagadores"
        , class_ "partes"
        ] $ do
        input_
          [ mempty
          , type_ "hidden"
          , value_ $ Digestive.fieldInputText "pagadores.indices" view
          , name_ $ Digestive.absoluteRef "pagadores.indices" view
          ]
        forM_ (Digestive.fieldInputText "pagadores.indices" view & Digestive.parseIndices) $ \index -> do
          parteView grupoId existentPago participantes index (Digestive.makeListSubView "pagadores" index view)
      div_ [] $ do
        button_
          [ hxPatch_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagoNewPatch grupoId existentPago
          , type_ "button"
          , hxVals_ "{\"event\": \"AGREGAR_PARTE\"}"
          ]
          "agregar pagador"

    div_
      [ id_ "seccion-deudores"
      , hxVals_ [i|{"tipoParte": "deudor"}|]
      ] $ do
      label_ "Deudores"
      div_
        [ id_ "deudores"
        , class_ "partes"
        ] $ do
        input_
          [ mempty
          , type_ "hidden"
          , value_ $ Digestive.fieldInputText "deudores.indices" view
          , name_ $ Digestive.absoluteRef "deudores.indices" view
          ]
        forM_ (Digestive.fieldInputText "deudores.indices" view & Digestive.parseIndices) $ \index -> do
          parteView grupoId existentPago participantes index (Digestive.makeListSubView "deudores" index view)

      div_ [] $ do
        button_
          [ hxPatch_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagoNewPatch grupoId existentPago
          , type_ "button"
          , hxVals_ "{\"event\": \"AGREGAR_PARTE\"}"
          ]
          "agregar deudor"

    p_ $ button_ 
        [ type_ "submit"
        ] $
      case existentPago of 
        Just _ -> "Actualizar pago"
        Nothing -> "Crear pago"

parteView :: Monad m => ULID -> Maybe ULID -> [Participante] -> Int -> Digestive.View Text -> HtmlT m ()
parteView grupoId pagoId participantes index view = do
  let tipo = Digestive.fieldInputText "tipo" view
  div_ [class_ "parte-form" ] $ do
    participantsView participantes (Digestive.subView "participante" view)
    input_
      [ name_ $ Digestive.absoluteRef "cuota" view
      , value_ $ Digestive.fieldInputText "cuota" view
      , if tipo /= "Ponderado" then type_ "hidden" else mempty
      ]
    input_
      [ name_ $ Digestive.absoluteRef "monto" view
      , value_ $ Digestive.fieldInputText "monto" view
      , if tipo /= "MontoFijo" then type_ "hidden" else mempty
      ]
    select_
      [ name_ $ Digestive.absoluteRef "tipo" view
      , hxPatch_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagoNewPatch grupoId pagoId
      ] $ do
      option_
        [ value_ "MontoFijo"
        , case tipo of
            "Ponderado" -> mempty
            "MontoFijo" -> selected_ "selected"
            _ -> mempty
        ] "Fijo"
      option_
        [ value_ "Ponderado"
        , case tipo of
            "Ponderado" -> selected_ "selected"
            "MontoFijo" -> mempty
            _ -> mempty
        ] "Ponderado"
    forM_ (Digestive.errors "" view) $ span_ . toHtml
    forM_ (Digestive.errors "tipo" view) $ span_ . toHtml
    forM_ (Digestive.errors "monto" view) $ span_ . toHtml
    forM_ (Digestive.errors "cuota" view) $ span_ . toHtml
    forM_ (Digestive.errors "participante" view) $ span_ . toHtml
    button_
      [ hxPatch_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagoNewPatch grupoId pagoId
      , type_ "button"
      , hxVals_ [i|{"event": "BORRAR_PARTE", "index": #{index}}|]
      ] "borrar"

participantsView :: Monad m => [Participante] -> Digestive.View Text -> HtmlT m ()
participantsView participantes view =
  if null participantes
    then
      "No hay participantes"
    else do
      select_
        [name_ $ Digestive.absoluteRef "" view] $ do
        forM_ participantes $ \(Participante ulid nombre) -> do
          option_
            [ value_ . pack . show $ ulid
            , if Digestive.fieldInputText "" view == (pack . show $ ulid)
                then selected_ "selected"
                else mempty
            ] $ toHtml nombre

pagosForm :: (MonadIO m) => Digestive.Formlet Text m Pago
pagosForm maybePago =
  Pago
  <$> pure nullUlid
  <*> "monto" Digestive..: montoForm (fmap (.monto) maybePago)
  <*> "nombre" Digestive..: (
    Digestive.text (fmap (.nombre) maybePago)
    & Digestive.check "no puede ser vacio" (not . Text.null)
    )
  <*> "deudores" Digestive..: Digestive.listOf parteForm (fmap (.deudores) maybePago)
  <*> "pagadores" Digestive..: Digestive.listOf parteForm (fmap (.pagadores) maybePago)

parteForm :: Monad m => Digestive.Formlet Text m Parte
parteForm parteDef =
  let
    (tipoDef, cuotaDef, montoDef, participanteDef) =
      case parteDef of
        Nothing ->
          ("Ponderado", Nothing, Nothing, Nothing)
        Just (MontoFijo m p) ->
          ("MontoFijo", Nothing, Just m, Just p)
        Just (Ponderado c p) ->
          ("Ponderado", Just c, Nothing, Just p)
  in
      ((,,,)
      <$> "tipo" Digestive..: Digestive.text (Just tipoDef)
      <*> "monto" Digestive..: Digestive.text (Just $ monto2Text $ fromMaybe 0 montoDef)
      <*> "cuota" Digestive..: Digestive.text (Just $ cs $ show $ fromMaybe 1 cuotaDef)
      <*> "participante" Digestive..: Digestive.stringRead "participante invalido" participanteDef
      ) & Digestive.validate (\case
          ("MontoFijo", montoRaw,          _,  participante) -> do
            monto <- validateMonto montoRaw
            Digestive.Success $ MontoFijo monto participante

          ("Ponderado",          _, cuotaRaw,  participante) -> do
            cuota <- case readMaybe $ cs cuotaRaw of
              Just c -> Digestive.Success c
              Nothing -> Digestive.Error $ "cuota invalida: " <> cuotaRaw

            Digestive.Success $ Ponderado cuota participante

          ( a, b, c, participante) -> Digestive.Error $ cs $ show (a,b,c,participante)
      )

montoForm :: Monad m => Digestive.Formlet Text m Monto
montoForm montoDefault =
  Digestive.validate (\t -> case text2Monto t of
    Just money -> Digestive.Success money
    Nothing -> Digestive.Error "Numero invalido"
    ) (Digestive.text (fmap monto2Text montoDefault))

validateMonto :: Text -> Digestive.Result Text Monto
validateMonto rawMonto =
    case text2Monto rawMonto of
      Just n -> Digestive.Success n
      Nothing -> Digestive.Error "numero invalido"

optionalMontoForm :: Monad m => Maybe Monto -> Digestive.Form Text m (Maybe Monto)
optionalMontoForm montoDefault =
  Digestive.validateOptional (\rawMonto ->
    case text2Monto rawMonto of
      Just n -> Digestive.Success n
      Nothing -> Digestive.Error "numero invalido"
  ) $ Digestive.optionalText (fmap monto2Text montoDefault)

runSelda :: SeldaT PG AppHandler a -> AppHandler a
runSelda dbAction = do
  seldaConn <- asks (.connection)
  runSeldaT dbAction seldaConn

renderPago :: Monad m => Grupo -> ULID -> Pago -> HtmlT m ()
renderPago grupo grupoId pago = do
  div_ 
    [ hxTarget_ "this"
    , id_ [i|p-#{pagoId pago}|]
    , class_ "card"
    ] $ do
    div_
      [ class_ "card-content"
      ] $ do
      p_ [class_ "title"] $ toHtml $ nombre pago
      p_ [class_ "subtitle"] $ do
        toHtml $ monto pago
        span_ " pagado por "
        span_ $ case pagadores pago of
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
    footer_ [class_ "card-footer"] $ do
      p_ [class_  "card-footer-item"] $ span_ [] $ a_
        [ hxGet_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagoEdit grupoId pago.pagoId
        , hxSwap_ "beforeend"
        , hxTarget_ "body"
        -- , class_ "button"
        ] "Editar"
      p_ [class_  "card-footer-item"] $ span_ [] $ a_
        [ hxDelete_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagoDelete grupoId pago.pagoId
        , hxConfirm_ "Estas seguro de borrar este pago?"
        , hxSwap_ "outerHTML"
        -- , class_ "button"
        ] "Borrar"