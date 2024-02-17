{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Site.Api where

import Control.Monad.Reader

import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Text (Text, pack)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Time.Calendar.OrdinalDate (fromOrdinalDate)
import Numeric.Natural

import GHC.Generics

import Network.Wai

import Servant
import Servant.Server.Generic

import Site.Handler.Utils (orElse, orElseMay, redirect, throwHtml)
import Lucid
import Lucid.Htmx

import Types
import Site.HTML
import Data.String.Interpolate
import System.Random (randomIO)
import RompePiernas.Persistence (createGrupo, fetchGrupo, addParticipante, fetchParticipantes, savePago, deletePago, updatePago)
import RompePiernas
import Web.FormUrlEncoded (FromForm, Form)
import Lucid.Base (makeAttributes)
import Data.Maybe (listToMaybe, fromJust, fromMaybe)
import Data.ULID (getULID, ULID, ulidFromInteger)
import Database.Selda.Backend
import Database.Selda (MonadMask)
import Database.Selda.PostgreSQL (PG)
import qualified Data.List as List
import Text.Pretty.Simple (pPrint)
import Text.Pretty.Simple
import qualified Text.Digestive as Digestive
import Web.Internal.FormUrlEncoded (Form(..))
import qualified Data.HashMap.Strict as HashMap
import Debug.Pretty.Simple (pTraceShowIdForceColor, pTraceShow, pTraceShowForceColor)
import qualified Data.Text as Text
import Data.Either (fromRight)
import qualified Data.Aeson as Aeson
import Data.String.Conversions (cs)
import Data.Aeson ((.=))

data Api routes
  = Api
    { _routeIndex ::
      routes :- Get '[HTML] RawHtml
    , _routeNewDeudas ::
      routes :- "new" :> Get '[HTML] (Headers '[Header "Location" String] NoContent)
    , _routeDeudas ::
      routes :- "deudas" :> Capture "id" ULID :> Get '[HTML] RawHtml
    , _routeGrupoParticipanteAdd ::
      routes :- "deudas" :> Capture "id" ULID :> "participantes" :> ReqBody '[FormUrlEncoded] Form :> Post '[HTML] RawHtml
    , _routePagoNew ::
      routes :- "deudas" :> Capture "id" ULID :> "pagos" :> Get '[HTML] RawHtml
    , _routeGrupoPagoAdd ::
      routes :- "deudas" :> Capture "id" ULID :> "pagos" :> ReqBody '[FormUrlEncoded] Form :> Post '[HTML] RawHtml
    , _routePagoDelete ::
      routes :- "deudas" :> Capture "id" ULID :> "pagos" :> Capture "pagoId" ULID :> Delete '[HTML] RawHtml
    , _routePagoEdit ::
      routes :- "deudas" :> Capture "id" ULID :> "pagos" :> Capture "pagoId" ULID :> Get '[HTML] RawHtml
    , _routePagoUpdate ::
      routes :- "deudas" :> Capture "id" ULID :> "pagos" :> Capture "pagoId" ULID :> ReqBody '[FormUrlEncoded] Pago :> Put '[HTML] RawHtml
    , _routeParteNew ::
      routes :- "deudas" :> Capture "id" ULID :> "partes" :> Capture "tipo" Text :> Put '[HTML] RawHtml
    , _routeStatic ::
      routes :- "static" :> Raw
    }
  deriving (Generic)

data ParticipanteAddParams = ParticipanteAddParams
  { name :: Text
  } deriving (Show, Eq, Generic)

instance FromForm ParticipanteAddParams

type TheAPI routes = ToServant Api routes


renderHtml :: MonadIO m => HtmlT m () -> m RawHtml
renderHtml content = do
  html <- renderBST content
  pure $ RawHtml html

htmlLayout :: Monad m => HtmlT m () -> HtmlT m ()
htmlLayout content =
  doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "UTF-8"]
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
      title_ "Rompepiernas"
      link_ [rel_ "shortcut icon", type_ "image/png", href_ "/static/favicon.png"]

      script_ [src_ "https://unpkg.com/htmx.org@1.9.10"] $ toHtml @Text ""
      script_ [src_ "https://unpkg.com/htmx.org@1.9.10/dist/ext/ws.js"] $ toHtml @Text ""
      script_ [src_ "https://unpkg.com/htmx.org@1.9.10/dist/ext/debug.js"] $ toHtml @Text ""
      script_ [src_ "https://cdn.jsdelivr.net/npm/alpinejs@3.x.x/dist/cdn.min.js", defer_ "true"] $ toHtml @Text ""

      link_ [rel_ "stylesheet", type_ "text/css", href_ "/static/styles.css"]
      -- link_ [rel_ "stylesheet", type_ "text/css", href_ "https://cdn.jsdelivr.net/npm/@picocss/pico@1/css/pico.min.css"]
      link_ [rel_ "stylesheet", type_ "text/css", href_ "https://cdn.jsdelivr.net/npm/bulma@0.9.4/css/bulma.min.css"]

    body_ $ do
      section_ [class_ "section"] $ do
        div_ [class_ "container"] $ do
          h1_ [class_ "title"] "El anti rompepiernas"
          p_ [class_ "subtitle"] "cosas re locas lalal al la al dwal djwial jwail wakdwabk"

      content

serverT :: Api (AsServerT AppHandler)
serverT =
  Api
    { _routeIndex = do
      renderHtml $ htmlLayout $ do
        main_ [hxBoost_ "true"] $ do
          a_ [href_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeNewDeudas] "Crear nuevo grupo"

    , _routeNewDeudas = do
      grupo <- runSelda createGrupo

      redirect $ encodeUtf8 $ ("/" <>) $ toUrlPiece $ fieldLink _routeDeudas (grupo.grupoId)
    , _routeDeudas = \grupoId -> do
      grupo <- runSelda (fetchGrupo grupoId)
        `orElseMay` (throwHtml $ h1_ $ toHtml @Text "grupo no encontrado")
      view <- Digestive.getForm "participante" newParticipanteForm

      renderHtml $ htmlLayout $ do
        div_ [hxBoost_ "true" ] $ do
          h2_ "Participantes:"
          newParticipanteView grupoId view
          div_ [id_ "usuarios"] $ do
            forM_ grupo.participantes $ \participante -> do
              p_ [id_ [i|participante-#{participanteId participante}|]] $ toHtml $ participanteNombre participante

          form_ [id_ "pagos-form", style_ "display: none;"] ""
          button_
            [ hxGet_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagoNew grupoId
            , hxSwap_ "outerHTML"
            , hxTarget_ "#pagos-form"
            ] "Agregar nuevo pago"

          div_ [id_ "pagos"] $ do
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

            h3_ "Pagos"
            forM_ grupo.pagos $ \pago -> do
              renderPago grupo grupoId pago
          
    , _routeGrupoParticipanteAdd = \grupoId (Form formMap) -> do
      (view, maybeAddParticipante) <- Digestive.postForm "participante" newParticipanteForm 
        (\e -> pure $ \p ->
          HashMap.lookup (Digestive.fromPath p) formMap & fromMaybe [] & fmap (Digestive.TextInput) & pure)

      liftIO $ print maybeAddParticipante
      liftIO $ print view

      maybeParticipante <- case maybeAddParticipante of
        Just (ParticipanteAddParams {name}) -> runSelda (addParticipante grupoId name)
        Nothing -> pure $ Left "Participante invalido"


      renderHtml $ do
        newParticipanteView grupoId view
        case maybeParticipante of
          Left _ -> mempty
          Right participante -> div_
            [ hxSwapOob_ "beforeend:#usuarios"
            ] $ do
              p_
                [ id_ [i|participante-#{participante & participanteId}|]
                ] $ toHtml $ participante & participanteNombre

    , _routeGrupoPagoAdd = \grupoId (Form formMap) -> do
      (view, maybePago) <- Digestive.postForm "pago" (pagosForm Nothing)
        (\e -> pure $ \p ->
          HashMap.lookup (Digestive.fromPath p) formMap & fromMaybe [] & fmap (Digestive.TextInput) & pure)

      pPrintForceColor maybePago

      case maybePago of
        Just pago -> do
          -- _ <- runSelda (savePago grupoId pago)
          grupo <- fromJust <$> runSelda (fetchGrupo grupoId)

          renderHtml $ do
            -- pagosView grupo.grupoId grupo.participantes view
            renderPago grupo grupoId pago
        Nothing -> do
          grupo <- fromJust <$> runSelda (fetchGrupo grupoId)

          renderHtml $ do
            pagosView grupo.grupoId grupo.participantes view

    , _routePagoUpdate = \grupoId pagoId pagoWitoutId -> do
      let pago = pagoWitoutId {pagoId = pagoId}
      runSelda (updatePago grupoId pagoId pago)
      grupo <- fromJust <$> runSelda (fetchGrupo grupoId)
      pPrint pago

      renderHtml $ do
        renderPago grupo grupoId pago

    , _routePagoNew = \grupoId -> do
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

      pPrint view

      renderHtml $ do
        pagosView grupoId participantes view

    , _routePagoEdit = \grupoId pId -> do
      grupo <- fromJust <$> runSelda (fetchGrupo grupoId)

      let
        pago =
          grupo.pagos
          & List.find (\p -> pagoId p == pId)

      view <- Digestive.getForm "pago" $ pagosForm pago

      renderHtml $ do
        pagosView grupoId grupo.participantes view
        -- pagosForm grupoId grupo.participantes pago

    , _routePagoDelete = \grupoId pagoId -> do
      runSelda $ deletePago pagoId

      renderHtml $ do
        "borradito"

    , _routeParteNew = \grupoId prefix -> do
      participantes <- runSelda $ fetchParticipantes grupoId
      
      indice <- abs <$> randomIO @Int

      renderHtml $ do
        pure ()
        -- parteForm prefix indice participantes Nothing

    , _routeStatic =
      -- serveDirectoryWebApp "./public"
      serveDirectoryFileServer "./public"
    }

pagosView :: Monad m => ULID -> [Participante] -> Digestive.View Text -> HtmlT m ()
pagosView grupoId participantes view =
  form_
    -- [ mconcat $ maybe
    --   ([ hxPost_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeGrupoPagoAdd grupoId
    --   , hxTarget_ "#pagos"
    --   , hxSwap_ "beforeend"
    --   ])
    --   (\p -> 
    --     [ hxPut_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagoUpdate grupoId $ pagoId p
    --     , hxTarget_ "this"
    --     , hxSwap_ "outerHtml"
    --     ]
    --   )
    --   pago
    -- [ hxPut_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagoUpdate grupoId $ pagoId p
    [ -- hxPost_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeGrupoPagoAdd grupoId
    hxTarget_ "this"
    , hxSwap_ "outerHtml"
    , makeAttributes "hx-ext" "ws"
    , makeAttributes "ws-connect" "wss://ws.postman-echo.com/raw"
    , makeAttributes "ws-send" ""
    , hxVals_ "{\"event\": \"submit\"}"
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
        , makeAttributes "hx-ext" "debug"
        , placeholder_ "1000"
        ]
      forM_ (Digestive.errors "monto" view) $ \err -> do
        span_ $ toHtml err

    div_ [id_ "seccion-pagadores"] $ do
      label_ "Pagadores"
      div_ 
        [ id_ "pagadores"
        , class_ "partes"
        -- , makeAttributes "x-data" $ cs $ Aeson.encode $ Aeson.object ["rawIndices" .= Digestive.fieldInputText "pagadores.indices" view]
        -- , makeAttributes "x-init" $ [__i|
        --   $data.indices = rawIndices.split(',').filter(i => i != '').map(n => parseInt(n))
        -- |]
        ] $ do
        input_
          [ mempty
          -- , type_ "hidden"
          , value_ $ Digestive.fieldInputText "pagadores.indices" view
          -- , makeAttributes ":value" "indices.join(',')"
          , name_ $ Digestive.absoluteRef "pagadores.indices" view
          ]
        forM_ (Digestive.listSubViews "pagadores" view & zip [0..]) $ \(index, pagadorView) -> do
          parteView grupoId participantes index pagadorView
      div_ [] $ do
        button_
          [ hxPut_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeParteNew grupoId "pagadores"
          , type_ "button"
          , hxTarget_ "#pagadores"
          , hxSwap_ "beforeend"
          ]
          "agregar pagador"

    div_ [id_ "seccion-deudores"] $ do
      label_ "Deudores"
      div_ 
        [ id_ "deudores"
        , class_ "partes"
        -- , makeAttributes "x-data" $ cs $ Aeson.encode $
        --     Aeson.object 
        --       [ "rawIndices" .= Digestive.fieldInputText "deudores.indices" view
        --       ]
        -- , makeAttributes "x-init" $ [__i|
        --   $data.indices = rawIndices.split(',').filter(i => i != '').map(n => parseInt(n))
        -- |]
        ] $ do
        input_
          [ mempty
          -- , type_ "hidden"
          , value_ $ Digestive.fieldInputText "deudores.indices" view
          -- , makeAttributes ":value" "indices.join(',')"
          , name_ $ Digestive.absoluteRef "deudores.indices" view
          ]
        forM_ (Digestive.listSubViews "deudores" view & zip [0..]) $ \(index, deudorView) -> do
          parteView grupoId participantes index deudorView
        
      div_ [] $ do
        button_
          [ hxPut_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeParteNew grupoId "deudores"
          , type_ "button"
          , hxTarget_ "#deudores"
          , hxSwap_ "beforeend"
          ]
          "agregar deudor"

    p_ $ button_ [] "Crear pago"

parteView :: Monad m => ULID -> [Participante] -> Integer -> Digestive.View Text -> HtmlT m ()
parteView grupoId participantes index view = do
  let tipo = Digestive.fieldInputText "tipo" view
  div_ [class_ "parte-form" ] $ do
    select_
      [ name_ $ Digestive.absoluteRef "tipo" view
      , hxPost_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeGrupoPagoAdd grupoId
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
    case tipo of 
      "Ponderado" ->
        input_
          [ name_ $ Digestive.absoluteRef "cuota" view
          , value_ $ Digestive.fieldInputText "cuota" view
          ]
      "MontoFijo" -> 
        input_
          [ name_ $ Digestive.absoluteRef "monto" view
          , value_ $ Digestive.fieldInputText "monto" view
          ]
      _ -> "mori"
    participantsView participantes (Digestive.subView "participante" view)
    forM_ (Digestive.errors "tipo" view) $ \e -> do
      span_ $ toHtml e
    forM_ (Digestive.errors "monto" view) $ \e -> do
      span_ $ toHtml e
    forM_ (Digestive.errors "cuota" view) $ \e -> do
      span_ $ toHtml e
    forM_ (Digestive.errors "participante" view) $ \e -> do
      span_ $ toHtml e
    button_ 
      [ mempty
      -- , makeAttributes "hx-on:click" [__i|
      --     debugger;
      --     // this.closest('.parte-form').remove();
      --   |]
      -- , makeAttributes "x-data" "{index: 2}"
      , makeAttributes "x-data" $ cs $ Aeson.encode $ Aeson.object ["index" .= index]
      , makeAttributes "@click" [__i|
        indices = indices.filter(n => n != index);
        $el.closest('.parte-form').remove();
      |]
      , type_ "button"
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

newParticipanteForm :: Monad m => Digestive.Form Text m ParticipanteAddParams
newParticipanteForm = 
  ParticipanteAddParams 
  <$> "name" Digestive..: (
    Digestive.text Nothing
    & Digestive.check "No puede ser vacio" (not . Text.null)
  )

pagosForm :: (MonadIO m) => Digestive.Formlet Text m Pago
pagosForm maybePago =
  Pago
  <$> Digestive.monadic (do { newUlid <- liftIO getULID; pure $ pure newUlid })
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
      (\cases 
          "MontoFijo" (Just monto)          _  participante -> MontoFijo monto participante
          "Ponderado"  _          (Just cuota) participante -> Ponderado cuota participante
          _            _           _           participante -> Ponderado 11 participante
          -- a b c d -> error $ show (a, b, c, d)
      )
      <$> "tipo" Digestive..: Digestive.text (Just tipoDef)
      <*> "monto" Digestive..: optionalMontoForm montoDef
      <*> "cuota" Digestive..: Digestive.optionalStringRead "numeros maloso" cuotaDef
      <*> "participante" Digestive..: Digestive.stringRead "participante malo" participanteDef

montoForm :: Monad m => Digestive.Formlet Text m Monto
montoForm montoDefault =
  Digestive.validate (\t -> case text2Monto t of
    Just money -> Digestive.Success money
    Nothing -> Digestive.Error "Numero invalido"
    ) (Digestive.text (fmap monto2Text montoDefault))

optionalMontoForm :: Monad m => Maybe Monto -> Digestive.Form Text m (Maybe Monto)
optionalMontoForm montoDefault =
  (Digestive.validateOptional (\rawMonto ->
    case text2Monto rawMonto of
      Just n -> Digestive.Success n
      Nothing -> Digestive.Error "numero invalido"
  ) $ Digestive.optionalText (fmap monto2Text montoDefault)
  )


newParticipanteView :: Monad m => ULID -> Digestive.View Text -> HtmlT m ()
newParticipanteView grupoId view =
  form_
    [ hxPost_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeGrupoParticipanteAdd grupoId
    , hxTarget_ "this"
    ] $ do
      input_
        [ name_ $ Digestive.absoluteRef "name" view
        , value_ $ Digestive.fieldInputText "name" view
        , id_ "name"
        ]
      forM_ (Digestive.errors "name" view) $ \err -> do
        span_ $ toHtml err
      p_ $ button_ "Agregar usuario"

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

-- pagosForm :: MonadIO m => ULID -> [Participante] -> Maybe Pago -> HtmlT m ()
-- pagosForm grupoId participantes pago =
--   let
--     fakeDeudores :: [Parte]
--     fakeDeudores =
--       participantes
--       & fmap participanteId
--       & fmap (\pId -> Ponderado 1 pId)
--     deudoresToRender :: [Parte]
--     deudoresToRender = maybe fakeDeudores (\p -> deudores p) pago

--     fakePagadores :: [Parte]
--     fakePagadores =
--       let participante = listToMaybe . fmap participanteId $ participantes
--       in maybe mempty (\pId -> [Ponderado 1 pId]) participante

--     pagadoresToRender :: [Parte]
--     pagadoresToRender = maybe fakePagadores (\p -> pagadores p) pago
--   in
--   form_
--     [ mconcat $ maybe
--       ([ hxPost_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeGrupoPagoAdd grupoId
--       , hxTarget_ "#pagos"
--       , hxSwap_ "beforeend"
--       ])
--       (\p -> 
--         [ hxPut_ $ ("/" <>) $ toUrlPiece $ fieldLink _routePagoUpdate grupoId $ pagoId p
--         , hxTarget_ "this"
--         , hxSwap_ "outerHtml"
--         ]
--       )
--       pago
--     , id_ "pagos-form"
--     ] $ do
--     p_ $ do
--       label_ "nombre"
--       input_ [ name_ "nombre", maybe mempty (\p -> value_ $ nombre p) pago]
--     p_ $ do
--       label_ "monto"
--       input_ 
--         [ name_ "monto"
--         , maybe mempty (\p -> value_ $ monto2Text $ monto p) pago
--         , placeholder_ "1000"
--         ]

--     div_ [id_ "seccion-pagadores"] $ do
--       label_ "Pagadores"
--       div_ [id_ "pagadores"] $ do
--         -- pagoUlid <- liftIO $ maybe getULID (pure . pagoId) pago
--         -- let pagoTextUlid = pack . show $ pagoUlid
--         forM_ (zipWith (,) [0..] pagadoresToRender) $ \(index, deudor) -> do
--           parteForm "pagadores" index participantes (Just deudor)
--       div_ [] $ do
--         button_
--           [ hxPut_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeParteNew grupoId "pagadores"
--           , hxTarget_ "#pagadores"
--           , hxSwap_ "beforeend"
--           ]
--           "agregar pagadores"

--     div_ [id_ "seccion-deudores"] $ do
--       label_ "Deudores"
--       div_ [id_ "deudores"] $ do
--         forM_ (zipWith (,)  [0..] deudoresToRender) $ \(index, parte) -> do
--           -- let index = participante & participanteId & show & pack
--           parteForm "deudores" index participantes (Just parte)
        
--       div_ [] $ do
--         button_
--           [ hxPut_ $ ("/" <>) $ toUrlPiece $ fieldLink _routeParteNew grupoId "deudores"
--           , hxTarget_ "#deudores"
--           , hxSwap_ "beforeend"
--           ]
--           "agregar deudores"

--     p_ $ button_ $ maybe "Crear pago" (const "Guardar pago") pago

-- parteForm :: Monad m => Text -> Int -> [Participante] -> Maybe Parte -> HtmlT m ()
-- parteForm prefix indice participantes oldParte =
--   div_ [id_ [i|distribucion-#{prefix}-#{indice}|]] $ do
--     select_ [value_ "ponderado", name_ [i|#{prefix}.#{indice}.tipo|]] $ do
--       option_
--         [ value_ "ponderado"
--         , maybe (selected_ "selected") (\case
--           Ponderado _ _ -> selected_ "selected"
--           MontoFijo _ _ -> mempty
--         ) oldParte
--         ] "Ponderado"
--       option_
--         [ value_ "fijo"
--         , maybe mempty (\case
--           Ponderado _ _ -> mempty
--           MontoFijo _ _ -> selected_ "selected"
--         ) oldParte
--         ] "Fijo"
--     input_ [name_ [i|#{prefix}.indices|], type_ "hidden", value_ [i|#{indice}|]]
--     input_
--       [ name_ [i|#{prefix}.#{indice}.monto|]
--       , maybe (value_ "1") (\case
--         Ponderado cuota _ -> value_ . pack . show $ cuota
--         MontoFijo monto _ -> value_ $ monto2Text monto
--       ) oldParte
--       ]
--     participantsSelect [i|#{prefix}.#{indice}.|] participantes $ fmap parteParticipante oldParte
--     button_ 
--       [ makeAttributes "hx-on:click" [i|this.closest('\#distribucion-#{prefix}-#{indice}').remove()|]
--       ] "borrar"

-- participantsSelect :: Monad m => Text -> [Participante] -> Maybe ParticipanteId -> HtmlT m ()
-- participantsSelect prefix participantes selectedParticipante = do
--   if null participantes
--     then
--       "No hay participantes"
--     else do
--       select_
--         [name_ [i|#{prefix}participante|]] $ do
--         forM_ participantes $ \(Participante ulid nombre) -> do
--           option_ 
--             [ value_ . pack . show $ ulid
--             , if selectedParticipante == Just ulid
--                 then selected_ "selected"
--                 else mempty
--             ] $ toHtml nombre

nt :: App -> AppHandler a -> Handler a
nt s x = runReaderT x s

app :: App -> Application
app appState =
  genericServeTWithContext
    (nt appState)
    serverT
    (EmptyContext)
