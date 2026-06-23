module Pages.Grupos.GrupoId_.Pagos.New exposing (Model, Msg, ReceiptReadingState, Section(..), andThenSendWarningOnExit, page, subscriptions, update, validatePago, validatePagoInSection, view, waitAndCheckNecessaryData)

import Base64.Encode
import Browser.Dom
import Bytes exposing (Bytes)
import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.Bootstrap as Bs
import Css
import Date exposing (Date)
import Effect exposing (Effect)
import File exposing (File)
import Form exposing (Form)
import Form.Error as FormError
import Form.Field as FormField
import Form.Init as Form
import Form.Validate as V exposing (Validation, nonEmpty)
import Generated.Api as Api exposing (Distribucion, DistribucionDeSobras(..), Moneda, Monto, Pago, Participante, ParticipanteId, Repartija, RepartijaItem, ResumenNetos, ResumenPago, TipoDistribucion(..), ULID)
import Html exposing (Html, a, button, details, div, i, li, p, span, summary, text)
import Html.Attributes as Attr exposing (accept, class, classList, disabled, placeholder, style, target, type_)
import Html.Events exposing (on, onClick, onSubmit)
import Http
import Json.Decode as Decode
import Layouts
import Models.Grupo exposing (GrupoLike)
import Models.LugarAccionable exposing (LugarParaAccionar(..))
import Models.Moneda as Moneda
import Models.Monto as Monto
import Models.ResumenNetos exposing (errorAccionableEn, errorMensaje, getDeudasFromResumen)
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import Process
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Task
import Utils.Day exposing (validateDay)
import Utils.Form exposing (CustomFormError, isDataModifyingEvent)
import Utils.Http exposing (viewHttpError)
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts
import Utils.Ulid exposing (emptyUlid)
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.grupoId shared.today shared.store
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared.store
        }
        |> Page.withLayout (\_ -> Layouts.Default {})



-- INIT


type Msg
    = NoOp
    | PagoForm Form.Msg
    | AddedPagoResponse (Result Http.Error Pago)
    | UpdatedPagoResponse (Result Http.Error Pago)
    | SelectSection Section
    | SubmitCurrentSection
    | CheckIfPagoAndGrupoArePresent
    | ResumenPagoUpdated (WebData ResumenPago)
    | ResumenDeudoresUpdated (WebData ResumenPago)
    | ResumenPagadoresUpdated (WebData ResumenPago)
    | ReceiptImageSelected File
    | ReceiptImageBytes File Bytes
    | ReceiptParseResponse (Result Http.Error Api.ReceiptImageResponse)
    | ClearReceiptError


type Section
    = BasicPagoData
    | PagadoresSection
    | DeudoresSection


type ReceiptReadingState
    = ReadingFile
    | ProcessingWithAI
    | ErrorProcessing String


type alias Model =
    { grupoId : String
    , currentPagoId : Maybe ULID
    , currentSection : Section
    , pagoBasicoForm : Form CustomFormError Pago
    , pagadoresForm : Form CustomFormError Pago
    , resumenPagadores : WebData ResumenPago
    , deudoresForm : Form CustomFormError Pago
    , resumenDeudores : WebData ResumenPago
    , pagoForm : Form CustomFormError Pago
    , resumenPago : WebData ResumenPago
    , receiptParseState : Maybe ReceiptReadingState
    , storedClaims : Maybe { pagadores : List Api.RepartijaClaim, deudores : List Api.RepartijaClaim }
    , hasUnsavedChanges : Bool
    }


init : ULID -> Date -> Store -> ( Model, Effect Msg )
init grupoId today store =
    let
        defaultFormValues =
            [ Form.setString "fecha" (Date.toIsoString today)
            , Form.setGroup "distribucion_pagadores" (pagadoresToForm [] Nothing)
            , Form.setGroup "distribucion_deudores" (deudoresToForm [] Nothing)
            ]
    in
    ( { grupoId = grupoId
      , currentPagoId = Nothing
      , currentSection = BasicPagoData
      , pagoBasicoForm = Form.initial defaultFormValues (validatePagoInSection BasicPagoData [])
      , deudoresForm = Form.initial defaultFormValues (validatePagoInSection DeudoresSection [])
      , resumenDeudores = NotAsked
      , pagadoresForm = Form.initial defaultFormValues (validatePagoInSection PagadoresSection [])
      , resumenPagadores = NotAsked
      , pagoForm = Form.initial defaultFormValues (validatePago [])
      , resumenPago = NotAsked
      , receiptParseState = Nothing
      , storedClaims = Nothing
      , hasUnsavedChanges = False
      }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Effect.getCurrentUser grupoId
        , waitAndCheckNecessaryData
        ]
    )
        |> andThenSendWarningOnExit


validatePagoInSection : Section -> List Participante -> Validation CustomFormError Pago
validatePagoInSection section participantes =
    let
        emptyDistribucion =
            { id = emptyUlid, tipo = Api.TipoDistribucionPartes { id = emptyUlid, partes = [] } }
    in
    V.succeed Pago
        |> V.andMap (V.field "id" validateId)
        |> V.andMap
            (if section == BasicPagoData then
                V.field "monto" Monto.validateMonto

             else
                V.maybe (V.field "monto" Monto.validateMonto) |> V.map (Maybe.withDefault Monto.zero)
            )
        |> V.andMap (V.field "moneda" Moneda.validate)
        |> V.andMap (V.succeed False)
        |> V.andMap
            (if section == BasicPagoData then
                V.field "nombre" (V.string |> V.andThen nonEmpty)

             else
                V.succeed ""
            )
        |> V.andMap (V.field "fecha" validateDay)
        |> V.andMap
            (if section == PagadoresSection then
                V.field "distribucion_pagadores" <| validateDistribucion participantes

             else
                V.succeed emptyDistribucion
            )
        |> V.andMap
            (if section == DeudoresSection then
                V.field "distribucion_deudores" <| validateDistribucion participantes

             else
                V.succeed emptyDistribucion
            )


validatePago : List Participante -> Validation CustomFormError Pago
validatePago participantes =
    V.succeed Pago
        |> V.andMap (V.field "id" validateId)
        |> V.andMap (V.field "monto" Monto.validateMonto)
        |> V.andMap (V.field "moneda" Moneda.validate)
        |> V.andMap (V.succeed False)
        |> V.andMap (V.field "nombre" (V.string |> V.andThen nonEmpty))
        |> V.andMap (V.field "fecha" validateDay)
        |> V.andMap (V.field "distribucion_pagadores" <| validateDistribucion participantes)
        |> V.andMap (V.field "distribucion_deudores" <| validateDistribucion participantes)


validateId : Validation CustomFormError ULID
validateId =
    V.defaultValue emptyUlid V.string


distribucionDeSobrasToString : DistribucionDeSobras -> String
distribucionDeSobrasToString distribucionDeSobras =
    case distribucionDeSobras of
        SobrasNoDistribuir ->
            "SobrasNoDistribuir"

        SobrasProporcional ->
            "SobrasProporcional"


validateRepartija : V.Validation CustomFormError Repartija
validateRepartija =
    V.succeed Repartija
        |> V.andMap (V.field "repartija_id" validateId)
        |> V.andMap (V.succeed "GENERATED")
        |> V.andMap (V.field "extra" Monto.validateMonto)
        |> V.andMap
            (V.field "distribucionDeSobras"
                (V.customValidation V.string
                    (\t ->
                        case t of
                            "SobrasNoDistribuir" ->
                                Ok SobrasNoDistribuir

                            "SobrasProporcional" ->
                                Ok SobrasProporcional

                            _ ->
                                Err <| FormError.value FormError.InvalidString
                    )
                )
            )
        |> V.andMap (V.field "items" (V.list validateRepartijaItem))
        |> V.andMap (V.field "claims" (V.succeed []))


validateRepartijaItem : V.Validation CustomFormError RepartijaItem
validateRepartijaItem =
    V.succeed RepartijaItem
        |> V.andMap (V.field "id" validateId)
        |> V.andMap (V.field "nombre" V.string)
        |> V.andMap (V.field "monto" Monto.validateMonto)
        |> V.andMap (V.field "cantidad" V.int)


validateDistribucion : List Participante -> Validation CustomFormError Distribucion
validateDistribucion participantes =
    V.succeed Distribucion
        |> V.andMap (V.field "id" validateId)
        |> V.andMap
            (V.field "tipo" V.string
                |> V.andThen
                    (\t ->
                        case t of
                            "repartija" ->
                                validateRepartija
                                    |> V.map Api.TipoDistribucionRepartija

                            "partes" ->
                                readModoPartes
                                    |> V.andThen
                                        (\mode ->
                                            V.succeed Api.DistribucionPartes
                                                |> V.andMap (V.field "partes_id" validateId)
                                                |> V.andMap
                                                    (V.field "partes"
                                                        (V.sequence
                                                            (participantes
                                                                |> List.map (\participante -> V.field participante.id (validateParte mode participante.id))
                                                            )
                                                            |> V.map (List.filterMap identity)
                                                        )
                                                    )
                                                |> V.map Api.TipoDistribucionPartes
                                        )

                            _ ->
                                V.fail <| FormError.value FormError.Empty
                    )
            )


{-| Qué columnas del reparto por partes se tienen en cuenta. Los flags viven
dentro del propio formulario (`mostrar_partes` / `mostrar_monto_fijo`), así que
los valores de partes y monto fijo siempre quedan guardados; estos flags sólo
deciden cuáles se ignoran al construir las `Parte`. Con ambos en `False` el
gasto se reparte en partes iguales.
-}
type alias ModoPartes =
    { mostrarPartes : Bool
    , mostrarMontoFijo : Bool
    }


mostrarPartesField : String
mostrarPartesField =
    "mostrar_partes"


mostrarMontoFijoField : String
mostrarMontoFijoField =
    "mostrar_monto_fijo"


readModoPartes : Validation CustomFormError ModoPartes
readModoPartes =
    V.succeed ModoPartes
        |> V.andMap (V.field mostrarPartesField (V.defaultValue False V.bool))
        |> V.andMap (V.field mostrarMontoFijoField (V.defaultValue False V.bool))


validateParte : ModoPartes -> ParticipanteId -> Validation CustomFormError (Maybe Api.Parte)
validateParte mode participanteId =
    V.defaultValue False (V.field "incluido" V.bool)
        |> V.andThen
            (\incluido ->
                if incluido then
                    V.succeed Tuple.pair
                        |> V.andMap
                            (if mode.mostrarMontoFijo then
                                V.maybe (V.field "monto" Monto.validateMonto)

                             else
                                V.succeed Nothing
                            )
                        |> V.andMap
                            (if mode.mostrarPartes then
                                V.maybe (V.field "cuota" (V.int |> V.andThen (V.minInt 1)))

                             else
                                V.succeed Nothing
                            )
                        |> V.andThen
                            (\( maybeMonto, maybeCuota ) ->
                                case ( maybeMonto, maybeCuota ) of
                                    ( Just monto, Just cuota ) ->
                                        V.succeed (Just (Api.PonderadoYMontoFijo monto cuota participanteId))

                                    ( Just monto, Nothing ) ->
                                        V.succeed (Just (Api.MontoFijo monto participanteId))

                                    ( Nothing, Just cuota ) ->
                                        V.succeed (Just (Api.Ponderado cuota participanteId))

                                    ( Nothing, Nothing ) ->
                                        if not mode.mostrarPartes && not mode.mostrarMontoFijo then
                                            -- En partes iguales: cada incluido aporta una parte.
                                            V.succeed (Just (Api.Ponderado 1 participanteId))

                                        else
                                            V.fail <| FormError.value FormError.Empty
                            )

                else
                    V.succeed Nothing
            )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    let
        store =
            shared.store

        participantes =
            case Store.getGrupo model.grupoId store of
                Success grupo ->
                    grupo.participantes

                Failure _ ->
                    []

                NotAsked ->
                    []

                Loading ->
                    []
    in
    case msg of
        NoOp ->
            ( model, Effect.none )

        AddedPagoResponse (Ok pago) ->
            let
                newModel =
                    initializePagoForms pago.moneda participantes (Just pago) model
            in
            ( { newModel | hasUnsavedChanges = False }
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshPagos model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Se creó el pago"
                , Effect.pushRoutePath <| Path.Grupos_Id_ { id = model.grupoId }
                ]
            )
                |> andThenSendWarningOnExit

        AddedPagoResponse (Err _) ->
            ( model
            , Effect.batch
                [ Toasts.pushToast Toasts.ToastDanger "Falló la creación del pago"
                , Store.refreshPagos model.grupoId
                ]
            )

        UpdatedPagoResponse (Ok pago) ->
            let
                newModel =
                    initializePagoForms pago.moneda participantes (Just pago) model
            in
            ( newModel
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshPagos model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Se actualizó el pago"
                ]
            )
                |> andThenUpdateResumenesFromForms model
                |> andThenSendWarningOnExit

        UpdatedPagoResponse (Err _) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "Falló la actualización del pago"
            )

        PagoForm Form.Submit ->
            case ( Form.getOutput model.pagoForm, store |> Store.getGrupo model.grupoId ) of
                ( Just pago, Success { id } ) ->
                    let
                        pagoWithClaims =
                            mergeClaimsIntoPago model.storedClaims pago
                    in
                    case model.currentPagoId of
                        Just pagoId ->
                            ( { model
                                | pagoForm = Form.update (validatePago participantes) Form.Submit model.pagoForm
                              }
                            , Effect.sendCmd <|
                                Api.putGrupoByIdPagosByPagoId
                                    id
                                    pagoId
                                    pagoWithClaims
                                    UpdatedPagoResponse
                            )

                        Nothing ->
                            ( { model
                                | pagoForm = Form.update (validatePago participantes) Form.Submit model.pagoForm
                              }
                            , Effect.sendCmd <|
                                Api.postGrupoByIdPagos
                                    id
                                    pagoWithClaims
                                    AddedPagoResponse
                            )

                _ ->
                    ( { model
                        | pagoForm = Form.update (validatePago participantes) Form.Submit model.pagoForm
                      }
                    , Effect.none
                    )

        PagoForm formMsg ->
            let
                newModel =
                    updateAllForms participantes [ formMsg ] model
            in
            ( { newModel
                | hasUnsavedChanges =
                    if isDataModifyingEvent formMsg then
                        True

                    else
                        model.hasUnsavedChanges
              }
            , Effect.none
            )
                |> andThenUpdateResumenesFromForms model
                |> andThenSendWarningOnExit

        ResumenPagoUpdated resumen ->
            ( { model | resumenPago = resumen }
            , Effect.none
            )

        ResumenDeudoresUpdated resumen ->
            ( { model | resumenDeudores = resumen }
            , Effect.none
            )

        ResumenPagadoresUpdated resumen ->
            ( { model | resumenPagadores = resumen }
            , Effect.none
            )

        ClearReceiptError ->
            ( { model | receiptParseState = Nothing }
            , Effect.none
            )

        ReceiptImageSelected file ->
            if List.member (File.mime file) allowedMimeTypesForReceiptUpload then
                ( { model | receiptParseState = Just ReadingFile }
                , Effect.sendCmd <| Task.perform (ReceiptImageBytes file) (File.toBytes file)
                )

            else
                ( { model | receiptParseState = Just <| ErrorProcessing <| "Este archivo no es una imagen (" ++ File.mime file ++ ")" }
                , Effect.none
                )

        ReceiptImageBytes file bytes ->
            let
                base64 =
                    Base64.Encode.encode (Base64.Encode.bytes bytes)
            in
            ( { model | receiptParseState = Just ProcessingWithAI }
            , Effect.sendCmd <| Api.postReceiptParseimage { imageBase64 = File.mime file ++ ";base64," ++ base64 } ReceiptParseResponse
            )

        ReceiptParseResponse result ->
            case result of
                Ok (Api.ReceiptImageSuccess { items }) ->
                    let
                        formMsgs =
                            Form.Input "distribucion_deudores.tipo" Form.Select (FormField.String "repartija")
                                :: receiptItemsFormMsgs "distribucion_deudores" items model.pagoForm

                        newModel =
                            updateAllForms participantes formMsgs { model | hasUnsavedChanges = True }
                    in
                    ( { newModel
                        | receiptParseState = Nothing
                      }
                    , Toasts.pushToast Toasts.ToastSuccess "Recibo parseado correctamente"
                    )
                        |> andThenUpdateResumenesFromForms model
                        |> andThenSendWarningOnExit

                Ok (Api.ReceiptImageError { error }) ->
                    ( { model | receiptParseState = Just (ErrorProcessing error) }
                    , Effect.none
                    )

                Err _ ->
                    ( { model | receiptParseState = Just (ErrorProcessing "Error al enviar la imagen") }
                    , Effect.none
                    )

        SelectSection section ->
            ( { model | currentSection = section }
            , Effect.none
            )
                |> andThenFocusFieldIfSectionChanged model.currentSection
                |> andThenUpdateResumenesFromForms model

        SubmitCurrentSection ->
            case model.currentSection of
                BasicPagoData ->
                    let
                        -- si todavía no hay pagadores arrancamos con el usuario
                        -- actual pagando el total
                        hayPagadores =
                            participantes
                                |> List.any
                                    (\participante ->
                                        (Form.getFieldAsBool ("distribucion_pagadores.partes." ++ participante.id ++ ".incluido") model.pagoForm).value == Just True
                                    )

                        autoAddMsgs =
                            if hayPagadores then
                                []

                            else
                                shared.userId
                                    |> Maybe.andThen
                                        (\userId ->
                                            participantes
                                                |> List.filter (\participante -> participante.id == userId)
                                                |> List.head
                                        )
                                    |> Maybe.map
                                        (\participante ->
                                            let
                                                basePath =
                                                    "distribucion_pagadores.partes." ++ participante.id

                                                totalRaw =
                                                    Form.getOutput model.pagoBasicoForm
                                                        |> Maybe.map (.monto >> Monto.toRawString)
                                                        |> Maybe.withDefault ""
                                            in
                                            [ Form.Input (basePath ++ ".incluido") Form.Checkbox (FormField.Bool True)
                                            , Form.Input (basePath ++ ".monto") Form.Text (FormField.String totalRaw)
                                            ]
                                        )
                                    |> Maybe.withDefault []

                        newModel =
                            updateAllForms participantes
                                autoAddMsgs
                                { model
                                    | pagoBasicoForm = Form.update (validatePagoInSection BasicPagoData participantes) Form.Submit model.pagoBasicoForm
                                    , currentSection = PagadoresSection
                                    , hasUnsavedChanges = model.hasUnsavedChanges || not (List.isEmpty autoAddMsgs)
                                }
                    in
                    ( newModel, Effect.none )
                        |> andThenFocusFieldIfSectionChanged model.currentSection
                        |> andThenUpdateResumenesFromForms model
                        |> andThenSendWarningOnExit

                PagadoresSection ->
                    ( { model
                        | pagadoresForm = Form.update (validatePagoInSection PagadoresSection participantes) Form.Submit model.pagadoresForm
                        , currentSection = DeudoresSection
                      }
                    , Effect.none
                    )
                        |> andThenFocusFieldIfSectionChanged model.currentSection
                        |> andThenUpdateResumenesFromForms model
                        |> andThenSendWarningOnExit

                DeudoresSection ->
                    update shared (PagoForm Form.Submit) model

        CheckIfPagoAndGrupoArePresent ->
            case model.currentPagoId of
                Nothing ->
                    case Store.getGrupo model.grupoId store of
                        NotAsked ->
                            ( model, waitAndCheckNecessaryData )

                        Loading ->
                            ( model, waitAndCheckNecessaryData )

                        Failure _ ->
                            ( model, Effect.none )

                        Success grupo ->
                            let
                                newModel =
                                    initializePagoForms grupo.monedaPorDefecto grupo.participantes Nothing model
                            in
                            ( { newModel
                                | storedClaims = Nothing
                              }
                            , Effect.none
                            )
                                |> andThenUpdateResumenesFromForms model
                                |> andThenSendWarningOnExit

                Just pagoId ->
                    case ( Store.getGrupo model.grupoId store, Store.getPago pagoId store ) of
                        ( NotAsked, _ ) ->
                            ( model, waitAndCheckNecessaryData )

                        ( _, NotAsked ) ->
                            ( model, waitAndCheckNecessaryData )

                        ( Loading, _ ) ->
                            ( model, waitAndCheckNecessaryData )

                        ( _, Loading ) ->
                            ( model, waitAndCheckNecessaryData )

                        ( Failure _, _ ) ->
                            ( model, Effect.none )

                        ( _, Failure _ ) ->
                            ( model, Effect.none )

                        ( Success grupo, Success pago ) ->
                            let
                                newModel =
                                    initializePagoForms grupo.monedaPorDefecto grupo.participantes (Just pago) model
                            in
                            ( newModel
                            , Effect.none
                            )
                                |> andThenUpdateResumenesFromForms model
                                |> andThenSendWarningOnExit


updateAllForms : List Participante -> List Form.Msg -> Model -> Model
updateAllForms participantes formMsgs model =
    List.foldl
        (\formMsg m ->
            { m
                | pagoForm = Form.update (validatePago participantes) formMsg m.pagoForm
                , pagoBasicoForm = Form.update (validatePagoInSection BasicPagoData participantes) formMsg m.pagoBasicoForm
                , pagadoresForm = Form.update (validatePagoInSection PagadoresSection participantes) formMsg m.pagadoresForm
                , deudoresForm = Form.update (validatePagoInSection DeudoresSection participantes) formMsg m.deudoresForm
            }
        )
        model
        formMsgs


andThenSendWarningOnExit : ( Model, Effect Msg ) -> ( Model, Effect Msg )
andThenSendWarningOnExit ( model, oldEffects ) =
    ( model
    , Effect.batch
        [ oldEffects
        , Effect.setUnsavedChangesWarning model.hasUnsavedChanges
        ]
    )


initializePagoForms : Moneda -> List Participante -> Maybe Pago -> Model -> Model
initializePagoForms monedaPorDefecto participantes pago model =
    let
        monedaInicial =
            pago |> Maybe.map .moneda |> Maybe.withDefault monedaPorDefecto

        initialFormValues =
            [ Form.setString "id" (pago |> Maybe.map .pagoId |> Maybe.withDefault "")
            , Form.setString "nombre" (pago |> Maybe.map .nombre |> Maybe.withDefault "")
            , Form.setString "monto" (pago |> Maybe.map (.monto >> Monto.toRawString) |> Maybe.withDefault "")
            , Form.setString "moneda" (Moneda.toString monedaInicial)
            , Form.setString "fecha"
                (pago
                    |> Maybe.map (.fecha >> Date.toIsoString)
                    |> Maybe.withDefault (Form.getFieldAsString "fecha" model.pagoForm |> .value |> Maybe.withDefault "")
                )
            , Form.setGroup "distribucion_pagadores" <|
                pagadoresToForm participantes (Maybe.map .pagadores pago)
            , Form.setGroup "distribucion_deudores" <|
                deudoresToForm participantes (Maybe.map .deudores pago)
            ]

        claimsToStore =
            pago
                |> Maybe.map
                    (\p ->
                        { pagadores = extractClaimsFromDistribucion p.pagadores
                        , deudores = extractClaimsFromDistribucion p.deudores
                        }
                    )
    in
    { model
        | pagoForm = Form.initial initialFormValues (validatePago participantes)
        , pagadoresForm = Form.initial initialFormValues (validatePagoInSection PagadoresSection participantes)
        , deudoresForm = Form.initial initialFormValues (validatePagoInSection DeudoresSection participantes)
        , pagoBasicoForm = Form.initial initialFormValues (validatePagoInSection BasicPagoData participantes)
        , storedClaims = claimsToStore
        , hasUnsavedChanges = False
    }


{-| This is a bit of a hack, we need to wait for the `Grupo` AND possibly the `Pago` (if we are editing) before
creating the ui and we don't get notified by the store when that happens so we "poll"
until we see that the store has the values we need
-}
waitAndCheckNecessaryData : Effect Msg
waitAndCheckNecessaryData =
    Effect.sendCmd <| Task.perform (\_ -> CheckIfPagoAndGrupoArePresent) (Process.sleep 100)


receiptItemsFormMsgs : String -> List Api.RepartijaItem -> Form CustomFormError Pago -> List Form.Msg
receiptItemsFormMsgs prefix items form =
    let
        startingIndex =
            List.length (Form.getListIndexes (prefix ++ ".items") form)
    in
    items
        |> List.indexedMap
            (\idx item ->
                let
                    itemPrefix =
                        prefix ++ ".items." ++ String.fromInt (startingIndex + idx)
                in
                [ Form.Append (prefix ++ ".items")
                , Form.Input (itemPrefix ++ ".id") Form.Text (FormField.String emptyUlid)
                , Form.Input (itemPrefix ++ ".nombre") Form.Text (FormField.String item.nombre)
                , Form.Input (itemPrefix ++ ".monto") Form.Text (FormField.String (Monto.toRawString item.monto))
                , Form.Input (itemPrefix ++ ".cantidad") Form.Text (FormField.String (String.fromInt item.cantidad))
                ]
            )
        |> List.concat


extractClaimsFromDistribucion : Distribucion -> List Api.RepartijaClaim
extractClaimsFromDistribucion distribucion =
    case distribucion.tipo of
        TipoDistribucionRepartija repartija ->
            repartija.claims

        _ ->
            []


mergeClaimsIntoPago : Maybe { pagadores : List Api.RepartijaClaim, deudores : List Api.RepartijaClaim } -> Pago -> Pago
mergeClaimsIntoPago maybeClaims pago =
    case maybeClaims of
        Nothing ->
            pago

        Just claims ->
            { pago
                | pagadores = mergeClaimsIntoDistribucion claims.pagadores pago.pagadores
                , deudores = mergeClaimsIntoDistribucion claims.deudores pago.deudores
            }


mergeClaimsIntoDistribucion : List Api.RepartijaClaim -> Distribucion -> Distribucion
mergeClaimsIntoDistribucion claims distribucion =
    case distribucion.tipo of
        TipoDistribucionRepartija repartija ->
            { distribucion
                | tipo = TipoDistribucionRepartija { repartija | claims = claims }
            }

        _ ->
            distribucion


parteParticipante : Api.Parte -> ParticipanteId
parteParticipante parte =
    case parte of
        Api.MontoFijo _ participanteId ->
            participanteId

        Api.Ponderado _ participanteId ->
            participanteId

        Api.PonderadoYMontoFijo _ _ participanteId ->
            participanteId


defaultRepartija : Repartija
defaultRepartija =
    { id = emptyUlid
    , nombre = "GENERATED"
    , items = []
    , claims = []
    , extra = Monto.zero
    , distribucionDeSobras = SobrasNoDistribuir
    }


{-| Los pagadores se editan con la misma distribución por partes que los
deudores; sólo cambian los valores por defecto: arrancan en modo "monto fijo" y
sin nadie preseleccionado (se agrega al usuario actual al avanzar desde el paso
del monto). Las distribuciones existentes se convierten conservando sus partes.
-}
pagadoresToForm : List Participante -> Maybe Distribucion -> List ( String, FormField.Field )
pagadoresToForm participantes distribucion =
    let
        modo =
            case distribucion |> Maybe.map .tipo of
                Just (TipoDistribucionPartes _) ->
                    deriveModoPartes distribucion

                _ ->
                    { mostrarPartes = False, mostrarMontoFijo = True }

        partes =
            case distribucion |> Maybe.map .tipo of
                Just (TipoDistribucionPartes p) ->
                    p

                _ ->
                    { id = emptyUlid, partes = [] }
    in
    [ Form.setString "id" (distribucion |> Maybe.map .id |> Maybe.withDefault emptyUlid)
    , Form.setBool mostrarPartesField modo.mostrarPartes
    , Form.setBool mostrarMontoFijoField modo.mostrarMontoFijo
    ]
        ++ partesToForm participantes partes


{-| Deduce qué columnas mostrar (partes / monto fijo) a partir de las `Parte`
ya guardadas. Si todas son `Ponderado 1` se asume "en partes iguales" (ambos
flags en `False`).
-}
deriveModoPartes : Maybe Distribucion -> ModoPartes
deriveModoPartes distribucion =
    case distribucion |> Maybe.map .tipo of
        Just (TipoDistribucionPartes partes) ->
            { mostrarPartes =
                partes.partes
                    |> List.any
                        (\parte ->
                            case parte of
                                Api.Ponderado cuota _ ->
                                    cuota /= 1

                                Api.PonderadoYMontoFijo _ _ _ ->
                                    True

                                Api.MontoFijo _ _ ->
                                    False
                        )
            , mostrarMontoFijo =
                partes.partes
                    |> List.any
                        (\parte ->
                            case parte of
                                Api.MontoFijo _ _ ->
                                    True

                                Api.PonderadoYMontoFijo _ _ _ ->
                                    True

                                Api.Ponderado _ _ ->
                                    False
                        )
            }

        _ ->
            { mostrarPartes = False, mostrarMontoFijo = False }


deudoresToForm : List Participante -> Maybe Distribucion -> List ( String, FormField.Field )
deudoresToForm participantes distribucion =
    let
        modo =
            deriveModoPartes distribucion

        partesEquitativas incluidos =
            { id = emptyUlid
            , partes = incluidos |> List.map (Api.Ponderado 1)
            }

        -- En un pago nuevo no elegimos modalidad por defecto: el usuario tiene
        -- que optar explícitamente entre "Clásica" (partes) y "Repartija". Al
        -- editar, arrancamos con la que ya tenía guardada.
        tipoInicial =
            case distribucion |> Maybe.map .tipo of
                Just (TipoDistribucionRepartija _) ->
                    "repartija"

                Just (TipoDistribucionPartes _) ->
                    "partes"

                Nothing ->
                    ""
    in
    [ Form.setString "id" (distribucion |> Maybe.map .id |> Maybe.withDefault emptyUlid)
    , Form.setBool mostrarPartesField modo.mostrarPartes
    , Form.setBool mostrarMontoFijoField modo.mostrarMontoFijo
    ]
        ++ (case distribucion |> Maybe.map .tipo of
                Just (TipoDistribucionRepartija repartija) ->
                    partesToForm participantes (partesEquitativas (participantes |> List.map .id))
                        ++ repartijaToForm repartija

                Just (TipoDistribucionPartes partes) ->
                    repartijaToForm defaultRepartija
                        ++ partesToForm participantes partes

                Nothing ->
                    repartijaToForm defaultRepartija
                        ++ partesToForm participantes (partesEquitativas (participantes |> List.map .id))
           )
        -- `partesToForm`/`repartijaToForm` fijan su propio "tipo"; lo
        -- sobreescribimos al final para que mande `tipoInicial`.
        ++ [ Form.setString "tipo" tipoInicial ]


partesToForm : List Participante -> Api.DistribucionPartes -> List ( String, FormField.Field )
partesToForm participantes distribucion =
    [ Form.setString "partes_id" distribucion.id
    , Form.setString "tipo" "partes"
    , Form.setGroup "partes"
        (participantes
            |> List.map
                (\participante ->
                    let
                        campos =
                            case
                                distribucion.partes
                                    |> List.filter (\parte -> parteParticipante parte == participante.id)
                                    |> List.head
                            of
                                Nothing ->
                                    { incluido = False, cuota = "1", monto = "" }

                                Just (Api.Ponderado cuota _) ->
                                    { incluido = True, cuota = String.fromInt cuota, monto = "" }

                                Just (Api.MontoFijo monto _) ->
                                    { incluido = True, cuota = "1", monto = Monto.toRawString monto }

                                Just (Api.PonderadoYMontoFijo monto cuota _) ->
                                    { incluido = True, cuota = String.fromInt cuota, monto = Monto.toRawString monto }
                    in
                    Form.setGroup participante.id
                        [ Form.setBool "incluido" campos.incluido
                        , Form.setString "cuota" campos.cuota
                        , Form.setString "monto" campos.monto
                        ]
                )
        )
    ]


repartijaToForm : Repartija -> List ( String, FormField.Field )
repartijaToForm repartija =
    [ Form.setString "repartija_id" repartija.id
    , Form.setString "tipo" "repartija"
    , Form.setString "extra" (Monto.toRawString repartija.extra)
    , Form.setString "distribucionDeSobras"
        (distribucionDeSobrasToString repartija.distribucionDeSobras)
    , Form.setList "items"
        (repartija.items
            |> List.map
                (\item ->
                    FormField.group
                        [ Form.setString "id" <| item.id
                        , Form.setString "monto" <| Monto.toRawString item.monto
                        , Form.setString "cantidad" <| String.fromInt item.cantidad
                        , Form.setString "nombre" item.nombre
                        ]
                )
        )
    ]


andThenFocusFieldIfSectionChanged : Section -> ( Model, Effect Msg ) -> ( Model, Effect Msg )
andThenFocusFieldIfSectionChanged oldSection ( model, oldEffects ) =
    let
        focusEffect =
            if oldSection == model.currentSection then
                Effect.none

            else
                case model.currentSection of
                    BasicPagoData ->
                        Effect.sendCmd <| Task.attempt (\_ -> NoOp) (Browser.Dom.focus "nombre")

                    PagadoresSection ->
                        Effect.sendCmd <| Task.attempt (\_ -> NoOp) (Browser.Dom.focus "pagadores-seleccionar")

                    DeudoresSection ->
                        Effect.sendCmd <| Task.attempt (\_ -> NoOp) (Browser.Dom.focus "deudores-modalidad")
    in
    ( model
    , Effect.batch [ oldEffects, focusEffect ]
    )


andThenUpdateResumenesFromForms : Model -> ( Model, Effect Msg ) -> ( Model, Effect Msg )
andThenUpdateResumenesFromForms originalModel ( model, oldEffects ) =
    let
        updateResumenFromForm getForm event =
            case Form.getOutput (getForm model) of
                Just pago ->
                    if Form.getOutput (getForm originalModel) == Just pago then
                        Effect.none

                    else
                        let
                            pagoWithClaims =
                                mergeClaimsIntoPago model.storedClaims pago
                        in
                        Effect.batch
                            [ Effect.sendMsg <| event Loading
                            , Effect.sendCmd <| Api.postPagosResumen pagoWithClaims (RemoteData.fromResult >> event)
                            ]

                Nothing ->
                    Effect.sendMsg <| event NotAsked
    in
    ( model
    , Effect.batch
        [ oldEffects
        , updateResumenFromForm .pagoForm ResumenPagoUpdated
        , updateResumenFromForm .pagadoresForm ResumenPagadoresUpdated
        , updateResumenFromForm .deudoresForm ResumenDeudoresUpdated
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


hasActionableErrors : LugarParaAccionar -> WebData ResumenPago -> (ResumenPago -> ResumenNetos) -> Bool
hasActionableErrors lugar resumenData accessor =
    case resumenData of
        Success resumen ->
            List.any (\e -> List.member lugar (errorAccionableEn e.tipo)) (accessor resumen).errores

        _ ->
            False


viewErrorFromResumenData : WebData ResumenPago -> (ResumenPago -> ResumenNetos) -> Html msg
viewErrorFromResumenData resumenData accessor =
    case resumenData of
        Success resumenPago ->
            let
                resumen =
                    accessor resumenPago
            in
            viewErrorFromResumen Lugar_CreacionPago resumen

        _ ->
            text ""


viewErrorFromResumen : LugarParaAccionar -> ResumenNetos -> Html msg
viewErrorFromResumen lugar resumen =
    case resumen.errores of
        [] ->
            text ""

        _ ->
            div []
                (resumen.errores
                    |> List.map
                        (\error ->
                            let
                                esAccionable =
                                    List.member lugar (errorAccionableEn error.tipo)

                                mensaje =
                                    errorMensaje error.tipo
                            in
                            Bs.alert
                                (if esAccionable then
                                    Bs.AlertDanger

                                 else
                                    Bs.AlertInfo
                                )
                                [ style "margin-bottom" "0.5rem" ]
                                [ case error.objeto of
                                    [] ->
                                        text mensaje

                                    _ ->
                                        div [ style "display" "flex", style "gap" "0.5rem", style "align-items" "baseline" ]
                                            [ Html.strong [] [ text (String.join " > " error.objeto ++ ":") ]
                                            , text mensaje
                                            ]
                                ]
                        )
                )


viewUnsavedChangesBanner : Html Msg
viewUnsavedChangesBanner =
    Bs.alert Bs.AlertWarning
        [ style "margin-bottom" "1rem" ]
        [ text "Hay cambios sin guardar" ]


{-| Pie de acciones de cada paso (errores + botón principal). En mobile queda
flotando al fondo de la pantalla; en desktop fluye al final del formulario. El
estilo vive en `styles.css` y se referencia vía `Css.action_footer` para que el
compilador avise si la clase deja de existir.
-}
viewActionFooter : List (Html Msg) -> Html Msg
viewActionFooter children =
    div [ Css.action_footer ] children


view : Store -> Model -> View Msg
view store model =
    case store |> Store.getGrupo model.grupoId of
        Success grupo ->
            { title = grupo.nombre
            , body =
                [ div [ class "row justify-content-center" ]
                    [ div [ class "col-12 col-md-8 col-lg-6 col-xxl-5" ]
                        [ if model.hasUnsavedChanges then
                            viewUnsavedChangesBanner

                          else
                            text ""
                        , viewStepTabs model
                        , case model.currentSection of
                            BasicPagoData ->
                                viewBasicSection model

                            PagadoresSection ->
                                viewPagadoresSection grupo model

                            DeudoresSection ->
                                viewDeudoresSection grupo model
                        ]
                    ]
                ]
            }

        NotAsked ->
            { title = "BananaSplit"
            , body = [ text "Cargando" ]
            }

        Loading ->
            { title = "BananaSplit"
            , body = [ text "Cargando" ]
            }

        Failure e ->
            { title = "BananaSplit"
            , body =
                [ details []
                    [ summary []
                        [ text "Algo salio mal" ]
                    , viewHttpError e
                    ]
                ]
            }


{-| Wizard de pasos como tabs por defecto de Bootstrap (`nav-tabs`). Marca el
paso actual y permite saltar a cualquier paso con `SelectSection`.
-}
viewStepTabs : Model -> Html Msg
viewStepTabs model =
    let
        tab section label =
            let
                active =
                    model.currentSection == section
            in
            li [ class "nav-item" ]
                [ button
                    [ type_ "button"
                    , classList
                        [ ( "nav-link", True )
                        , ( "active", active )

                        -- Paso incompleto (todavía sin completar): se ve grisado
                        -- en vez de como error, así no aparece "en rojo" al inicio.
                        , ( "text-body-tertiary", not active && sectionIncomplete model section )
                        ]
                    , onClick (SelectSection section)
                    ]
                    (text label
                        :: (if not (sectionIncomplete model section) && sectionHasError model section then
                                [ i [ class "bi bi-exclamation-circle-fill text-danger ms-1" ] [] ]

                            else
                                []
                           )
                    )
                ]
    in
    Html.ul [ class "nav nav-tabs mb-4" ]
        [ tab BasicPagoData "Gasto"
        , tab PagadoresSection "Pago"
        , tab DeudoresSection "Reparto"
        ]


{-| Un paso está incompleto cuando su form todavía no produce un valor válido.
Se muestra grisado (no como error) para no alarmar al inicio.
-}
sectionIncomplete : Model -> Section -> Bool
sectionIncomplete model section =
    let
        -- Sin el total del gasto (la sección básica) los pasos siguientes no se
        -- pueden evaluar: el monto en Pago/Reparto es opcional y cae a cero, así
        -- que el resumen daría "no cuadra" antes de que se cargue el total.
        faltaTotal =
            Form.getOutput model.pagoBasicoForm == Nothing
    in
    case section of
        BasicPagoData ->
            faltaTotal

        PagadoresSection ->
            faltaTotal || Form.getOutput model.pagadoresForm == Nothing

        DeudoresSection ->
            faltaTotal || Form.getOutput model.deudoresForm == Nothing


{-| Un paso tiene error sólo cuando ya tiene datos cargados pero su resumen
marca errores accionables (p. ej. los montos no cuadran). Un paso simplemente
incompleto no cuenta como error: se muestra grisado vía `sectionIncomplete`.
-}
sectionHasError : Model -> Section -> Bool
sectionHasError model section =
    case section of
        BasicPagoData ->
            False

        PagadoresSection ->
            hasActionableErrors Lugar_CreacionPago model.resumenPagadores .resumenPagadores

        DeudoresSection ->
            hasActionableErrors Lugar_CreacionPago model.resumenDeudores .resumenDeudores


viewStepHeader : Model -> { title : String, showMonto : Bool } -> Html Msg
viewStepHeader model opts =
    div [ class "mb-4" ]
        [ div [ class "d-flex justify-content-between align-items-start gap-3" ]
            [ Html.h2 [ class "mb-0" ] [ text opts.title ]
            , if opts.showMonto then
                viewMontoChip model

              else
                text ""
            ]
        ]


viewMontoChip : Model -> Html Msg
viewMontoChip model =
    case Form.getOutput model.pagoBasicoForm of
        Just pago ->
            div [ class "text-end" ]
                [ div [ class "text-body-secondary text-uppercase", style "font-size" "0.75rem" ] [ text "Monto" ]
                , div [ class "fw-bold" ] [ text (Moneda.simboloUnico pago.moneda ++ " " ++ Monto.toString pago.monto) ]
                ]

        Nothing ->
            text ""


viewBasicSection : Model -> Html Msg
viewBasicSection model =
    let
        form =
            model.pagoBasicoForm

        nombreField =
            Form.getFieldAsString "nombre" form

        montoField =
            Form.getFieldAsString "monto" form

        monedaField =
            Form.getFieldAsString "moneda" form

        fechaField =
            Form.getFieldAsString "fecha" form
    in
    div []
        [ viewStepHeader model
            { title =
                case model.currentPagoId of
                    Nothing ->
                        "Gasto compartido"

                    Just _ ->
                        "Editar gasto"
            , showMonto = False
            }
        , Html.form [ onSubmit SubmitCurrentSection ]
            [ Html.map PagoForm <|
                Bs.textFormItem nombreField
                    { label = "Título"
                    , placeholder = Just "Restaurant El Oso Pardo"
                    , required = True
                    }
            , div [ class "d-flex gap-3 flex-wrap align-items-start" ]
                [ div [ class "flex-grow-1 mb-3" ]
                    [ Html.label [ class "form-label", Attr.for "monto" ]
                        [ text "Monto total", Bs.requiredMarker True ]
                    , div [ class "d-flex gap-2" ]
                        [ Html.map PagoForm <|
                            Bs.selectInput
                                (Moneda.todas |> List.map (\m -> ( Moneda.toString m, Moneda.simboloUnico m )))
                                monedaField
                                [ style "max-width" "6.5rem" ]
                        , div [ class "flex-grow-1" ]
                            [ Html.map PagoForm <|
                                Bs.montoInput montoField [ placeholder "33.000,00" ]
                            ]
                        ]
                    ]
                , Html.map PagoForm <|
                    Bs.dateFormItem fechaField
                        { label = "Fecha"
                        , required = True
                        }
                ]
            , viewActionFooter
                [ Bs.btn Bs.Primary
                    [ disabled (Form.getOutput form == Nothing)
                    , onClick SubmitCurrentSection
                    , class "w-100"
                    ]
                    [ text "Siguiente" ]
                ]
            ]
        ]


viewPagadoresSection : GrupoLike g -> Model -> Html Msg
viewPagadoresSection grupo model =
    let
        form =
            model.pagadoresForm

        prefix =
            "distribucion_pagadores"

        incluidos =
            grupo.participantes
                |> List.filter
                    (\participante ->
                        (Form.getFieldAsBool (prefix ++ ".partes." ++ participante.id ++ ".incluido") form).value == Just True
                    )

        mode =
            { mostrarPartes =
                (Form.getFieldAsBool (prefix ++ "." ++ mostrarPartesField) form).value == Just True
            , mostrarMontoFijo =
                (Form.getFieldAsBool (prefix ++ "." ++ mostrarMontoFijoField) form).value == Just True
            }
    in
    div []
        [ viewStepHeader model
            { title = "Pago"
            , showMonto = True
            }
        , Html.form [ onSubmit SubmitCurrentSection ]
            [ div [ class "d-flex flex-wrap align-items-center gap-2 mb-2" ]
                [ Html.h5 [ class "mb-0" ] [ text "Quienes pagaron" ]
                , viewSeleccionarParticipantes "Quienes pagaron" "pagadores-seleccionar" grupo.participantes prefix form
                ]
            , div [ class "d-flex flex-wrap gap-2 mb-3" ]
                [ viewModoChip (prefix ++ "." ++ mostrarPartesField) mode.mostrarPartes "Partes"
                , viewModoChip (prefix ++ "." ++ mostrarMontoFijoField) mode.mostrarMontoFijo "Monto fijo"
                ]
            , viewPartesTable prefix mode incluidos (Form.getOutput form |> Maybe.map (\pago -> sumaMontosFijos pago.pagadores)) form
            , viewActionFooter
                [ viewErrorFromResumenData model.resumenPagadores .resumenPagadores
                , Bs.btn Bs.Primary
                    [ -- Se permite continuar aunque el gasto sea inválido (p. ej.
                      -- montos que no cuadran); sólo se bloquea si el form no es
                      -- siquiera construible.
                      disabled (Form.getOutput form == Nothing)
                    , onClick SubmitCurrentSection
                    , class "w-100"
                    ]
                    [ text "Siguiente" ]
                ]
            ]
        ]


viewDeudoresSection : GrupoLike g -> Model -> Html Msg
viewDeudoresSection grupo model =
    let
        form =
            model.deudoresForm

        tipoField =
            Form.getFieldAsString "distribucion_deudores.tipo" form
    in
    div []
        [ viewStepHeader model
            { title = "Reparto"
            , showMonto = True
            }
        , Html.form [ onSubmit <| PagoForm Form.Submit ]
            [ viewReceiptBanner model.receiptParseState
            , viewModalidadSelector tipoField
            , case tipoField.value of
                Just "repartija" ->
                    div []
                        [ viewRepartijaForm "distribucion_deudores" form
                        , viewRepartijaLink model.grupoId form
                        ]

                Just "partes" ->
                    viewPartesForm grupo "distribucion_deudores" form

                _ ->
                    text ""
            , viewBalanceCard grupo model
            , viewActionFooter
                [ -- Sólo los errores de esta sección (deudores) y sin el tag de
                  -- scope: `.resumenDeudores` ya viene sin el prefijo "deudores"
                  -- que `.resumen` agrega al combinar pagadores y deudores.
                  viewErrorFromResumenData model.resumenDeudores .resumenDeudores
                , if model.hasUnsavedChanges then
                    let
                        textoCTA =
                            case model.currentPagoId of
                                Nothing ->
                                    "Terminar"

                                Just _ ->
                                    "Actualizar pago"
                    in
                    Bs.btn Bs.Primary
                        [ -- Se permite enviar aunque el gasto sea inválido; queda
                          -- guardado con `isValid = False`. Sólo se bloquea si el
                          -- form no es construible.
                          disabled (Form.getOutput model.pagoForm == Nothing)
                        , onClick (PagoForm Form.Submit)
                        , Attr.id "pago-submit-button"
                        , class "w-100"
                        ]
                        [ text textoCTA ]

                  else
                    text ""
                ]
            ]
        ]


viewModalidadSelector : Form.FieldState CustomFormError String -> Html Msg
viewModalidadSelector tipoField =
    let
        card value icono titulo descripcion attrs =
            let
                active =
                    tipoField.value == Just value

                -- Borde resaltado con `--bs-emphasis-color`, que se invierte según
                -- el tema (casi negro en claro, casi blanco en oscuro), así se ve
                -- bien en ambos. Sin esto un borde fijo oscuro quedaría invisible
                -- sobre el fondo oscuro.
                emphasisBorder =
                    if active then
                        [ style "border-color" "var(--bs-emphasis-color)" ]

                    else
                        []
            in
            button
                ([ type_ "button"
                 , classList
                    [ ( "card flex-fill text-center border-2 position-relative", True )
                    , ( "shadow-sm", active )
                    ]
                 , onClick <| PagoForm <| Form.Input tipoField.path Form.Select (FormField.String value)
                 ]
                    ++ emphasisBorder
                    ++ attrs
                )
                [ -- El círculo del ícono se posiciona absoluto sobre el borde
                  -- superior (translate-middle lo centra ahí) y lleva fondo
                  -- `bg-body` para tapar la línea del borde por detrás.
                  span
                    [ class "position-absolute top-0 start-50 translate-middle d-inline-flex align-items-center justify-content-center rounded-circle fs-4 bg-body"
                    , style "width" "3.5rem"
                    , style "height" "3.5rem"
                    , style "border-style" "solid"
                    , style "border-width"
                        (if active then
                            "2px"

                         else
                            "var(--bs-border-width)"
                        )
                    , style "border-color"
                        (if active then
                            "var(--bs-emphasis-color)"

                         else
                            "var(--bs-border-color)"
                        )
                    ]
                    [ i [ class icono ] [] ]
                , div [ class "card-body px-2 pb-2", style "padding-top" "2.25rem" ]
                    [ Html.h6 [ class "mb-1" ] [ text titulo ]
                    , p [ class "text-body-secondary small mb-0" ] [ text descripcion ]
                    ]
                ]
    in
    div [ class "mb-4" ]
        [ Html.h5 [ class "mb-2" ] [ text "Modalidad" ]
        , div [ class "d-flex gap-3 mt-4" ]
            [ card "partes"
                "bi bi-robot"
                "Clásica"
                "Repartí de manera automática el gasto, con varias modalidades"
                [ Attr.id "deudores-modalidad" ]
            , card "repartija"
                "bi bi-people-fill"
                "Repartija"
                "Repartí el gasto como items de un recibo y realizá una carga colaborativa"
                []
            ]
        ]


viewReceiptBanner : Maybe ReceiptReadingState -> Html Msg
viewReceiptBanner receiptParseState =
    div [ class "card mb-4" ]
        [ div [ class "card-body" ]
            [ Html.h6 [ class "mb-1" ] [ i [ class "bi bi-stars me-1" ] [], text "Leer recibo con IA" ]
            , p [ class "text-body-secondary small mb-2" ]
                [ text "Subí una foto del recibo y cargá los items automáticamente" ]
            , Bs.fileInput
                [ accept "image/*"
                , on "change" (Decode.map ReceiptImageSelected fileDecoder)
                ]
            , case receiptParseState of
                Just ReadingFile ->
                    div [ class "d-flex gap-2 align-items-center mt-3" ]
                        [ Bs.spinner [ Attr.attribute "aria-hidden" "true" ]
                        , Bs.alert Bs.AlertInfo
                            [ style "margin-bottom" "0", style "flex" "1" ]
                            [ text "Leyendo la imagen..." ]
                        ]

                Just ProcessingWithAI ->
                    div [ class "mt-3" ]
                        [ div [ class "d-flex gap-2 align-items-center mb-3" ]
                            [ Bs.spinner [ Attr.attribute "aria-hidden" "true" ]
                            , Bs.alert Bs.AlertInfo
                                [ style "margin-bottom" "0", style "flex" "1" ]
                                [ text "Analizando el recibo con inteligencia artificial..." ]
                            ]
                        , Bs.alert Bs.AlertWarning
                            [ style "margin-bottom" "0" ]
                            [ text "Esto podria tomar varios minutos, no cierres esta ventana" ]
                        ]

                Just (ErrorProcessing errorMsg) ->
                    Bs.alert Bs.AlertDanger
                        [ class "mt-3"
                        , style "margin-bottom" "0"
                        , style "display" "flex"
                        , style "align-items" "center"
                        , style "gap" "0.5rem"
                        ]
                        [ span [ style "flex" "1" ] [ text ("Algo salio mal: " ++ errorMsg) ]
                        , button
                            [ type_ "button"
                            , class "btn-close"
                            , Attr.attribute "aria-label" "Cerrar"
                            , onClick ClearReceiptError
                            ]
                            []
                        ]

                Nothing ->
                    text ""
            ]
        ]


viewPartesForm : GrupoLike g -> String -> Form CustomFormError Pago -> Html Msg
viewPartesForm grupo prefix form =
    let
        incluidos =
            grupo.participantes
                |> List.filter
                    (\participante ->
                        (Form.getFieldAsBool (prefix ++ ".partes." ++ participante.id ++ ".incluido") form).value == Just True
                    )

        mode =
            { mostrarPartes =
                (Form.getFieldAsBool (prefix ++ "." ++ mostrarPartesField) form).value == Just True
            , mostrarMontoFijo =
                (Form.getFieldAsBool (prefix ++ "." ++ mostrarMontoFijoField) form).value == Just True
            }
    in
    div [ class "mb-4" ]
        [ div [ class "d-flex flex-wrap align-items-center gap-2 mb-2" ]
            [ Html.h5 [ class "mb-0" ] [ text "Quienes participan" ]
            , viewSeleccionarParticipantes "Quienes participan" "deudores-seleccionar" grupo.participantes prefix form
            ]
        , div [ class "d-flex flex-wrap gap-2 mb-3" ]
            [ viewModoChip (prefix ++ "." ++ mostrarPartesField) mode.mostrarPartes "Partes"
            , viewModoChip (prefix ++ "." ++ mostrarMontoFijoField) mode.mostrarMontoFijo "Monto fijo"
            ]
        , viewPartesTable prefix mode incluidos (Form.getOutput form |> Maybe.map (\pago -> sumaMontosFijos pago.deudores)) form
        ]


{-| Pill con checkbox interno para activar/desactivar un modo de reparto
("Partes" o "Monto fijo"). El cuadradito refleja el estado igual que un
checkbox; al togglearlo se muestra u oculta la columna correspondiente.
-}
viewModoChip : String -> Bool -> String -> Html Msg
viewModoChip path active label =
    button
        [ type_ "button"
        , class "btn btn-light rounded-pill d-inline-flex align-items-center gap-2"
        , Attr.attribute "aria-pressed"
            (if active then
                "true"

             else
                "false"
            )
        , onClick <| PagoForm <| Form.Input path Form.Checkbox (FormField.Bool (not active))
        ]
        [ i
            [ class
                (if active then
                    "bi bi-check-square-fill"

                 else
                    "bi bi-square"
                )
            ]
            []
        , text label
        ]


{-| Selector de participantes como grupo de pills. En desktop se muestra
directamente; en mobile se esconde detrás de un botón que abre un modal con las
mismas pills. El estado abierto/cerrado del modal lo maneja Bootstrap por JS
(atributos `data-bs-*`), así que no toca el `Model`.
-}
viewSeleccionarParticipantes : String -> String -> List Participante -> String -> Form CustomFormError Pago -> Html Msg
viewSeleccionarParticipantes titulo selectorId participantes prefix form =
    let
        modalId =
            selectorId ++ "-modal"

        pills =
            div [ class "d-flex flex-wrap gap-2" ]
                (participantes |> List.map (\participante -> viewParticipantePill participante prefix form))
    in
    div []
        [ div [ class "d-none d-md-block" ] [ pills ]
        , div [ class "d-md-none" ]
            [ button
                [ type_ "button"
                , class "btn btn-light rounded-pill d-inline-flex align-items-center gap-2"
                , Attr.id selectorId
                , Attr.attribute "data-bs-toggle" "modal"
                , Attr.attribute "data-bs-target" ("#" ++ modalId)
                ]
                [ i [ class "bi bi-person-fill" ] [], text "Seleccionar" ]
            ]
        , div
            [ class "modal fade"
            , Attr.id modalId
            , Attr.attribute "tabindex" "-1"
            , Attr.attribute "aria-hidden" "true"
            ]
            [ div [ class "modal-dialog modal-dialog-scrollable modal-fullscreen-sm-down" ]
                [ div [ class "modal-content" ]
                    [ div [ class "modal-header" ]
                        [ Html.h5 [ class "modal-title" ] [ text titulo ]
                        , button
                            [ type_ "button"
                            , class "btn-close"
                            , Attr.attribute "data-bs-dismiss" "modal"
                            , Attr.attribute "aria-label" "Cerrar"
                            ]
                            []
                        ]
                    , div [ class "modal-body" ] [ pills ]
                    , div [ class "modal-footer" ]
                        [ button
                            [ type_ "button"
                            , class "btn btn-dark w-100"
                            , Attr.attribute "data-bs-dismiss" "modal"
                            ]
                            [ text "Listo" ]
                        ]
                    ]
                ]
            ]
        ]


viewParticipantePill : Participante -> String -> Form CustomFormError Pago -> Html Msg
viewParticipantePill participante prefix form =
    let
        incluidoField =
            Form.getFieldAsBool (prefix ++ ".partes." ++ participante.id ++ ".incluido") form

        incluido =
            incluidoField.value == Just True
    in
    button
        [ type_ "button"
        , class "btn btn-light rounded-pill d-inline-flex align-items-center gap-2"
        , Attr.attribute "aria-pressed"
            (if incluido then
                "true"

             else
                "false"
            )
        , onClick <| PagoForm <| Form.Input incluidoField.path Form.Checkbox (FormField.Bool (not incluido))
        ]
        [ i
            [ class
                (if incluido then
                    "bi bi-check-square-fill"

                 else
                    "bi bi-square"
                )
            ]
            []
        , text participante.nombre
        ]


{-| Indica si la columna "División" debe mostrarse. Sólo se oculta cuando el
único modo activo es el de monto fijo (no hay nada que repartir por partes).
-}
divisionVisible : ModoPartes -> Bool
divisionVisible mode =
    mode.mostrarPartes || not mode.mostrarMontoFijo


{-| Suma de los montos fijos de una distribución, usada en la fila "Suma". Toma
las `Parte` ya parseadas del `Pago` del form, así que no reparsea strings.
-}
sumaMontosFijos : Distribucion -> Monto
sumaMontosFijos distribucion =
    case distribucion.tipo of
        Api.TipoDistribucionPartes dp ->
            dp.partes
                |> List.filterMap
                    (\parte ->
                        case parte of
                            Api.MontoFijo monto _ ->
                                Just monto

                            Api.PonderadoYMontoFijo monto _ _ ->
                                Just monto

                            Api.Ponderado _ _ ->
                                Nothing
                    )
                |> List.foldl Monto.add Monto.zero

        Api.TipoDistribucionRepartija _ ->
            Monto.zero


{-| Total del listado de una repartija: la suma de los montos de cada ítem (la
cantidad no multiplica, sólo se usa al reclamar). No incluye la propina. Toma
los ítems ya parseados del `Pago` del form.
-}
totalItemsRepartija : Distribucion -> Monto
totalItemsRepartija distribucion =
    case distribucion.tipo of
        Api.TipoDistribucionRepartija repartija ->
            repartija.items
                |> List.map .monto
                |> List.foldl Monto.add Monto.zero

        Api.TipoDistribucionPartes _ ->
            Monto.zero


{-| Tabla de partes con columnas dinámicas: "Monto fijo" cuando ese modo está
activo y "División" (contador de partes o "En partes iguales") salvo cuando el
único modo es el de monto fijo. `suma` es el total de montos fijos parseado del
form, mostrado en el pie cuando se editan montos.
-}
viewPartesTable : String -> ModoPartes -> List Participante -> Maybe Monto -> Form CustomFormError Pago -> Html Msg
viewPartesTable prefix mode incluidos suma form =
    let
        headerCells =
            Html.th [ Attr.scope "col" ] [ text "Participante" ]
                :: (if mode.mostrarMontoFijo then
                        [ Html.th [ Attr.scope "col", class "text-end" ] [ text "Monto fijo" ] ]

                    else
                        []
                   )
                ++ (if divisionVisible mode then
                        [ Html.th [ Attr.scope "col", class "text-end" ] [ text "División" ] ]

                    else
                        []
                   )
    in
    Html.table [ class "table align-middle" ]
        [ Html.thead [] [ Html.tr [] headerCells ]
        , Html.tbody []
            (incluidos |> List.map (\participante -> viewParteRow prefix mode participante form))
        , if mode.mostrarMontoFijo then
            let
                sumaRow =
                    Html.tr [ class "text-body-secondary" ]
                        ([ Html.td [] [ text "Total" ]
                         , Html.td [ class "text-end" ]
                            [ text ("$ " ++ (suma |> Maybe.map Monto.toString |> Maybe.withDefault "—")) ]
                         ]
                            ++ (if divisionVisible mode then
                                    [ Html.td [] [] ]

                                else
                                    []
                               )
                        )
            in
            Html.tfoot [] [ sumaRow ]

          else
            text ""
        ]


viewParteRow : String -> ModoPartes -> Participante -> Form CustomFormError Pago -> Html Msg
viewParteRow prefix mode participante form =
    let
        basePath =
            prefix ++ ".partes." ++ participante.id
    in
    Html.tr []
        (Html.td [ class "fw-bold" ] [ text participante.nombre ]
            :: (if mode.mostrarMontoFijo then
                    let
                        montoField =
                            Form.getFieldAsString (basePath ++ ".monto") form
                    in
                    [ Html.td [ class "text-end" ]
                        [ div [ class "ms-auto", style "width" "11rem" ]
                            [ Html.map PagoForm <|
                                Bs.montoInput montoField [ placeholder "13.000,00" ]
                            ]
                        ]
                    ]

                else
                    []
               )
            ++ (if divisionVisible mode then
                    [ Html.td [ class "text-end" ]
                        [ if mode.mostrarPartes then
                            viewCuotaCounter (basePath ++ ".cuota") form

                          else
                            span [ class "text-body-secondary" ] [ text "En partes iguales" ]
                        ]
                    ]

                else
                    []
               )
        )


viewCuotaCounter : String -> Form CustomFormError Pago -> Html Msg
viewCuotaCounter path form =
    let
        cuotaField =
            Form.getFieldAsString path form

        cuota =
            cuotaField.value
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 0

        setCuota nuevaCuota =
            PagoForm <| Form.Input cuotaField.path Form.Text (FormField.String (String.fromInt (max 0 nuevaCuota)))
    in
    div [ class "d-flex align-items-center justify-content-end gap-2" ]
        [ button
            [ type_ "button"
            , class "btn btn-dark btn-sm rounded-circle"
            , Attr.attribute "aria-label" "Menos partes"
            , onClick (setCuota (cuota - 1))
            ]
            [ i [ class "bi bi-dash" ] [] ]
        , span [ class "px-1" ] [ text (String.fromInt cuota) ]
        , button
            [ type_ "button"
            , class "btn btn-dark btn-sm rounded-circle"
            , Attr.attribute "aria-label" "Más partes"
            , onClick (setCuota (cuota + 1))
            ]
            [ i [ class "bi bi-plus" ] [] ]
        ]


viewBalanceCard : GrupoLike g -> Model -> Html Msg
viewBalanceCard grupo model =
    case model.resumenPago of
        Success resumen ->
            case getDeudasFromResumen resumen.resumen of
                Just netos ->
                    div [ class "mb-3" ]
                        [ Html.h5 [ class "mb-2" ] [ text "Balance de este gasto" ]
                        , div [ class "card" ]
                            [ div [ class "card-body" ] [ viewNetosBarras grupo netos ] ]
                        ]

                Nothing ->
                    text ""

        Loading ->
            div [ class "mb-3 d-flex justify-content-center" ] [ Bs.spinner [] ]

        _ ->
            text ""


viewRepartijaLink : ULID -> Form CustomFormError Pago -> Html Msg
viewRepartijaLink grupoId form =
    let
        repartijaId =
            (Form.getFieldAsString "distribucion_deudores.repartija_id" form).value
                |> Maybe.withDefault emptyUlid
    in
    if repartijaId /= emptyUlid && repartijaId /= "" then
        p []
            [ a
                [ class "link-primary"
                , Path.href <| Path.Grupos_GrupoId__Repartijas_RepartijaId_ { grupoId = grupoId, repartijaId = repartijaId }
                , target "_blank"
                ]
                [ text "Ver repartija colaborativa" ]
            ]

    else
        text ""


fileDecoder : Decode.Decoder File
fileDecoder =
    Decode.at [ "target", "files" ] (Decode.list File.decoder)
        |> Decode.andThen
            (\files ->
                case files of
                    [] ->
                        Decode.fail "No files selected"

                    [ file ] ->
                        Decode.succeed file

                    _ ->
                        Decode.fail "Too many files"
            )


viewRepartijaForm : String -> Form CustomFormError Pago -> Html Msg
viewRepartijaForm prefix form =
    let
        montoField =
            Form.getFieldAsString (prefix ++ ".extra") form

        itemsIndexes =
            Form.getListIndexes (prefix ++ ".items") form
    in
    div []
        [ div [ class "mb-4" ]
            [ div [ class "d-flex justify-content-between align-items-center mb-2" ]
                [ Html.h6 [ class "mb-0" ] [ text "Ítems" ]
                , Bs.btn Bs.Secondary
                    [ onClick <| PagoForm <| Form.Append <| prefix ++ ".items"
                    , type_ "button"
                    , class "btn-sm rounded-pill"
                    ]
                    [ i [ class "bi bi-plus me-1" ] [], text "Item" ]
                ]
            , div [ class "table-responsive" ]
                [ Html.table [ class "table align-middle", style "min-width" "40rem" ]
                    [ Html.thead []
                        [ Html.tr []
                            [ Html.th [ Attr.scope "col" ] [ text "Nombre" ]
                            , Html.th [ Attr.scope "col", style "width" "5rem" ] [ text "Cantidad" ]
                            , Html.th [ Attr.scope "col" ] [ text "Monto" ]
                            , Html.th [ Attr.scope "col", style "width" "1%" ] []
                            ]
                        ]
                    , Html.tbody []
                        (List.map (\idx -> viewRepartijaItemForm idx prefix form) itemsIndexes)
                    , Html.tfoot []
                        [ Html.tr [ class "text-body-secondary" ]
                            [ Html.td [] [ text "Total" ]
                            , Html.td [] []
                            , Html.td []
                                [ text
                                    ("$ "
                                        ++ (Form.getOutput form
                                                |> Maybe.map (\pago -> Monto.toString (totalItemsRepartija pago.deudores))
                                                |> Maybe.withDefault "—"
                                           )
                                    )
                                ]
                            , Html.td [ style "width" "1%" ] []
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "mb-4" ]
            [ Html.h6 [ class "mb-2" ] [ text "Propina" ]
            , Html.map PagoForm <|
                Bs.montoInput montoField [ placeholder "1000" ]
            ]
        , Html.hr [ class "my-3" ] []
        , div [ class "mb-4" ]
            [ Html.h6 [ class "mb-2" ] [ text "Distribución de sobras" ]
            , p [ class "text-body-secondary mb-2", style "font-size" "0.875rem" ]
                [ text "Qué hacer con los items que nadie reclamó. Podés cambiar esta opción en cualquier momento." ]
            , let
                distribucionDeSobrasField =
                    Form.getFieldAsString (prefix ++ ".distribucionDeSobras") form

                selectSobras v =
                    PagoForm <| Form.Input distribucionDeSobrasField.path Form.Select (FormField.String v)
              in
              Bs.segmentedButton []
                [ Bs.segmentedButtonItem
                    { active = distribucionDeSobrasField.value == Just "SobrasNoDistribuir"
                    , onSelect = selectSobras "SobrasNoDistribuir"
                    }
                    []
                    [ text "No distribuir" ]
                , Bs.segmentedButtonItem
                    { active = distribucionDeSobrasField.value == Just "SobrasProporcional"
                    , onSelect = selectSobras "SobrasProporcional"
                    }
                    []
                    [ text "Proporcional" ]
                ]
            ]
        ]


viewRepartijaItemForm : Int -> String -> Form CustomFormError Pago -> Html Msg
viewRepartijaItemForm i prefix form =
    let
        nombreField =
            Form.getFieldAsString (prefix ++ ".items." ++ String.fromInt i ++ ".nombre") form

        montoField =
            Form.getFieldAsString (prefix ++ ".items." ++ String.fromInt i ++ ".monto") form

        cantidadField =
            Form.getFieldAsString (prefix ++ ".items." ++ String.fromInt i ++ ".cantidad") form
    in
    Html.tr []
        [ Html.td []
            [ Html.map PagoForm <|
                Bs.textInput nombreField [ placeholder "Birrita" ]
            ]
        , Html.td [ style "width" "5rem" ]
            [ Html.map PagoForm <|
                Bs.textInput cantidadField [ placeholder "4" ]
            ]
        , Html.td []
            [ Html.map PagoForm <|
                Bs.montoInput montoField [ placeholder "20.000" ]
            ]
        , Html.td [ style "width" "1%" ]
            [ Bs.btn Bs.Danger
                [ type_ "button"
                , onClick <| PagoForm <| Form.RemoveItem (prefix ++ ".items") i
                , Attr.attribute "aria-label" "Eliminar item"
                ]
                [ Html.i [ class "bi bi-trash" ] [] ]
            ]
        ]


allowedMimeTypesForReceiptUpload : List String
allowedMimeTypesForReceiptUpload =
    [ "image/png"
    , "image/jpeg"
    , "image/webp"
    , "image/gif"
    ]
