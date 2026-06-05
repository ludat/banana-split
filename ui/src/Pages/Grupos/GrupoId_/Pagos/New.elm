module Pages.Grupos.GrupoId_.Pagos.New exposing (Model, Msg, ReceiptReadingState, Section(..), andThenSendWarningOnExit, page, subscriptions, update, validatePago, validatePagoInSection, view, waitAndCheckNecessaryData)

import Base64.Encode
import Browser.Dom
import Bytes exposing (Bytes)
import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.Bootstrap as Bs
import Date exposing (Date)
import Effect exposing (Effect)
import File exposing (File)
import Form exposing (Form)
import Form.Error as FormError
import Form.Field as FormField
import Form.Init as Form
import Form.Validate as V exposing (Validation, nonEmpty)
import Generated.Api as Api exposing (Distribucion, DistribucionDeSobras(..), DistribucionMontoEquitativo, DistribucionMontosEspecificos, Moneda, Pago, Participante, ParticipanteId, Repartija, RepartijaItem, ResumenNetos, ResumenPago, TipoDistribucion(..), ULID)
import Html exposing (Html, a, button, details, div, i, li, p, span, summary, text, ul)
import Html.Attributes as Attr exposing (accept, class, classList, disabled, id, placeholder, style, target, type_)
import Html.Events exposing (on, onClick, onSubmit)
import Http
import Json.Decode as Decode
import Layouts
import Models.Grupo exposing (GrupoLike, lookupNombreParticipante)
import Models.LugarAccionable exposing (LugarParaAccionar(..))
import Models.Moneda as Moneda
import Models.Monto as Monto
import Models.ResumenNetos exposing (errorAccionableEn, errorMensaje, getDeudasFromResumen, getTotalFromResumen)
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
        , update = update shared.store
        , subscriptions = subscriptions
        , view = view shared.store
        }
        |> Page.withLayout (\_ -> Layouts.Default_Grupo {})



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
    | PagoConfirmation


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
        defaultDistribucionValues =
            [ Form.setString "fecha" (Date.toIsoString today)
            , Form.setGroup "distribucion_pagadores"
                [ Form.setString "tipo" "monto_equitativo"
                , Form.setString "distribucionDeSobras" <| distribucionDeSobrasToString SobrasNoDistribuir
                ]
            , Form.setGroup "distribucion_deudores"
                [ Form.setString "tipo" "monto_equitativo"
                , Form.setString "distribucionDeSobras" <| distribucionDeSobrasToString SobrasNoDistribuir
                ]
            ]
    in
    ( { grupoId = grupoId
      , currentPagoId = Nothing
      , currentSection = BasicPagoData
      , pagoBasicoForm = Form.initial defaultDistribucionValues (validatePagoInSection BasicPagoData [])
      , deudoresForm = Form.initial defaultDistribucionValues (validatePagoInSection DeudoresSection [])
      , resumenDeudores = NotAsked
      , pagadoresForm = Form.initial defaultDistribucionValues (validatePagoInSection PagadoresSection [])
      , resumenPagadores = NotAsked
      , pagoForm = Form.initial defaultDistribucionValues (validatePago [])
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
    V.succeed Pago
        |> V.andMap (V.field "id" validateId)
        |> V.andMap
            (if section == PagoConfirmation || section == BasicPagoData then
                V.field "monto" Monto.validateMonto

             else
                V.maybe (V.field "monto" Monto.validateMonto) |> V.map (Maybe.withDefault Monto.zero)
             --V.succeed Monto.zero
            )
        |> V.andMap (V.field "moneda" Moneda.validate)
        |> V.andMap (V.succeed False)
        |> V.andMap
            (if section == PagoConfirmation || section == BasicPagoData then
                V.field "nombre" (V.string |> V.andThen nonEmpty)

             else
                V.succeed ""
            )
        |> V.andMap (V.field "fecha" validateDay)
        |> V.andMap
            (if section == PagoConfirmation || section == PagadoresSection then
                V.field "distribucion_pagadores" <| validateDistribucion participantes

             else
                V.succeed { id = emptyUlid, tipo = Api.TipoDistribucionMontosEspecificos { id = emptyUlid, montos = [] } }
            )
        |> V.andMap
            (if section == PagoConfirmation || section == DeudoresSection then
                V.field "distribucion_deudores" <| validateDistribucion participantes

             else
                V.succeed { id = emptyUlid, tipo = Api.TipoDistribucionMontosEspecificos { id = emptyUlid, montos = [] } }
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

                            "montos_especificos" ->
                                V.succeed Api.DistribucionMontosEspecificos
                                    |> V.andMap (V.field "montos_especificos_id" validateId)
                                    |> V.andMap
                                        (V.field "montos"
                                            (V.list
                                                (V.succeed Api.MontoEspecifico
                                                    |> V.andMap (V.field "id" validateId)
                                                    |> V.andMap (V.field "participante" V.string |> V.andThen V.nonEmpty)
                                                    |> V.andMap (V.field "monto" Monto.validateMonto)
                                                )
                                            )
                                        )
                                    |> V.map Api.TipoDistribucionMontosEspecificos

                            "monto_equitativo" ->
                                V.succeed Api.DistribucionMontoEquitativo
                                    |> V.andMap (V.field "monto_equitativo_id" validateId)
                                    |> V.andMap
                                        (V.field "participantes"
                                            (V.sequence
                                                (participantes
                                                    |> List.map
                                                        (\p -> V.field p.id V.bool |> V.map (\b -> ( p, b )))
                                                )
                                                |> V.map
                                                    (List.filterMap
                                                        (\( p, b ) ->
                                                            if b then
                                                                Just p.id

                                                            else
                                                                Nothing
                                                        )
                                                    )
                                            )
                                        )
                                    |> V.map Api.TipoDistribucionMontoEquitativo

                            _ ->
                                V.fail <| FormError.value FormError.Empty
                    )
            )


update : Store -> Msg -> Model -> ( Model, Effect Msg )
update store msg model =
    let
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
            ( { model
                | pagoForm = Form.update (validatePago participantes) formMsg model.pagoForm
                , pagoBasicoForm = Form.update (validatePagoInSection BasicPagoData participantes) formMsg model.pagoBasicoForm
                , pagadoresForm = Form.update (validatePagoInSection PagadoresSection participantes) formMsg model.pagadoresForm
                , deudoresForm = Form.update (validatePagoInSection DeudoresSection participantes) formMsg model.deudoresForm
                , hasUnsavedChanges =
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
                        newModel =
                            case model.currentSection of
                                PagadoresSection ->
                                    { model
                                        | pagoForm = addItemsToForm "distribucion_pagadores" items model.pagoForm (validatePago participantes)
                                        , pagoBasicoForm = addItemsToForm "distribucion_pagadores" items model.pagoBasicoForm (validatePagoInSection BasicPagoData participantes)
                                        , pagadoresForm = addItemsToForm "distribucion_pagadores" items model.pagadoresForm (validatePagoInSection PagadoresSection participantes)
                                        , deudoresForm = addItemsToForm "distribucion_pagadores" items model.deudoresForm (validatePagoInSection DeudoresSection participantes)
                                        , hasUnsavedChanges = True
                                    }

                                DeudoresSection ->
                                    { model
                                        | pagoForm = addItemsToForm "distribucion_deudores" items model.pagoForm (validatePago participantes)
                                        , pagoBasicoForm = addItemsToForm "distribucion_deudores" items model.pagoBasicoForm (validatePagoInSection BasicPagoData participantes)
                                        , pagadoresForm = addItemsToForm "distribucion_deudores" items model.pagadoresForm (validatePagoInSection PagadoresSection participantes)
                                        , deudoresForm = addItemsToForm "distribucion_deudores" items model.deudoresForm (validatePagoInSection DeudoresSection participantes)
                                        , hasUnsavedChanges = True
                                    }

                                _ ->
                                    model
                    in
                    ( { newModel
                        | receiptParseState = Nothing
                      }
                    , Effect.batch
                        [ Toasts.pushToast Toasts.ToastSuccess "Recibo parseado correctamente"
                        ]
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
            let
                updateModel =
                    case model.currentSection of
                        BasicPagoData ->
                            \m ->
                                { m
                                    | pagoBasicoForm = Form.update (validatePagoInSection model.currentSection participantes) Form.Submit model.pagoBasicoForm
                                    , currentSection = PagadoresSection
                                }

                        PagadoresSection ->
                            \m ->
                                { m
                                    | pagadoresForm =
                                        Form.update (validatePagoInSection model.currentSection participantes) Form.Submit model.pagadoresForm
                                    , currentSection = DeudoresSection
                                }

                        DeudoresSection ->
                            \m ->
                                { m
                                    | deudoresForm =
                                        Form.update (validatePagoInSection model.currentSection participantes) Form.Submit model.deudoresForm
                                    , currentSection = PagoConfirmation
                                }

                        PagoConfirmation ->
                            \m ->
                                { m
                                    | pagoForm =
                                        Form.update (validatePagoInSection model.currentSection participantes) Form.Submit model.pagoForm
                                    , currentSection = PagoConfirmation
                                }
            in
            ( updateModel model
            , Effect.none
            )
                |> andThenFocusFieldIfSectionChanged model.currentSection
                |> andThenUpdateResumenesFromForms model
                |> andThenSendWarningOnExit

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
                distribucionToForm (distribucionesPagadoresDefault participantes) (Maybe.map .pagadores pago)
            , Form.setGroup "distribucion_deudores" <|
                distribucionToForm (distribucionesDeudoresDefault participantes) (Maybe.map .deudores pago)
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


addItemsToForm : String -> List Api.RepartijaItem -> Form CustomFormError Pago -> Validation CustomFormError Pago -> Form CustomFormError Pago
addItemsToForm prefix items form validation =
    let
        -- Get the current number of items in the form
        currentIndexes =
            Form.getListIndexes (prefix ++ ".items") form

        startingIndex =
            List.length currentIndexes

        -- Build a list of Form.Msg to add and populate the new items
        formUpdates =
            items
                |> List.indexedMap
                    (\idx item ->
                        let
                            newIndex =
                                startingIndex + idx

                            itemPrefix =
                                prefix ++ ".items." ++ String.fromInt newIndex
                        in
                        [ Form.Append (prefix ++ ".items")
                        , Form.Input (itemPrefix ++ ".id") Form.Text (FormField.String emptyUlid)
                        , Form.Input (itemPrefix ++ ".nombre") Form.Text (FormField.String item.nombre)
                        , Form.Input (itemPrefix ++ ".monto") Form.Text (FormField.String (Monto.toRawString item.monto))
                        , Form.Input (itemPrefix ++ ".cantidad") Form.Text (FormField.String (String.fromInt item.cantidad))
                        ]
                    )
                |> List.concat
    in
    -- Apply all form updates using a fold
    List.foldl
        (\formMsg accForm -> Form.update validation formMsg accForm)
        form
        formUpdates


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


equitativosToForm : DistribucionMontoEquitativo -> List ( String, FormField.Field )
equitativosToForm equitativo =
    [ Form.setString "monto_equitativo_id" equitativo.id
    , Form.setString "tipo" "monto_equitativo"
    , Form.setGroup "participantes"
        (equitativo.participantes
            |> List.map (\p -> Form.setBool p True)
        )
    ]


especificosToForm : DistribucionMontosEspecificos -> List ( String, FormField.Field )
especificosToForm especificos =
    [ Form.setString "montos_especificos_id" especificos.id
    , Form.setString "tipo" "montos_especificos"
    , Form.setList "montos"
        (especificos.montos
            |> List.map
                (\m ->
                    FormField.group
                        [ Form.setString "id" m.id
                        , Form.setString "participante" <| m.participante
                        , Form.setString "monto" <| Monto.toRawString m.monto
                        ]
                )
        )
    ]


distribucionesPagadoresDefault : List Participante -> DistribucionesFormDefaults
distribucionesPagadoresDefault participantes =
    { especificos =
        { id = emptyUlid
        , montos =
            participantes
                |> List.map
                    (\p ->
                        { id = emptyUlid
                        , participante = p.id
                        , monto = Monto.zero
                        }
                    )
        }
    , equitativo =
        { id = emptyUlid
        , participantes = []
        }
    , repartija =
        { id = emptyUlid
        , nombre = "GENERATED"
        , items = []
        , claims = []
        , extra = Monto.zero
        , distribucionDeSobras = SobrasNoDistribuir
        }
    }


distribucionesDeudoresDefault : List Participante -> DistribucionesFormDefaults
distribucionesDeudoresDefault participantes =
    { especificos =
        { id = emptyUlid
        , montos =
            participantes
                |> List.map
                    (\p ->
                        { id = emptyUlid
                        , participante = p.id
                        , monto = Monto.zero
                        }
                    )
        }
    , equitativo =
        { id = emptyUlid
        , participantes =
            participantes
                |> List.map .id
        }
    , repartija =
        { id = emptyUlid
        , nombre = "GENERATED"
        , items = []
        , claims = []
        , extra = Monto.zero
        , distribucionDeSobras = SobrasNoDistribuir
        }
    }


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


type alias DistribucionesFormDefaults =
    { repartija : Repartija
    , especificos : DistribucionMontosEspecificos
    , equitativo : DistribucionMontoEquitativo
    }


distribucionToForm :
    DistribucionesFormDefaults
    -> Maybe Distribucion
    -> List ( String, FormField.Field )
distribucionToForm defaults distribucion =
    Form.setString "id" (distribucion |> Maybe.map .id |> Maybe.withDefault emptyUlid)
        :: (case distribucion |> Maybe.map .tipo of
                Just (TipoDistribucionRepartija repartija) ->
                    equitativosToForm defaults.equitativo
                        ++ especificosToForm defaults.especificos
                        ++ repartijaToForm repartija

                Just (TipoDistribucionMontosEspecificos especificos) ->
                    equitativosToForm defaults.equitativo
                        ++ repartijaToForm defaults.repartija
                        ++ especificosToForm especificos

                Just (TipoDistribucionMontoEquitativo equitativo) ->
                    especificosToForm defaults.especificos
                        ++ repartijaToForm defaults.repartija
                        ++ equitativosToForm equitativo

                Nothing ->
                    especificosToForm defaults.especificos
                        ++ repartijaToForm defaults.repartija
                        ++ equitativosToForm defaults.equitativo
           )


andThenFocusFieldIfSectionChanged : Section -> ( Model, Effect Msg ) -> ( Model, Effect Msg )
andThenFocusFieldIfSectionChanged oldSection ( model, oldEffects ) =
    let
        focusEffect =
            if oldSection == model.currentSection then
                Effect.none

            else
                case model.currentSection of
                    BasicPagoData ->
                        Effect.sendCmd <| Task.attempt (\_ -> NoOp) (Browser.Dom.focus "pago-nombre")

                    PagadoresSection ->
                        Effect.sendCmd <| Task.attempt (\_ -> NoOp) (Browser.Dom.focus "distribucion_pagadores-tipo")

                    DeudoresSection ->
                        Effect.sendCmd <| Task.attempt (\_ -> NoOp) (Browser.Dom.focus "distribucion_deudores-tipo")

                    PagoConfirmation ->
                        Effect.sendCmd <| Task.attempt (\_ -> NoOp) (Browser.Dom.focus "pago-submit-button")
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


getParticipantesFromResumen : ResumenNetos -> Maybe (List ParticipanteId)
getParticipantesFromResumen resumen =
    resumen.netos
        |> List.map (\( p, _ ) -> p)
        |> Just


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


viewResumenPanel :
    GrupoLike g
    -> WebData ResumenPago
    -> (ResumenPago -> ResumenNetos)
    -> Bool
    -> List (Html Msg)
    -> Html Msg
viewResumenPanel grupo resumenData accessor negateMontos extraContent =
    div [ style "flex" "1", style "min-width" "300px" ]
        [ Bs.card []
            [ Bs.cardHeader [] [ text "Resumen" ]
            , Bs.cardBody []
                (case resumenData of
                    Success resumen ->
                        let
                            resumenNetos =
                                accessor resumen

                            netos =
                                getDeudasFromResumen resumenNetos
                                    |> (if negateMontos then
                                            Maybe.map (List.map (\( pp, m ) -> ( pp, Monto.negate m )))

                                        else
                                            identity
                                       )
                        in
                        [ p [] [ text <| "total: " ++ (Monto.toString <| getTotalFromResumen resumenNetos) ]
                        , p []
                            [ text <|
                                "participantes: "
                                    ++ (resumenNetos
                                            |> getParticipantesFromResumen
                                            |> Maybe.map (\ps -> ps |> List.map (lookupNombreParticipante grupo) |> String.join ", ")
                                            |> Maybe.withDefault "???"
                                       )
                            ]
                        , p [] [ Maybe.withDefault (text "") <| Maybe.map (viewNetosBarras grupo) netos ]
                        ]
                            ++ extraContent

                    NotAsked ->
                        []

                    Loading ->
                        [ Bs.spinner [] ]

                    Failure _ ->
                        [ Bs.alert Bs.AlertDanger [] [ text "Error al cargar el resumen" ] ]
                )
            ]
        ]


viewUnsavedChangesBanner : Html Msg
viewUnsavedChangesBanner =
    Bs.alert Bs.AlertWarning
        [ style "margin-bottom" "1rem" ]
        [ text "Hay cambios sin guardar" ]


viewWizardNav : Section -> Html Msg
viewWizardNav current =
    let
        tab section title =
            li [ class "nav-item" ]
                [ button
                    [ type_ "button"
                    , classList [ ( "nav-link", True ), ( "active", current == section ) ]
                    , onClick (SelectSection section)
                    ]
                    [ text title ]
                ]
    in
    ul [ class "nav nav-tabs mb-3" ]
        [ tab BasicPagoData "Datos básicos"
        , tab PagadoresSection "Pagadores"
        , tab DeudoresSection "Deudores"
        , tab PagoConfirmation "Detalles"
        ]


view : Store -> Model -> View Msg
view store model =
    case store |> Store.getGrupo model.grupoId of
        Success grupo ->
            { title = grupo.nombre
            , body =
                [ if model.hasUnsavedChanges then
                    viewUnsavedChangesBanner

                  else
                    text ""
                , viewWizardNav model.currentSection
                , case model.currentSection of
                    BasicPagoData ->
                        div []
                            [ p [] [ text "Ingresá la información básica del pago: un nombre descriptivo y el monto total." ]
                            , viewPagoForm model.pagoBasicoForm
                            ]

                    PagadoresSection ->
                        div [ style "display" "flex", style "gap" "1.5rem", style "flex-wrap" "wrap" ]
                            [ div [ style "flex" "1", style "min-width" "300px" ]
                                [ Html.form [ onSubmit <| SubmitCurrentSection ]
                                    [ p [] [ text "Indicá quién pagó y cómo se distribuye el gasto entre los que pusieron plata." ]
                                    , viewDistribucionForm grupo.participantes "distribucion_pagadores" model.pagadoresForm model.receiptParseState
                                    , viewErrorFromResumenData model.resumenPagadores .resumenPagadores
                                    , Bs.btn Bs.Primary
                                        [ disabled (Form.getOutput model.pagadoresForm == Nothing || hasActionableErrors Lugar_CreacionPago model.resumenPagadores .resumenPagadores)
                                        , onClick SubmitCurrentSection
                                        ]
                                        [ text "Siguiente seccion" ]
                                    ]
                                ]
                            , viewResumenPanel grupo model.resumenPagadores .resumenPagadores False []
                            ]

                    DeudoresSection ->
                        div [ style "display" "flex", style "gap" "1.5rem", style "flex-wrap" "wrap" ]
                            [ div [ style "flex" "1", style "min-width" "300px" ]
                                [ Html.form [ onSubmit <| SubmitCurrentSection ]
                                    [ p [] [ text "Indicá quiénes deben y cómo se reparte la deuda entre ellos." ]
                                    , viewDistribucionForm grupo.participantes "distribucion_deudores" model.deudoresForm model.receiptParseState
                                    , viewErrorFromResumenData model.resumenDeudores .resumenDeudores
                                    , Bs.btn Bs.Primary
                                        [ disabled (Form.getOutput model.deudoresForm == Nothing || hasActionableErrors Lugar_CreacionPago model.resumenDeudores .resumenDeudores)
                                        , onClick SubmitCurrentSection
                                        ]
                                        [ text "Siguiente seccion" ]
                                    ]
                                ]
                            , viewResumenPanel grupo model.resumenDeudores .resumenDeudores True []
                            ]

                    PagoConfirmation ->
                        Html.form [ onSubmit <| PagoForm Form.Submit ]
                            [ viewResumenPanel grupo
                                model.resumenPago
                                .resumen
                                False
                                (case Form.getOutput model.pagoForm of
                                    Just pago ->
                                        [ case pago.pagadores.tipo of
                                            TipoDistribucionRepartija repartija ->
                                                if repartija.id /= emptyUlid then
                                                    p []
                                                        [ a
                                                            [ class "link-primary"
                                                            , Path.href <| Path.Grupos_GrupoId__Repartijas_RepartijaId_ { grupoId = model.grupoId, repartijaId = repartija.id }
                                                            , target "_blank"
                                                            ]
                                                            [ text "repartija pagadores" ]
                                                        ]

                                                else
                                                    text ""

                                            _ ->
                                                text ""
                                        , case pago.deudores.tipo of
                                            TipoDistribucionRepartija repartija ->
                                                if repartija.id /= emptyUlid then
                                                    p []
                                                        [ a
                                                            [ class "link-primary"
                                                            , Path.href <| Path.Grupos_GrupoId__Repartijas_RepartijaId_ { grupoId = model.grupoId, repartijaId = repartija.id }
                                                            ]
                                                            [ text "repartija deudores" ]
                                                        ]

                                                else
                                                    text ""

                                            _ ->
                                                text ""
                                        ]

                                    Nothing ->
                                        []
                                )
                            , viewErrorFromResumenData model.resumenPago .resumen
                            , if model.hasUnsavedChanges then
                                let
                                    textoCTA =
                                        case model.currentPagoId of
                                            Nothing ->
                                                text "Crear pago"

                                            Just _ ->
                                                text "Actualizar pago"
                                in
                                Bs.btn Bs.Primary
                                    [ disabled (Form.getOutput model.pagoForm == Nothing)
                                    , onClick (PagoForm Form.Submit)
                                    , id "pago-submit-button"
                                    ]
                                    [ textoCTA ]

                              else
                                text ""
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


viewPagoForm : Form CustomFormError Pago -> Html Msg
viewPagoForm form =
    let
        nombreField =
            Form.getFieldAsString "nombre" form

        montoField =
            Form.getFieldAsString "monto" form

        monedaField =
            Form.getFieldAsString "moneda" form

        fechaField =
            Form.getFieldAsString "fecha" form
    in
    Html.form [ onSubmit SubmitCurrentSection ]
        [ Html.map PagoForm <|
            Bs.textFormItem nombreField
                { label = "Nombre"
                , placeholder = Just "Pago de deudas"
                , required = True
                }
        , Html.map PagoForm <|
            Bs.montoFormItem montoField
                { label = "Monto"
                , placeholder = Just "2000"
                , required = True
                }
        , Html.map PagoForm <|
            Bs.selectFormItem monedaField
                { label = "Moneda"
                , required = True
                , options = Moneda.todas |> List.map (\m -> ( Moneda.toString m, Moneda.nombre m ))
                }
        , Html.map PagoForm <|
            Bs.dateFormItem fechaField
                { label = "Fecha"
                , required = True
                }
        , Bs.btn Bs.Primary
            [ disabled (Form.getOutput form == Nothing)
            , onClick SubmitCurrentSection
            ]
            [ text "Siguiente seccion" ]
        ]


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


viewDistribucionForm : List Participante -> String -> Form CustomFormError Pago -> Maybe ReceiptReadingState -> Html Msg
viewDistribucionForm participantes prefix form receiptParseState =
    let
        tipoField =
            Form.getFieldAsString (prefix ++ ".tipo") form

        selectTipo v =
            PagoForm <| Form.Input tipoField.path Form.Select (FormField.String v)
    in
    div [] <|
        div [ style "margin-bottom" "1rem" ]
            [ Bs.segmentedButton []
                [ Bs.segmentedButtonItem
                    { active = tipoField.value == Just "monto_equitativo"
                    , onSelect = selectTipo "monto_equitativo"
                    }
                    [ id (prefix ++ "-tipo") ]
                    [ text "Equitativo" ]
                , Bs.segmentedButtonItem
                    { active = tipoField.value == Just "montos_especificos"
                    , onSelect = selectTipo "montos_especificos"
                    }
                    []
                    [ text "Específico" ]
                , Bs.segmentedButtonItem
                    { active = tipoField.value == Just "repartija"
                    , onSelect = selectTipo "repartija"
                    }
                    []
                    [ text "Repartija" ]
                ]
            ]
            :: (case tipoField.value of
                    Just "repartija" ->
                        [ p [ style "margin-bottom" "1rem" ]
                            [ text "División por items del recibo. Podés subir una foto del ticket para que se complete automáticamente." ]
                        , viewRepartijaForm prefix form receiptParseState
                        ]

                    Just "montos_especificos" ->
                        let
                            montosIndexes =
                                Form.getListIndexes (prefix ++ ".montos") form
                        in
                        [ p [ style "margin-bottom" "1rem" ]
                            [ text "Cada participante pone/debe un monto fijo que vos especificás." ]
                        , div [ style "margin-bottom" "0.5rem" ] <|
                            (montosIndexes
                                |> List.map
                                    (\index ->
                                        let
                                            montoField =
                                                Form.getFieldAsString (prefix ++ ".montos." ++ String.fromInt index ++ ".monto") form

                                            participanteField =
                                                Form.getFieldAsString (prefix ++ ".montos." ++ String.fromInt index ++ ".participante") form
                                        in
                                        div [ style "display" "flex", style "gap" "0.5rem", style "align-items" "end", style "margin-bottom" "0.5rem" ]
                                            [ div [ style "flex" "1" ]
                                                [ Html.map PagoForm <|
                                                    Bs.montoInput montoField [ placeholder "200" ]
                                                ]
                                            , div [ style "flex" "1" ]
                                                [ Html.map PagoForm <|
                                                    Bs.selectInput
                                                        (( "", "" ) :: List.map (\p -> ( p.id, p.nombre )) participantes)
                                                        participanteField
                                                        []
                                                ]
                                            , Bs.btn Bs.Danger
                                                [ type_ "button"
                                                , onClick <| PagoForm <| Form.RemoveItem (prefix ++ ".montos") index
                                                , Attr.attribute "aria-label" "Eliminar"
                                                ]
                                                [ i [ class "bi bi-trash" ] [] ]
                                            ]
                                    )
                            )
                                ++ [ Bs.btn Bs.Secondary
                                        [ onClick <| PagoForm <| Form.Append (prefix ++ ".montos")
                                        , type_ "button"
                                        ]
                                        [ i [ class "bi bi-plus me-1" ] [], text "Agregar" ]
                                   ]
                        ]

                    Just "monto_equitativo" ->
                        [ p [ style "margin-bottom" "1rem" ]
                            [ text "El monto se divide en partes iguales entre los participantes seleccionados." ]
                        , div [ style "margin-bottom" "0.5rem" ]
                            (participantes
                                |> List.map
                                    (\p ->
                                        let
                                            participanteField =
                                                Form.getFieldAsBool (prefix ++ ".participantes." ++ p.id) form
                                        in
                                        Html.map PagoForm <|
                                            Bs.checkbox participanteField { label = p.nombre }
                                    )
                            )
                        ]

                    _ ->
                        []
               )


viewRepartijaForm : String -> Form CustomFormError Pago -> Maybe ReceiptReadingState -> Html Msg
viewRepartijaForm prefix form receiptParseState =
    let
        montoField =
            Form.getFieldAsString (prefix ++ ".extra") form

        itemsIndexes =
            Form.getListIndexes (prefix ++ ".items") form
    in
    div []
        [ div [ style "margin-bottom" "1rem" ]
            [ Bs.fileInput
                [ accept "image/*"
                , on "change" (Decode.map ReceiptImageSelected fileDecoder)
                ]
            ]
        , case receiptParseState of
            Just ReadingFile ->
                div [ style "display" "flex", style "gap" "0.5rem", style "align-items" "center", style "margin-bottom" "1rem" ]
                    [ Bs.spinner [ Attr.attribute "aria-hidden" "true" ]
                    , Bs.alert Bs.AlertInfo
                        [ style "margin-bottom" "0", style "flex" "1" ]
                        [ text "Leyendo la imagen..." ]
                    ]

            Just ProcessingWithAI ->
                div []
                    [ div [ style "display" "flex", style "gap" "0.5rem", style "align-items" "center", style "margin-bottom" "1rem" ]
                        [ Bs.spinner [ Attr.attribute "aria-hidden" "true" ]
                        , Bs.alert Bs.AlertInfo
                            [ style "margin-bottom" "0", style "flex" "1" ]
                            [ text "Analizando el recibo con inteligencia artificial..." ]
                        ]
                    , Bs.alert Bs.AlertWarning
                        [ style "margin-bottom" "1rem" ]
                        [ text "Esto podria tomar varios minutos, no cierres esta ventana" ]
                    ]

            Just (ErrorProcessing errorMsg) ->
                Bs.alert Bs.AlertDanger
                    [ style "margin-bottom" "1rem"
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
        , div [ class "mb-4" ]
            [ Html.h6 [ class "mb-2" ] [ text "Items" ]
            , Html.table [ class "table align-middle" ]
                [ Html.thead []
                    [ Html.tr []
                        [ Html.th [ Attr.scope "col" ] [ text "Item" ]
                        , Html.th [ Attr.scope "col" ] [ text "Monto total" ]
                        , Html.th [ Attr.scope "col" ] [ text "Cantidad" ]
                        , Html.th [ Attr.scope "col", style "width" "1%" ] []
                        ]
                    ]
                , Html.tbody []
                    (List.map (\idx -> viewRepartijaItemForm idx prefix form) itemsIndexes)
                ]
            , Bs.btn Bs.Secondary
                [ onClick <| PagoForm <| Form.Append <| prefix ++ ".items"
                , type_ "button"
                ]
                [ i [ class "bi bi-plus me-1" ] [], text "Agregar item" ]
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
        , Html.td []
            [ Html.map PagoForm <|
                Bs.montoInput montoField [ placeholder "20.000" ]
            ]
        , Html.td []
            [ Html.map PagoForm <|
                Bs.textInput cantidadField [ placeholder "4" ]
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
