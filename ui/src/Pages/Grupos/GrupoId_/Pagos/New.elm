module Pages.Grupos.GrupoId_.Pagos.New exposing (Model, Msg, ReceiptReadingState, Section(..), andThenSendWarningOnExit, page, subscriptions, update, validatePago, validatePagoInSection, view, waitAndCheckNecessaryData)

import Base64.Encode
import Browser.Dom
import Bytes exposing (Bytes)
import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.NavBar as NavBar
import Components.Ui5 as Ui5
import Effect exposing (Effect)
import File exposing (File)
import Form exposing (Form)
import Form.Error as FormError
import Form.Field as FormField
import Form.Init as Form
import Form.Validate as V exposing (Validation, nonEmpty)
import Generated.Api as Api exposing (Distribucion, DistribucionDeSobras(..), Monto, Netos, Pago, Participante, ParticipanteId, Repartija, RepartijaItem, ResumenNetos, ResumenPago, TipoDistribucion(..), TipoErrorResumen(..), ULID)
import Html exposing (Html, a, details, div, p, summary, text)
import Html.Attributes as Attr exposing (accept, disabled, id, placeholder, selected, style, target, type_)
import Html.Events exposing (on, onClick, onSubmit)
import Http
import Json.Decode as Decode
import Layouts
import Models.Grupo exposing (GrupoLike, lookupNombreParticipante)
import Models.LugarAccionable exposing (LugarParaAccionar(..))
import Models.Monto as Monto
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import Process
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Task
import Utils.Form exposing (CustomFormError)
import Utils.Http exposing (viewHttpError)
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts
import Utils.Ulid exposing (emptyUlid)
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.grupoId shared.store
        , update = update shared.store
        , subscriptions = subscriptions
        , view = view shared.store
        }
        |> Page.withLayout
            (\m ->
                Layouts.Default
                    { navBarContent = Just <| NavBar.navBar (NavBar.modelFromShared shared route.params.grupoId) shared.store route.path
                    , grupo = Store.getGrupo m.grupoId shared.store
                    }
            )



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


init : ULID -> Store -> ( Model, Effect Msg )
init grupoId store =
    let
        defaultDistribucionValues =
            [ Form.setGroup "distribucion_pagadores"
                [ Form.setString "tipo" "monto_equitativo"
                , Form.setString "distribucionDeSobras" "SobrasNoDistribuir"
                ]
            , Form.setGroup "distribucion_deudores"
                [ Form.setString "tipo" "monto_equitativo"
                , Form.setString "distribucionDeSobras" "SobrasNoDistribuir"
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
        |> V.andMap (V.succeed False)
        |> V.andMap
            (if section == PagoConfirmation || section == BasicPagoData then
                V.field "nombre" (V.string |> V.andThen nonEmpty)

             else
                V.succeed ""
            )
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
        |> V.andMap (V.succeed False)
        |> V.andMap (V.field "nombre" (V.string |> V.andThen nonEmpty))
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
                                                        (\p -> V.field p.participanteId V.bool |> V.map (\b -> ( p, b )))
                                                )
                                                |> V.map
                                                    (List.filterMap
                                                        (\( p, b ) ->
                                                            if b then
                                                                Just p.participanteId

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
            ( { model | hasUnsavedChanges = False }
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshPagos model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Se creó el pago"
                , Effect.pushRoutePath <| Path.Grupos_GrupoId__Pagos_PagoId_ { grupoId = model.grupoId, pagoId = pago.pagoId }
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
            ( setFormsFromPago participantes pago model
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
                isDataModifyingEvent =
                    case formMsg of
                        Form.Input _ _ _ ->
                            True

                        Form.Append _ ->
                            True

                        Form.RemoveItem _ _ ->
                            True

                        Form.Reset _ ->
                            True

                        Form.NoOp ->
                            False

                        Form.Focus _ ->
                            False

                        Form.Blur _ ->
                            False

                        Form.Submit ->
                            False

                        Form.Validate ->
                            False
            in
            ( { model
                | pagoForm = Form.update (validatePago participantes) formMsg model.pagoForm
                , pagoBasicoForm = Form.update (validatePagoInSection BasicPagoData participantes) formMsg model.pagoBasicoForm
                , pagadoresForm = Form.update (validatePagoInSection PagadoresSection participantes) formMsg model.pagadoresForm
                , deudoresForm = Form.update (validatePagoInSection DeudoresSection participantes) formMsg model.deudoresForm
                , hasUnsavedChanges =
                    if isDataModifyingEvent then
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
                                initialFormValues =
                                    [ Form.setString "id" emptyUlid
                                    , Form.setString "nombre" ""
                                    , Form.setString "monto" ""
                                    , Form.setGroup "distribucion_pagadores" <|
                                        (distribucionToForm
                                            { id = emptyUlid
                                            , tipo =
                                                TipoDistribucionMontosEspecificos
                                                    { id = emptyUlid
                                                    , montos =
                                                        grupo.participantes
                                                            |> List.map
                                                                (\p ->
                                                                    { id = emptyUlid
                                                                    , participante = p.participanteId
                                                                    , monto = Monto.zero
                                                                    }
                                                                )
                                                    }
                                            }
                                            ++ distribucionToForm
                                                { id = emptyUlid
                                                , tipo =
                                                    TipoDistribucionMontoEquitativo
                                                        { id = emptyUlid
                                                        , participantes = []
                                                        }
                                                }
                                        )
                                    , Form.setGroup "distribucion_deudores"
                                        (distribucionToForm
                                            { id = emptyUlid
                                            , tipo =
                                                TipoDistribucionMontosEspecificos
                                                    { id = emptyUlid
                                                    , montos =
                                                        grupo.participantes
                                                            |> List.map
                                                                (\p ->
                                                                    { id = emptyUlid
                                                                    , participante = p.participanteId
                                                                    , monto = Monto.zero
                                                                    }
                                                                )
                                                    }
                                            }
                                            ++ distribucionToForm
                                                { id = emptyUlid
                                                , tipo =
                                                    TipoDistribucionRepartija
                                                        { id = emptyUlid
                                                        , nombre = "GENERATED"
                                                        , items = []
                                                        , claims = []
                                                        , extra = Monto.zero
                                                        , distribucionDeSobras = SobrasNoDistribuir
                                                        }
                                                }
                                            ++ distribucionToForm
                                                { id = emptyUlid
                                                , tipo =
                                                    TipoDistribucionMontoEquitativo
                                                        { id = emptyUlid
                                                        , participantes = grupo.participantes |> List.map .participanteId
                                                        }
                                                }
                                        )
                                    ]
                            in
                            ( { model
                                | pagoForm = Form.initial initialFormValues (validatePago grupo.participantes)
                                , pagadoresForm = Form.initial initialFormValues (validatePagoInSection PagadoresSection grupo.participantes)
                                , deudoresForm = Form.initial initialFormValues (validatePagoInSection DeudoresSection grupo.participantes)
                                , pagoBasicoForm = Form.initial initialFormValues (validatePagoInSection BasicPagoData grupo.participantes)
                                , resumenPago = Loading
                                , storedClaims = Nothing
                                , hasUnsavedChanges = False
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
                            ( setFormsFromPago grupo.participantes pago model
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


setFormsFromPago : List Participante -> Pago -> Model -> Model
setFormsFromPago participantes pago model =
    let
        initialFormValues =
            [ Form.setString "id" pago.pagoId
            , Form.setString "nombre" pago.nombre
            , Form.setString "monto" (Monto.toString pago.monto)
            , Form.setGroup "distribucion_pagadores" (distribucionToForm pago.pagadores)
            , Form.setGroup "distribucion_deudores" (distribucionToForm pago.deudores)
            ]

        claimsToStore =
            { pagadores = extractClaimsFromDistribucion pago.pagadores
            , deudores = extractClaimsFromDistribucion pago.deudores
            }
    in
    { model
        | pagoForm = Form.initial initialFormValues (validatePago participantes)
        , pagadoresForm = Form.initial initialFormValues (validatePagoInSection PagadoresSection participantes)
        , deudoresForm = Form.initial initialFormValues (validatePagoInSection DeudoresSection participantes)
        , pagoBasicoForm = Form.initial initialFormValues (validatePagoInSection BasicPagoData participantes)
        , storedClaims = Just claimsToStore
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
                        , Form.Input (itemPrefix ++ ".monto") Form.Text (FormField.String (Monto.toString item.monto))
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


distribucionToForm : Distribucion -> List ( String, FormField.Field )
distribucionToForm distribucion =
    Form.setString "id" distribucion.id
        :: (case distribucion.tipo of
                TipoDistribucionMontoEquitativo distribucionMontoEquitativo ->
                    [ Form.setString "monto_equitativo_id" distribucionMontoEquitativo.id
                    , Form.setString "tipo" "monto_equitativo"
                    , Form.setGroup "participantes"
                        (distribucionMontoEquitativo.participantes
                            |> List.map (\p -> Form.setBool p True)
                        )
                    ]

                TipoDistribucionMontosEspecificos distribucionMontosEspecificos ->
                    [ Form.setString "montos_especificos_id" distribucionMontosEspecificos.id
                    , Form.setString "tipo" "montos_especificos"
                    , Form.setList "montos"
                        (distribucionMontosEspecificos.montos
                            |> List.map
                                (\m ->
                                    FormField.group
                                        [ Form.setString "id" m.id
                                        , Form.setString "participante" <| m.participante
                                        , Form.setString "monto" <| Monto.toString m.monto
                                        ]
                                )
                        )
                    ]

                TipoDistribucionRepartija repartija ->
                    [ Form.setString "repartija_id" repartija.id
                    , Form.setString "tipo" "repartija"
                    , Form.setString "extra" (Monto.toString repartija.extra)
                    , Form.setString "distribucionDeSobras" (distribucionDeSobrasToString repartija.distribucionDeSobras)
                    , Form.setList "items"
                        (repartija.items
                            |> List.map
                                (\item ->
                                    FormField.group
                                        [ Form.setString "id" <| item.id
                                        , Form.setString "monto" <| Monto.toString item.monto
                                        , Form.setString "cantidad" <| String.fromInt item.cantidad
                                        , Form.setString "nombre" item.nombre
                                        ]
                                )
                        )
                    ]
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


getTotalFromResumen : ResumenNetos -> Maybe Monto
getTotalFromResumen resumen =
    Just resumen.total


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
                            Ui5.messageStrip
                                [ Attr.attribute "design"
                                    (if esAccionable then
                                        "Negative"

                                     else
                                        "Information"
                                    )
                                , Attr.attribute "hide-close-button" ""
                                ]
                                [ case error.objeto of
                                    [] ->
                                        text mensaje

                                    _ ->
                                        div [ style "display" "flex", style "gap" "0.5rem", style "align-items" "baseline" ]
                                            [ Ui5.label [ Attr.attribute "show-colon" "" ] [ text (String.join " > " error.objeto) ]
                                            , text mensaje
                                            ]
                                ]
                        )
                )


errorMensaje : TipoErrorResumen -> String
errorMensaje tipo =
    case tipo of
        ErrorMontosEspecificosVacios ->
            "No hay montos especificados"

        ErrorMontosEspecificosTotalNoCoincide actual esperado ->
            "El total (" ++ Monto.toString actual ++ ") debería ser igual al monto del pago (" ++ Monto.toString esperado ++ "), " ++ Monto.diffText actual esperado

        ErrorEquitativoSinParticipantes ->
            "No hay participantes especificados"

        ErrorRepartijaSinItems ->
            "No hay items para repartir."

        ErrorRepartijaTotalItemsNoCoincide totalItems totalPago ->
            "El total de items (" ++ Monto.toString totalItems ++ ") debería ser igual al monto del pago (" ++ Monto.toString totalPago ++ "), " ++ Monto.diffText totalItems totalPago

        ErrorRepartijaSinClaims ->
            "Nadie reclamo ningun item."

        ErrorRepartijaTotalReclamadoNoCoincide totalReclamado totalPago ->
            "El total reclamado (" ++ Monto.toString totalReclamado ++ ") no coincide con el monto del pago (" ++ Monto.toString totalPago ++ "), " ++ Monto.diffText totalReclamado totalPago


errorAccionableEn : TipoErrorResumen -> List LugarParaAccionar
errorAccionableEn tipo =
    case tipo of
        ErrorMontosEspecificosVacios ->
            [ Lugar_CreacionPago ]

        ErrorMontosEspecificosTotalNoCoincide _ _ ->
            [ Lugar_CreacionPago ]

        ErrorEquitativoSinParticipantes ->
            [ Lugar_CreacionPago ]

        ErrorRepartijaSinItems ->
            [ Lugar_CreacionPago ]

        ErrorRepartijaTotalItemsNoCoincide _ _ ->
            [ Lugar_CreacionPago ]

        ErrorRepartijaSinClaims ->
            [ Lugar_PaginaRepartija ]

        ErrorRepartijaTotalReclamadoNoCoincide _ _ ->
            [ Lugar_PaginaRepartija ]


getDeudasFromResumen : ResumenNetos -> Maybe (Netos Monto)
getDeudasFromResumen resumen =
    Just resumen.netos


viewResumenPanel :
    GrupoLike g
    -> WebData ResumenPago
    -> (ResumenPago -> ResumenNetos)
    -> Bool
    -> List (Html Msg)
    -> Html Msg
viewResumenPanel grupo resumenData accessor negateMontos extraContent =
    div [ style "flex" "1", style "min-width" "300px" ]
        [ Ui5.panel
            [ Attr.attribute "header-text" "Resumen" ]
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
                    [ p [] [ Ui5.text <| "total: " ++ (Maybe.withDefault "???" <| Maybe.map Monto.toString <| getTotalFromResumen resumenNetos) ]
                    , p []
                        [ Ui5.text <|
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
                    [ Ui5.busyIndicator [ Attr.attribute "active" "" ] [] ]

                Failure _ ->
                    [ Ui5.messageStrip
                        [ Attr.attribute "design" "Negative"
                        , Attr.attribute "hide-close-button" ""
                        ]
                        [ text "Error al cargar el resumen" ]
                    ]
            )
        ]


viewUnsavedChangesBanner : Html Msg
viewUnsavedChangesBanner =
    Ui5.messageStrip
        [ Attr.attribute "design" "Critical"
        , style "margin-bottom" "1rem"
        ]
        [ text "Hay cambios sin guardar" ]


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
                , Ui5.wizard
                    [ Attr.attribute "content-layout" "SingleStep"
                    , on "step-change"
                        (Decode.at [ "detail", "step", "dataset", "section" ] Decode.string
                            |> Decode.andThen
                                (\s ->
                                    case s of
                                        "basic" ->
                                            Decode.succeed (SelectSection BasicPagoData)

                                        "pagadores" ->
                                            Decode.succeed (SelectSection PagadoresSection)

                                        "deudores" ->
                                            Decode.succeed (SelectSection DeudoresSection)

                                        "confirmation" ->
                                            Decode.succeed (SelectSection PagoConfirmation)

                                        _ ->
                                            Decode.fail "unknown section"
                                )
                        )
                    ]
                    [ Ui5.wizardStep
                        [ Attr.attribute "title-text" "Datos básicos"
                        , Attr.attribute "data-section" "basic"
                        , selected (model.currentSection == BasicPagoData)
                        ]
                        [ p [] [ Ui5.text "Ingresá la información básica del pago: un nombre descriptivo y el monto total." ]
                        , pagoForm model.pagoBasicoForm
                        ]
                    , Ui5.wizardStep
                        [ Attr.attribute "title-text" "Pagadores"
                        , Attr.attribute "data-section" "pagadores"
                        , selected (model.currentSection == PagadoresSection)
                        ]
                        [ div [ style "display" "flex", style "gap" "1.5rem", style "flex-wrap" "wrap" ]
                            [ div [ style "flex" "1", style "min-width" "300px" ]
                                [ Html.form [ onSubmit <| SubmitCurrentSection ]
                                    [ p [] [ Ui5.text "Indicá quién pagó y cómo se distribuye el gasto entre los que pusieron plata." ]
                                    , distribucionForm grupo.participantes "distribucion_pagadores" model.pagadoresForm model.receiptParseState
                                    , viewErrorFromResumenData model.resumenPagadores .resumenPagadores
                                    , Ui5.button
                                        [ Attr.attribute "design" "Emphasized"
                                        , disabled (Form.getOutput model.pagadoresForm == Nothing || hasActionableErrors Lugar_CreacionPago model.resumenPagadores .resumenPagadores)
                                        , onClick SubmitCurrentSection
                                        ]
                                        [ text "Siguiente seccion" ]
                                    ]
                                ]
                            , viewResumenPanel grupo model.resumenPagadores .resumenPagadores False []
                            ]
                        ]
                    , Ui5.wizardStep
                        [ Attr.attribute "title-text" "Deudores"
                        , Attr.attribute "data-section" "deudores"
                        , selected (model.currentSection == DeudoresSection)
                        ]
                        [ div [ style "display" "flex", style "gap" "1.5rem", style "flex-wrap" "wrap" ]
                            [ div [ style "flex" "1", style "min-width" "300px" ]
                                [ Html.form [ onSubmit <| SubmitCurrentSection ]
                                    [ p [] [ Ui5.text "Indicá quiénes deben y cómo se reparte la deuda entre ellos." ]
                                    , distribucionForm grupo.participantes "distribucion_deudores" model.deudoresForm model.receiptParseState
                                    , viewErrorFromResumenData model.resumenDeudores .resumenDeudores
                                    , Ui5.button
                                        [ Attr.attribute "design" "Emphasized"
                                        , disabled (Form.getOutput model.deudoresForm == Nothing || hasActionableErrors Lugar_CreacionPago model.resumenDeudores .resumenDeudores)
                                        , onClick SubmitCurrentSection
                                        ]
                                        [ text "Siguiente seccion" ]
                                    ]
                                ]
                            , viewResumenPanel grupo model.resumenDeudores .resumenDeudores True []
                            ]
                        ]
                    , Ui5.wizardStep
                        [ Attr.attribute "title-text" "Detalles"
                        , Attr.attribute "data-section" "confirmation"
                        , selected (model.currentSection == PagoConfirmation)
                        ]
                        [ Html.form [ onSubmit <| PagoForm Form.Submit ]
                            [ p [] [ Ui5.text "Revisá el resumen del pago antes de confirmar. Verificá que los montos y participantes sean correctos." ]
                            , viewResumenPanel grupo
                                model.resumenPago
                                .resumen
                                False
                                (case Form.getOutput model.pagoForm of
                                    Just pago ->
                                        [ case pago.pagadores.tipo of
                                            TipoDistribucionRepartija repartija ->
                                                if repartija.id /= emptyUlid then
                                                    p [] <|
                                                        [ Ui5.link
                                                            [ Path.href <| Path.Grupos_GrupoId__Repartijas_RepartijaId_ { grupoId = model.grupoId, repartijaId = repartija.id }
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
                                                    p [] <|
                                                        [ a
                                                            [ Path.href <| Path.Grupos_GrupoId__Repartijas_RepartijaId_ { grupoId = model.grupoId, repartijaId = repartija.id }
                                                            , target "_blank"
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
                                Ui5.button
                                    [ Attr.attribute "design" "Emphasized"
                                    , disabled (hasActionableErrors Lugar_CreacionPago model.resumenPago .resumen)
                                    , onClick (PagoForm Form.Submit)
                                    , id "pago-submit-button"
                                    ]
                                    [ textoCTA
                                    ]

                              else
                                text ""
                            ]
                        ]
                    ]
                ]
            }

        NotAsked ->
            { title = "BananaSplit"
            , body = [ Ui5.text "Cargando" ]
            }

        Loading ->
            { title = "BananaSplit"
            , body = [ Ui5.text "Cargando" ]
            }

        Failure e ->
            { title = "BananaSplit"
            , body =
                [ details []
                    [ summary []
                        [ Ui5.text "Algo salio mal" ]
                    , viewHttpError e
                    ]
                ]
            }


pagoForm : Form CustomFormError Pago -> Html Msg
pagoForm form =
    let
        nombreField =
            Form.getFieldAsString "nombre" form

        montoField =
            Form.getFieldAsString "monto" form
    in
    Ui5.form (always SubmitCurrentSection)
        [ Attr.attribute "label-span" "S12 M12 L12 XL12"
        ]
        [ Html.map PagoForm <|
            Ui5.textFormItem nombreField
                { label = "Nombre"
                , placeholder = Just "Pago de deudas"
                , required = True
                }
        , Html.map PagoForm <|
            Ui5.textFormItem montoField
                { label = "Monto"
                , placeholder = Just "2000"
                , required = True
                }
        , Ui5.button
            [ Attr.attribute "design" "Emphasized"
            , disabled (Form.getOutput form == Nothing)
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


distribucionForm : List Participante -> String -> Form CustomFormError Pago -> Maybe ReceiptReadingState -> Html Msg
distribucionForm participantes prefix form receiptParseState =
    let
        tipoField =
            Form.getFieldAsString (prefix ++ ".tipo") form
    in
    div [] <|
        div [ style "margin-bottom" "1rem" ]
            [ Html.map PagoForm <|
                Ui5.segmentedButton
                    [ on "selection-change"
                        (Decode.at [ "detail", "selectedItems", "0", "dataset", "id" ] Decode.string
                            |> Decode.map (\v -> Form.Input tipoField.path Form.Select (FormField.String v))
                        )
                    ]
                    [ Ui5.segmentedButtonItem
                        [ Attr.attribute "data-id" "monto_equitativo"
                        , selected <| tipoField.value == Just "monto_equitativo"
                        ]
                        [ text "Equitativo" ]
                    , Ui5.segmentedButtonItem
                        [ Attr.attribute "data-id" "montos_especificos"
                        , selected <| tipoField.value == Just "montos_especificos"
                        ]
                        [ text "Específico" ]
                    , Ui5.segmentedButtonItem
                        [ Attr.attribute "data-id" "repartija"
                        , selected <| tipoField.value == Just "repartija"
                        ]
                        [ text "Repartija" ]
                    ]
            ]
            :: (case tipoField.value of
                    Just "repartija" ->
                        [ p [ style "margin-bottom" "1rem" ]
                            [ Ui5.text "División por items del recibo. Podés subir una foto del ticket para que se complete automáticamente." ]
                        , repartijaForm prefix form receiptParseState
                        ]

                    Just "montos_especificos" ->
                        let
                            montosIndexes =
                                Form.getListIndexes (prefix ++ ".montos") form
                        in
                        [ p [ style "margin-bottom" "1rem" ]
                            [ Ui5.text "Cada participante pone/debe un monto fijo que vos especificás." ]
                        , div [ style "margin-bottom" "0.5rem" ] <|
                            (montosIndexes
                                |> List.map
                                    (\i ->
                                        let
                                            montoField =
                                                Form.getFieldAsString (prefix ++ ".montos." ++ String.fromInt i ++ ".monto") form

                                            participanteField =
                                                Form.getFieldAsString (prefix ++ ".montos." ++ String.fromInt i ++ ".participante") form
                                        in
                                        div [ style "display" "flex", style "gap" "0.5rem", style "align-items" "end", style "margin-bottom" "0.5rem" ]
                                            [ Html.map PagoForm <|
                                                Ui5.textInput montoField
                                                    [ placeholder "200.0" ]
                                            , Html.map PagoForm <|
                                                Ui5.formSelect
                                                    (( "", "" ) :: List.map (\p -> ( p.participanteId, p.participanteNombre )) participantes)
                                                    participanteField
                                                    []
                                            , Ui5.button
                                                [ Attr.attribute "design" "Negative"
                                                , Attr.attribute "icon" "delete"
                                                , type_ "button"
                                                , onClick <| PagoForm <| Form.RemoveItem (prefix ++ ".montos") i
                                                ]
                                                []
                                            ]
                                    )
                            )
                                ++ [ Ui5.button
                                        [ Attr.attribute "icon" "add"
                                        , onClick <| PagoForm <| Form.Append (prefix ++ ".montos")
                                        , type_ "button"
                                        ]
                                        [ text "Agregar" ]
                                   ]
                        ]

                    Just "monto_equitativo" ->
                        [ p [ style "margin-bottom" "1rem" ]
                            [ Ui5.text "El monto se divide en partes iguales entre los participantes seleccionados." ]
                        , div [ style "margin-bottom" "0.5rem" ]
                            (participantes
                                |> List.map
                                    (\p ->
                                        let
                                            participanteField =
                                                Form.getFieldAsBool (prefix ++ ".participantes." ++ p.participanteId) form
                                        in
                                        Html.map PagoForm <|
                                            Ui5.formCheckbox participanteField
                                                [ Attr.attribute "text" p.participanteNombre ]
                                    )
                            )
                        ]

                    _ ->
                        []
               )


repartijaForm : String -> Form CustomFormError Pago -> Maybe ReceiptReadingState -> Html Msg
repartijaForm prefix form receiptParseState =
    let
        montoField =
            Form.getFieldAsString (prefix ++ ".extra") form

        distribucionDeSobrasField =
            Form.getFieldAsString (prefix ++ ".distribucionDeSobras") form

        itemsIndexes =
            Form.getListIndexes (prefix ++ ".items") form
    in
    div []
        [ div [ style "margin-bottom" "1rem" ]
            [ Ui5.fileUploader
                [ accept "image/*"
                , on "change" (Decode.map ReceiptImageSelected fileDecoder)
                ]
                []
            ]
        , case receiptParseState of
            Just ReadingFile ->
                div []
                    [ Ui5.busyIndicator [ Attr.attribute "active" "", Attr.attribute "size" "M" ] []
                    , Ui5.messageStrip
                        [ Attr.attribute "design" "Information", style "margin-bottom" "1rem" ]
                        [ text "Leyendo la imagen..." ]
                    ]

            Just ProcessingWithAI ->
                div []
                    [ Ui5.busyIndicator [ Attr.attribute "active" "", Attr.attribute "size" "M" ] []
                    , Ui5.messageStrip
                        [ Attr.attribute "design" "Information", style "margin-bottom" "1rem" ]
                        [ text "Analizando el recibo con inteligencia artificial..." ]
                    , Ui5.messageStrip
                        [ Attr.attribute "design" "Warning", style "margin-bottom" "1rem" ]
                        [ text "Esto podria tomar varios minutos, no cierres esta ventana" ]
                    ]

            Just (ErrorProcessing errorMsg) ->
                div []
                    [ Ui5.messageStrip
                        [ Attr.attribute "design" "Negative"
                        , style "margin-bottom" "1rem"
                        , on "close" (Decode.succeed ClearReceiptError)
                        ]
                        [ text ("Algo salio mal: " ++ errorMsg) ]
                    ]

            Nothing ->
                text ""
        , Ui5.formLayout
            [ Attr.attribute "label-span" "S12 M12 L12 XL12" ]
            [ Ui5.formGroup [ Attr.attribute "header-text" "Items" ]
                [ Ui5.table
                    [ Attr.attribute "row-action-count" "1" ]
                    (Ui5.tableHeaderRow
                        [ Ui5.slot "headerRow" ]
                        [ Ui5.tableHeaderCell [ Attr.attribute "width" "auto", Attr.attribute "min-width" "12rem" ] [ text "Item" ]
                        , Ui5.tableHeaderCell [ Attr.attribute "min-width" "10rem" ] [ text "Monto total" ]
                        , Ui5.tableHeaderCell [ Attr.attribute "min-width" "8rem" ] [ text "Cantidad" ]
                        ]
                        :: List.map
                            (\i -> repartijaItemForm i prefix form)
                            itemsIndexes
                    )
                , Ui5.button
                    [ Attr.attribute "icon" "add"
                    , onClick <| PagoForm <| Form.Append <| prefix ++ ".items"
                    , type_ "button"
                    ]
                    [ text "Agregar item" ]
                ]
            , Ui5.formGroup [ Attr.attribute "header-text" "Propina" ]
                [ Html.map PagoForm <|
                    Ui5.textInput montoField
                        [ placeholder "1000" ]
                ]
            ]
        , Html.hr [ style "border" "none", style "border-top" "1px solid var(--sapGroup_ContentBorderColor)", style "margin" "1rem 0" ] []
        , Ui5.formLayout
            [ Attr.attribute "label-span" "S12 M12 L12 XL12" ]
            [ Ui5.formGroup
                [ Attr.attribute "header-text" "Distribución de sobras" ]
                [ p [ style "margin-bottom" "0.5rem", style "font-size" "0.875rem", style "color" "var(--sapNeutralTextColor)" ]
                    [ text "Qué hacer con los items que nadie reclamó. Podés cambiar esta opción en cualquier momento." ]
                , Html.map PagoForm <|
                    Ui5.segmentedButton
                        [ on "selection-change"
                            (Decode.at [ "detail", "selectedItems", "0", "dataset", "id" ] Decode.string
                                |> Decode.map (\v -> Form.Input distribucionDeSobrasField.path Form.Select (FormField.String v))
                            )
                        ]
                        [ Ui5.segmentedButtonItem
                            [ Attr.attribute "data-id" "SobrasNoDistribuir"
                            , selected (distribucionDeSobrasField.value == Just "SobrasNoDistribuir")
                            ]
                            [ text "No distribuir" ]
                        , Ui5.segmentedButtonItem
                            [ Attr.attribute "data-id" "SobrasProporcional"
                            , selected (distribucionDeSobrasField.value == Just "SobrasProporcional")
                            ]
                            [ text "Proporcional" ]
                        ]
                ]
            ]
        ]


repartijaItemForm : Int -> String -> Form CustomFormError Pago -> Html Msg
repartijaItemForm i prefix form =
    let
        nombreField =
            Form.getFieldAsString (prefix ++ ".items." ++ String.fromInt i ++ ".nombre") form

        montoField =
            Form.getFieldAsString (prefix ++ ".items." ++ String.fromInt i ++ ".monto") form

        cantidadField =
            Form.getFieldAsString (prefix ++ ".items." ++ String.fromInt i ++ ".cantidad") form
    in
    Ui5.tableRow
        []
        [ Ui5.tableCell
            []
            [ Html.map PagoForm <|
                Ui5.textInput nombreField
                    [ placeholder "Birrita" ]
            ]
        , Ui5.tableCell
            []
            [ Html.map PagoForm <|
                Ui5.textInput montoField
                    [ placeholder "20000" ]
            ]
        , Ui5.tableCell
            []
            [ Html.map PagoForm <|
                Ui5.textInput cantidadField
                    [ placeholder "4" ]
            ]
        , Ui5.tableRowAction
            [ Ui5.slot "actions"
            , Attr.attribute "icon" "delete"
            , Attr.attribute "text" "Eliminar"
            , Attr.attribute "tooltip" "Eliminar item"
            , on "click" (Decode.succeed <| PagoForm <| Form.RemoveItem (prefix ++ ".items") i)
            ]
            []
        ]


allowedMimeTypesForReceiptUpload : List String
allowedMimeTypesForReceiptUpload =
    [ "image/png"
    , "image/jpeg"
    , "image/webp"
    , "image/gif"
    ]
