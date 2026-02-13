module Pages.Grupos.GrupoId_.Pagos.New exposing (Model, Msg(..), Section(..), andThenSendWarningOnExit, page, subscriptions, update, validatePago, validatePagoInSection, view, waitAndCheckNecessaryData)

import Base64.Encode
import Browser.Dom
import Bytes exposing (Bytes)
import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.NavBar as NavBar
import Components.Ui5 exposing (..)
import Effect exposing (Effect)
import File exposing (File)
import Form exposing (Form)
import Form.Error as FormError
import Form.Field as FormField
import Form.Init as Form
import Form.Validate as V exposing (Validation, nonEmpty)
import Generated.Api as Api exposing (Distribucion, ErrorResumen(..), Grupo, Monto, Netos, Pago, Parte(..), Participante, ParticipanteId, Repartija, RepartijaItem, ResumenNetos(..), ResumenPago, ShallowGrupo, TipoDistribucion(..), ULID)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (on, onClick, onSubmit)
import Http
import Json.Decode as Decode
import Layouts
import Models.Grupo exposing (lookupNombreParticipante)
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
import Utils.Form exposing (..)
import Utils.Http exposing (viewHttpError)
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts exposing (ToastLevel(..))
import Utils.Ulid exposing (emptyUlid)
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.grupoId shared.store
        , update = update shared.store shared.userId
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
                ]
            , Form.setGroup "distribucion_deudores"
                [ Form.setString "tipo" "monto_equitativo"
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


validateRepartija : V.Validation CustomFormError Repartija
validateRepartija =
    V.succeed Repartija
        |> V.andMap (V.field "repartija_id" validateId)
        |> V.andMap (V.succeed "GENERATED")
        |> V.andMap (V.field "extra" Monto.validateMonto)
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


update : Store -> Maybe ULID -> Msg -> Model -> ( Model, Effect Msg )
update store userId msg model =
    let
        participantes =
            case Store.getGrupo model.grupoId store of
                Success grupo ->
                    grupo.participantes

                Failure e ->
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

        AddedPagoResponse (Err error) ->
            ( model
            , Effect.batch
                [ Toasts.pushToast Toasts.ToastDanger "Falló la creación del pago"
                , Store.refreshPagos model.grupoId
                ]
            )

        UpdatedPagoResponse (Ok pago) ->
            ( { model | hasUnsavedChanges = False }
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshPagos model.grupoId

                -- , Effect.pushRoutePath <| Path.Grupos_GrupoId__Pagos { grupoId = model.grupoId }
                , Toasts.pushToast Toasts.ToastSuccess "Se actualizó el pago"
                ]
            )
                |> andThenSendWarningOnExit

        UpdatedPagoResponse (Err error) ->
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

                ( _, _ ) ->
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
                            let
                                initialFormValues =
                                    [ Form.setString "id" pagoId
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
                            ( { model
                                | pagoForm = Form.initial initialFormValues (validatePago grupo.participantes)
                                , pagadoresForm = Form.initial initialFormValues (validatePagoInSection PagadoresSection grupo.participantes)
                                , deudoresForm = Form.initial initialFormValues (validatePagoInSection DeudoresSection grupo.participantes)
                                , pagoBasicoForm = Form.initial initialFormValues (validatePagoInSection BasicPagoData grupo.participantes)
                                , resumenPago = Loading
                                , storedClaims = Just claimsToStore
                                , hasUnsavedChanges = False
                              }
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
                    let
                        pagoWithClaims =
                            mergeClaimsIntoPago model.storedClaims pago
                    in
                    if Form.getOutput (getForm originalModel) == Just pago then
                        Effect.none

                    else
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
subscriptions model =
    Sub.none



-- VIEW


getTotalFromResumen : ResumenNetos -> Maybe Monto
getTotalFromResumen resumen =
    case resumen of
        NetosIncomputables total _ ->
            total

        ResumenNetos total _ ->
            total


getParticipantesFromResumen : ResumenNetos -> Maybe (List ParticipanteId)
getParticipantesFromResumen resumen =
    case resumen of
        NetosIncomputables _ _ ->
            Nothing

        ResumenNetos _ deudas ->
            deudas
                |> List.map (\( p, _ ) -> p)
                |> Just


getErrorFromResumen : ResumenNetos -> Maybe String
getErrorFromResumen resumen =
    case resumen of
        NetosIncomputables _ (ErrorResumen error _) ->
            error

        ResumenNetos _ _ ->
            Nothing


getDeudasFromResumen : ResumenNetos -> Maybe (Netos Monto)
getDeudasFromResumen resumen =
    case resumen of
        NetosIncomputables _ _ ->
            Nothing

        ResumenNetos _ deudas ->
            Just deudas


viewUnsavedChangesBanner : Html Msg
viewUnsavedChangesBanner =
    Html.node "ui5-message-strip"
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
                , Html.node "ui5-wizard"
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
                    [ Html.node "ui5-wizard-step"
                        [ Attr.attribute "title-text" "Datos básicos"
                        , Attr.attribute "data-section" "basic"
                        , selected (model.currentSection == BasicPagoData)
                        ]
                        [ p [] [ text "Ingresá la información básica del pago: un nombre descriptivo y el monto total." ]
                        , pagoForm grupo.participantes model.pagoBasicoForm
                        ]
                    , Html.node "ui5-wizard-step"
                        [ Attr.attribute "title-text" "Pagadores"
                        , Attr.attribute "data-section" "pagadores"
                        , selected (model.currentSection == PagadoresSection)
                        ]
                        [ Html.form [ onSubmit <| SubmitCurrentSection ]
                            [ p [] [ text "Indicá quién pagó y cómo se distribuye el gasto entre los que pusieron plata." ]
                            , distribucionForm grupo.participantes "distribucion_pagadores" model.pagadoresForm model.receiptParseState
                            , Html.node "ui5-button"
                                [ Attr.attribute "design" "Emphasized"
                                , disabled (Form.getOutput model.pagadoresForm == Nothing)
                                , onClick SubmitCurrentSection
                                ]
                                [ text "Siguiente seccion" ]
                            , case model.resumenPagadores of
                                Success resumen ->
                                    div []
                                        [ Html.section []
                                            [ h1 [] [ text "Pagadores" ]
                                            , p [] [ text <| "total: ", text <| Maybe.withDefault "???" <| Maybe.map Monto.toString <| getTotalFromResumen resumen.resumenPagadores ]
                                            , p []
                                                [ text <| "pagadores: "
                                                , resumen.resumenPagadores
                                                    |> getParticipantesFromResumen
                                                    |> Maybe.map (\ps -> ps |> List.map (lookupNombreParticipante grupo) |> String.join ", ")
                                                    |> Maybe.withDefault "???"
                                                    |> text
                                                ]
                                            , p [] [ text <| Maybe.withDefault "" <| Maybe.map (\s -> "problemas: " ++ s) <| getErrorFromResumen resumen.resumenPagadores ]
                                            , p [] [ Maybe.withDefault (text "") <| Maybe.map (viewNetosBarras grupo) <| getDeudasFromResumen <| resumen.resumenPagadores ]
                                            ]
                                        ]

                                NotAsked ->
                                    text ""

                                Loading ->
                                    text "cargando..."

                                Failure _ ->
                                    text "error"
                            ]
                        ]
                    , Html.node "ui5-wizard-step"
                        [ Attr.attribute "title-text" "Deudores"
                        , Attr.attribute "data-section" "deudores"
                        , selected (model.currentSection == DeudoresSection)
                        ]
                        [ Html.form [ onSubmit <| SubmitCurrentSection ]
                            [ p [] [ text "Indicá quiénes deben y cómo se reparte la deuda entre ellos." ]
                            , distribucionForm grupo.participantes "distribucion_deudores" model.deudoresForm model.receiptParseState
                            , Html.node "ui5-button"
                                [ Attr.attribute "design" "Emphasized"
                                , disabled (Form.getOutput model.deudoresForm == Nothing)
                                , onClick SubmitCurrentSection
                                ]
                                [ text "Siguiente seccion" ]
                            , case model.resumenDeudores of
                                Success resumen ->
                                    div []
                                        [ Html.section []
                                            [ h1 [] [ text "Deudores" ]
                                            , p [] [ text <| "total: ", text <| Maybe.withDefault "???" <| Maybe.map Monto.toString <| getTotalFromResumen resumen.resumenDeudores ]
                                            , p []
                                                [ text <| "deudores: "
                                                , resumen.resumenDeudores
                                                    |> getParticipantesFromResumen
                                                    |> Maybe.map (\ps -> ps |> List.map (lookupNombreParticipante grupo) |> String.join ", ")
                                                    |> Maybe.withDefault "???"
                                                    |> text
                                                ]
                                            , p [] [ text <| Maybe.withDefault "" <| Maybe.map (\s -> "problemas: " ++ s) <| getErrorFromResumen resumen.resumenDeudores ]
                                            , p [] [ Maybe.withDefault (text "") <| Maybe.map (viewNetosBarras grupo) <| Maybe.map (List.map (\( pp, m ) -> ( pp, Monto.negate m ))) <| getDeudasFromResumen <| resumen.resumenDeudores ]
                                            ]
                                        ]

                                NotAsked ->
                                    text ""

                                Loading ->
                                    text "cargando..."

                                Failure _ ->
                                    text "error"
                            ]
                        ]
                    , Html.node "ui5-wizard-step"
                        [ Attr.attribute "title-text" "Detalles"
                        , Attr.attribute "data-section" "confirmation"
                        , selected (model.currentSection == PagoConfirmation)
                        ]
                        [ Html.form [ onSubmit <| PagoForm Form.Submit ]
                            [ p [] [ text "Revisá el resumen del pago antes de confirmar. Verificá que los montos y participantes sean correctos." ]
                            , case model.resumenPago of
                                Success resumen ->
                                    Html.section [] <|
                                        [ h1 [] [ text "Pago" ]
                                        , p [] [ text <| "total: ", text <| Maybe.withDefault "???" <| Maybe.map Monto.toString <| getTotalFromResumen resumen.resumen ]
                                        , p []
                                            [ text <| "participantes: "
                                            , resumen.resumen
                                                |> getParticipantesFromResumen
                                                |> Maybe.map (\ps -> ps |> List.map (lookupNombreParticipante grupo) |> String.join ", ")
                                                |> Maybe.withDefault "???"
                                                |> text
                                            ]
                                        , p [] [ text <| Maybe.withDefault "" <| Maybe.map (\s -> "problemas: " ++ s) <| getErrorFromResumen resumen.resumen ]
                                        , p [] [ Maybe.withDefault (text "") <| Maybe.map (viewNetosBarras grupo) <| getDeudasFromResumen <| resumen.resumen ]
                                        ]
                                            ++ (case Form.getOutput model.pagoForm of
                                                    Just pago ->
                                                        [ case pago.pagadores.tipo of
                                                            TipoDistribucionRepartija repartija ->
                                                                if repartija.id /= emptyUlid then
                                                                    p [] <|
                                                                        [ node "ui5-link"
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

                                NotAsked ->
                                    text "notasked"

                                Loading ->
                                    text "loading"

                                Failure e ->
                                    viewHttpError e
                            , let
                                textoCTA =
                                    case model.currentPagoId of
                                        Nothing ->
                                            text "Crear pago"

                                        Just _ ->
                                            text "Actualizar pago"
                              in
                              if model.hasUnsavedChanges then
                                Html.node "ui5-button"
                                    [ Attr.attribute "design" "Emphasized"
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


pagoForm : List Participante -> Form CustomFormError Pago -> Html Msg
pagoForm participantes form =
    let
        nombreField =
            Form.getFieldAsString "nombre" form

        montoField =
            Form.getFieldAsString "monto" form
    in
    Html.form [ onSubmit <| SubmitCurrentSection ]
        [ Html.node "ui5-form"
            [ Attr.attribute "label-span" "S12 M12 L12 XL12"
            ]
            [ Html.map PagoForm <|
                ui5TextFormItem nombreField
                    { label = "Nombre"
                    , placeholder = Just "Pago de deudas"
                    , required = True
                    }
            , Html.map PagoForm <|
                ui5TextFormItem montoField
                    { label = "Monto"
                    , placeholder = Just "2000"
                    , required = True
                    }
            ]
        , Html.node "ui5-button"
            [ Attr.attribute "design" "Emphasized"
            , disabled (Form.getOutput form == Nothing)
            , onClick SubmitCurrentSection
            ]
            [ text "Siguiente seccion" ]
        ]


fileDecoder : Decode.Decoder File
fileDecoder =
    Decode.at [ "target", "files", "0" ] File.decoder


distribucionForm : List Participante -> String -> Form CustomFormError Pago -> Maybe ReceiptReadingState -> Html Msg
distribucionForm participantes prefix form receiptParseState =
    let
        tipoField =
            Form.getFieldAsString (prefix ++ ".tipo") form
    in
    div [] <|
        div [ style "margin-bottom" "1rem" ]
            [ Html.map PagoForm <|
                Html.node "ui5-segmented-button"
                    [ on "selection-change"
                        (Decode.at [ "detail", "selectedItems", "0", "dataset", "id" ] Decode.string
                            |> Decode.map (\v -> Form.Input tipoField.path Form.Select (FormField.String v))
                        )
                    ]
                    [ Html.node "ui5-segmented-button-item"
                        [ Attr.attribute "data-id" "monto_equitativo"
                        , if tipoField.value == Just "monto_equitativo" then
                            Attr.attribute "selected" ""

                          else
                            class ""
                        ]
                        [ text "Equitativo" ]
                    , Html.node "ui5-segmented-button-item"
                        [ Attr.attribute "data-id" "montos_especificos"
                        , if tipoField.value == Just "montos_especificos" then
                            Attr.attribute "selected" ""

                          else
                            class ""
                        ]
                        [ text "Específico" ]
                    , Html.node "ui5-segmented-button-item"
                        [ Attr.attribute "data-id" "repartija"
                        , if tipoField.value == Just "repartija" then
                            Attr.attribute "selected" ""

                          else
                            class ""
                        ]
                        [ text "Repartija" ]
                    ]
            ]
            :: (case tipoField.value of
                    Just "repartija" ->
                        [ p [ style "margin-bottom" "1rem" ]
                            [ text "División por items del recibo. Podés subir una foto del ticket para que se complete automáticamente." ]
                        , repartijaForm prefix form receiptParseState
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
                                    (\i ->
                                        let
                                            montoField =
                                                Form.getFieldAsString (prefix ++ ".montos." ++ String.fromInt i ++ ".monto") form

                                            participanteField =
                                                Form.getFieldAsString (prefix ++ ".montos." ++ String.fromInt i ++ ".participante") form
                                        in
                                        div [ style "display" "flex", style "gap" "0.5rem", style "align-items" "end", style "margin-bottom" "0.5rem" ]
                                            [ Html.map PagoForm <|
                                                ui5TextInput montoField
                                                    [ placeholder "200.0" ]
                                            , Html.map PagoForm <|
                                                ui5Select
                                                    (( "", "" ) :: List.map (\p -> ( p.participanteId, p.participanteNombre )) participantes)
                                                    participanteField
                                                    []
                                            , Html.node "ui5-button"
                                                [ Attr.attribute "design" "Negative"
                                                , Attr.attribute "icon" "delete"
                                                , type_ "button"
                                                , onClick <| PagoForm <| Form.RemoveItem (prefix ++ ".montos") i
                                                ]
                                                []
                                            ]
                                    )
                            )
                                ++ [ Html.node "ui5-button"
                                        [ Attr.attribute "icon" "add"
                                        , onClick <| PagoForm <| Form.Append (prefix ++ ".montos")
                                        , type_ "button"
                                        ]
                                        [ text "Agregar" ]
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
                                                Form.getFieldAsBool (prefix ++ ".participantes." ++ p.participanteId) form
                                        in
                                        Html.map PagoForm <|
                                            ui5CheckBox participanteField
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

        itemsIndexes =
            Form.getListIndexes (prefix ++ ".items") form
    in
    div []
        [ div [ style "margin-bottom" "1rem" ]
            [ Html.node "ui5-file-uploader"
                [ accept "image/*"
                , on "change" (Decode.map ReceiptImageSelected fileDecoder)
                ]
                []
            ]
        , case receiptParseState of
            Just ReadingFile ->
                div []
                    [ Html.node "ui5-busy-indicator" [ Attr.attribute "active" "", Attr.attribute "size" "M" ] []
                    , Html.node "ui5-message-strip"
                        [ Attr.attribute "design" "Information", style "margin-bottom" "1rem" ]
                        [ text "Leyendo la imagen..." ]
                    ]

            Just ProcessingWithAI ->
                div []
                    [ Html.node "ui5-busy-indicator" [ Attr.attribute "active" "", Attr.attribute "size" "M" ] []
                    , Html.node "ui5-message-strip"
                        [ Attr.attribute "design" "Information", style "margin-bottom" "1rem" ]
                        [ text "Analizando el recibo con inteligencia artificial..." ]
                    , Html.node "ui5-message-strip"
                        [ Attr.attribute "design" "Warning", style "margin-bottom" "1rem" ]
                        [ text "Esto podria tomar varios minutos, no cierres esta ventana" ]
                    ]

            Just (ErrorProcessing errorMsg) ->
                div []
                    [ Html.node "ui5-message-strip"
                        [ Attr.attribute "design" "Negative"
                        , style "margin-bottom" "1rem"
                        , on "close" (Decode.succeed ClearReceiptError)
                        ]
                        [ text ("Algo salio mal: " ++ errorMsg) ]
                    ]

            Nothing ->
                text ""
        , Html.node "ui5-table"
            [ Attr.attribute "row-action-count" "1" ]
            (Html.node "ui5-table-header-row"
                [ Attr.attribute "slot" "headerRow" ]
                [ Html.node "ui5-table-header-cell" [] [ text "Item" ]
                , Html.node "ui5-table-header-cell" [] [ text "Monto total" ]
                , Html.node "ui5-table-header-cell" [] [ text "Cantidad" ]
                ]
                :: List.map
                    (\i -> repartijaItemForm i prefix form)
                    itemsIndexes
            )
        , div [ style "margin-bottom" "1rem" ]
            [ Html.node "ui5-button"
                [ Attr.attribute "icon" "add"
                , onClick <| PagoForm <| Form.Append <| prefix ++ ".items"
                , type_ "button"
                ]
                [ text "Agregar item" ]
            ]
        , div [ style "margin-bottom" "1rem" ]
            [ Html.node "ui5-label" [ Attr.attribute "show-colon" "" ] [ text "Propina" ]
            , div [ style "margin-top" "0.5rem" ]
                [ Html.map PagoForm <|
                    ui5TextInput montoField
                        [ placeholder "1000" ]
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
    Html.node "ui5-table-row"
        []
        [ Html.node "ui5-table-cell"
            []
            [ Html.map PagoForm <|
                ui5TextInput nombreField
                    [ placeholder "Birrita" ]
            ]
        , Html.node "ui5-table-cell"
            []
            [ Html.map PagoForm <|
                ui5TextInput montoField
                    [ placeholder "20000" ]
            ]
        , Html.node "ui5-table-cell"
            []
            [ Html.map PagoForm <|
                ui5TextInput cantidadField
                    [ placeholder "4" ]
            ]
        , Html.node "ui5-table-row-action"
            [ Attr.attribute "slot" "actions"
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
