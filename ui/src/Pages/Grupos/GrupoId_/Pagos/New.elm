module Pages.Grupos.GrupoId_.Pagos.New exposing (Model, Msg(..), Section(..), page, subscriptions, update, validatePago, validatePagoInSection, view, waitAndCheckNecessaryData)

import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.NavBar as NavBar
import Effect exposing (Effect)
import FeatherIcons as Icons
import Form exposing (Form)
import Form.Error as FormError
import Form.Field as FormField
import Form.Init as Form
import Form.Input as FormInput
import Form.Validate as V exposing (Validation, nonEmpty)
import Generated.Api as Api exposing (Deudas, Distribucion, ErrorResumen(..), Grupo, Monto, Netos, Pago, Parte(..), Participante, ParticipanteId, Repartija, RepartijaItem, ResumenDeudas(..), ResumenPago, ShallowGrupo, TipoDistribucion(..), ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit)
import Http
import Json.Encode
import Layouts
import List.Extra as List
import Models.Grupo exposing (lookupNombreParticipante, lookupParticipantes)
import Models.Monto as Monto
import Models.Pago as Pago
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


type Section
    = BasicPagoData
    | PagadoresSection
    | DeudoresSection
    | PagoConfirmation


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
    }


init : ULID -> Store -> ( Model, Effect Msg )
init grupoId store =
    ( { grupoId = grupoId
      , currentPagoId = Nothing
      , currentSection = BasicPagoData
      , pagoBasicoForm = Form.initial [] (validatePagoInSection BasicPagoData [])
      , deudoresForm = Form.initial [] (validatePagoInSection DeudoresSection [])
      , resumenDeudores = NotAsked
      , pagadoresForm = Form.initial [] (validatePagoInSection PagadoresSection [])
      , resumenPagadores = NotAsked
      , pagoForm = Form.initial [] (validatePago [])
      , resumenPago = NotAsked
      }
    , Effect.batch
        [ Store.ensureGrupo grupoId store

        -- , Store.ensurePago grupoId store
        ]
    )


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
            ( model
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshPagos pago.pagoId
                , Toasts.pushToast Toasts.ToastSuccess "Se creó el pago"
                , Effect.pushRoutePath <| Path.Grupos_GrupoId__Pagos_PagoId_ { grupoId = model.grupoId, pagoId = pago.pagoId }
                ]
            )

        AddedPagoResponse (Err error) ->
            ( model
            , Effect.batch
                [ Toasts.pushToast Toasts.ToastDanger "Falló la creación del pago"
                , Store.refreshPagos model.grupoId
                ]
            )

        UpdatedPagoResponse (Ok pago) ->
            ( model
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshPagos model.grupoId

                -- , Effect.pushRoutePath <| Path.Grupos_GrupoId__Pagos { grupoId = model.grupoId }
                , Toasts.pushToast Toasts.ToastSuccess "Se actualizó el pago"
                ]
            )

        UpdatedPagoResponse (Err error) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "Falló la actualización del pago"
            )

        PagoForm Form.Submit ->
            case ( Form.getOutput model.pagoForm, store |> Store.getGrupo model.grupoId ) of
                ( Just pago, Success { id } ) ->
                    case model.currentPagoId of
                        Just pagoId ->
                            ( { model
                                | pagoForm = Form.update (validatePago participantes) Form.Submit model.pagoForm
                              }
                            , Effect.sendCmd <|
                                Api.putGrupoByIdPagosByPagoId
                                    id
                                    pagoId
                                    pago
                                    UpdatedPagoResponse
                            )

                        Nothing ->
                            ( { model
                                | pagoForm = Form.update (validatePago participantes) Form.Submit model.pagoForm
                              }
                            , Effect.sendCmd <|
                                Api.postGrupoByIdPagos
                                    id
                                    pago
                                    AddedPagoResponse
                            )

                ( _, _ ) ->
                    ( { model
                        | pagoForm = Form.update (validatePago participantes) Form.Submit model.pagoForm
                      }
                    , Effect.none
                    )

        PagoForm formMsg ->
            case formMsg of
                _ ->
                    ( { model
                        | pagoForm = Form.update (validatePago participantes) formMsg model.pagoForm
                        , pagoBasicoForm = Form.update (validatePagoInSection BasicPagoData participantes) formMsg model.pagoBasicoForm
                        , pagadoresForm = Form.update (validatePagoInSection PagadoresSection participantes) formMsg model.pagadoresForm
                        , deudoresForm = Form.update (validatePagoInSection DeudoresSection participantes) formMsg model.deudoresForm
                      }
                    , Effect.none
                    )
                        |> andThenUpdateResumenesFromForms model

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

        SelectSection section ->
            ( { model | currentSection = section }
            , Effect.none
            )
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
                |> andThenUpdateResumenesFromForms model

        CheckIfPagoAndGrupoArePresent ->
            case model.currentPagoId of
                Nothing ->
                    ( model, Effect.none )
                        |> andThenUpdateResumenesFromForms model

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

                        ( Failure e, _ ) ->
                            ( model, Effect.none )

                        ( _, Failure e ) ->
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
                            in
                            ( { model
                                | pagoForm = Form.initial initialFormValues (validatePago grupo.participantes)
                                , pagadoresForm = Form.initial initialFormValues (validatePagoInSection PagadoresSection grupo.participantes)
                                , deudoresForm = Form.initial initialFormValues (validatePagoInSection DeudoresSection grupo.participantes)
                                , pagoBasicoForm = Form.initial initialFormValues (validatePagoInSection BasicPagoData grupo.participantes)
                                , resumenPago = Loading
                              }
                            , Effect.none
                            )
                                |> andThenUpdateResumenesFromForms model


{-| This is a bit of a hack, we need to wait for the `Grupo` AND possibly the `Pago` (if we are editing) before
creating the ui and we don't get notified by the store when that happens so we "poll"
until we see that the store has the values we need
-}
waitAndCheckNecessaryData : Effect Msg
waitAndCheckNecessaryData =
    Effect.sendCmd <| Task.perform (\_ -> CheckIfPagoAndGrupoArePresent) (Process.sleep 100)


distribucionToForm : Distribucion -> List ( String, FormField.Field )
distribucionToForm distribucion =
    [ Form.setString "id" distribucion.id
    ]
        ++ (case distribucion.tipo of
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


andThenUpdateResumenesFromForms : Model -> ( Model, Effect Msg ) -> ( Model, Effect Msg )
andThenUpdateResumenesFromForms originalModel ( model, oldEffects ) =
    let
        updateResumenFromForm getForm event =
            case Form.getOutput (getForm model) of
                Just pago ->
                    if Form.getOutput (getForm originalModel) == Just pago then
                        Effect.none

                    else
                        Effect.batch
                            [ Effect.sendMsg <| event Loading
                            , Effect.sendCmd <| Api.postPagosResumen pago (RemoteData.fromResult >> event)
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


getTotalFromResumen : ResumenDeudas -> Maybe Monto
getTotalFromResumen resumen =
    case resumen of
        DeudasIncomputables total _ ->
            total

        ResumenDeudas total _ ->
            total


getParticipantesFromResumen : ResumenDeudas -> Maybe (List ParticipanteId)
getParticipantesFromResumen resumen =
    case resumen of
        DeudasIncomputables _ _ ->
            Nothing

        ResumenDeudas _ deudas ->
            deudas
                |> List.map (\( p, _ ) -> p)
                |> Just


getErrorFromResumen : ResumenDeudas -> Maybe String
getErrorFromResumen resumen =
    case resumen of
        DeudasIncomputables _ (ErrorResumen error _) ->
            error

        ResumenDeudas _ _ ->
            Nothing


getDeudasFromResumen : ResumenDeudas -> Maybe (Deudas Monto)
getDeudasFromResumen resumen =
    case resumen of
        DeudasIncomputables _ _ ->
            Nothing

        ResumenDeudas _ deudas ->
            Just deudas


view : Store -> Model -> View Msg
view store model =
    case store |> Store.getGrupo model.grupoId of
        Success grupo ->
            { title = grupo.nombre
            , body =
                [ div [ class "tabs is-centered" ]
                    [ let
                        sectionName section =
                            case section of
                                BasicPagoData ->
                                    [ text "Datos básicos" ]

                                PagadoresSection ->
                                    [ text "Pagadores" ]

                                DeudoresSection ->
                                    [ text "Deudores" ]

                                PagoConfirmation ->
                                    [ text "Detalles" ]

                        mkHeader section =
                            li
                                [ if model.currentSection == section then
                                    class "is-active"

                                  else
                                    class ""
                                , onClick <| SelectSection section
                                ]
                                [ a [] <| sectionName section ]
                      in
                      ul []
                        [ mkHeader BasicPagoData
                        , mkHeader PagadoresSection
                        , mkHeader DeudoresSection
                        , mkHeader PagoConfirmation
                        ]
                    ]
                , case model.currentSection of
                    BasicPagoData ->
                        pagoForm grupo.participantes model.pagoBasicoForm

                    PagadoresSection ->
                        Html.form [ class "mb-6", onSubmit <| SubmitCurrentSection ]
                            [ distribucionForm grupo.participantes "distribucion_pagadores" model.pagadoresForm
                            , button [ class "button is-primary", disabled (Form.getOutput model.pagadoresForm == Nothing) ] [ text "Siguiente seccion" ]
                            , case model.resumenPagadores of
                                Success resumen ->
                                    div [ class "content" ]
                                        [ section []
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

                                Failure e ->
                                    text "error"
                            ]

                    DeudoresSection ->
                        Html.form [ class "mb-6", onSubmit <| SubmitCurrentSection ]
                            [ distribucionForm grupo.participantes "distribucion_deudores" model.deudoresForm
                            , p [] [ button [ class "button is-primary", disabled (Form.getOutput model.deudoresForm == Nothing) ] [ text "Siguiente seccion" ] ]
                            , case model.resumenDeudores of
                                Success resumen ->
                                    div [ class "content" ]
                                        [ section []
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
                                            , p [] [ Maybe.withDefault (text "") <| Maybe.map (viewNetosBarras grupo) <| Maybe.map (List.map (\( p, m ) -> ( p, Monto.negate m ))) <| getDeudasFromResumen <| resumen.resumenDeudores ]
                                            ]
                                        ]

                                NotAsked ->
                                    text ""

                                Loading ->
                                    text "cargando..."

                                Failure e ->
                                    text "error"
                            ]

                    PagoConfirmation ->
                        Html.form [ class "mb-6", onSubmit <| PagoForm Form.Submit ]
                            [ case model.resumenPago of
                                Success resumen ->
                                    div [ class "content" ]
                                        [ section []
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
                                            , p [] <|
                                                case Form.getOutput model.pagoForm of
                                                    Just pago ->
                                                        [ case pago.pagadores.tipo of
                                                            TipoDistribucionRepartija repartija ->
                                                                if repartija.id /= emptyUlid then
                                                                    a
                                                                        [ Path.href <| Path.Grupos_GrupoId__Repartijas_RepartijaId_ { grupoId = model.grupoId, repartijaId = repartija.id }
                                                                        , target "_blank"
                                                                        ]
                                                                        [ text "repartija pagadores"
                                                                        ]

                                                                else
                                                                    text ""

                                                            _ ->
                                                                text ""
                                                        , case pago.deudores.tipo of
                                                            TipoDistribucionRepartija repartija ->
                                                                if repartija.id /= emptyUlid then
                                                                    a
                                                                        [ Path.href <| Path.Grupos_GrupoId__Repartijas_RepartijaId_ { grupoId = model.grupoId, repartijaId = repartija.id }
                                                                        , target "_blank"
                                                                        ]
                                                                        [ text "repartija deudores"
                                                                        ]

                                                                else
                                                                    text ""

                                                            _ ->
                                                                text ""
                                                        ]

                                                    Nothing ->
                                                        []
                                            ]
                                        ]

                                NotAsked ->
                                    text "notasked"

                                Loading ->
                                    text "loading"

                                Failure e ->
                                    text "e"
                            , div [ class "container" ] <|
                                [ button
                                    [ class "button is-primary"
                                    , type_ "submit"
                                    ]
                                    [ case model.currentPagoId of
                                        Nothing ->
                                            text "Crear pago"

                                        Just _ ->
                                            text "Actualizar pago"
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
    Html.form [ class "mb-6", onSubmit <| SubmitCurrentSection ]
        [ div [ class "field mb-5" ]
            [ div [ class "field mb-5" ]
                [ label [ class "label" ]
                    [ text "Nombre" ]
                , div [ class "control" ]
                    [ Html.map PagoForm <|
                        FormInput.textInput nombreField
                            [ class "input"
                            , type_ "text"
                            , placeholder "Pago de deudas"
                            , classList [ ( "is-danger", hasErrorField nombreField ) ]
                            ]
                    , errorForField nombreField
                    ]
                ]
            , label [ class "label" ]
                [ text "Monto" ]
            , div [ class "control" ]
                [ Html.map PagoForm <|
                    FormInput.textInput montoField
                        [ class "input"
                        , type_ "text"
                        , placeholder "2000"
                        , classList [ ( "is-danger", hasErrorField montoField ) ]
                        ]
                , errorForField montoField
                ]
            ]
        , button [ class "button is-primary", disabled (Form.getOutput form == Nothing) ] [ text "Siguiente seccion" ]
        ]


distribucionForm : List Participante -> String -> Form CustomFormError Pago -> Html Msg
distribucionForm participantes prefix form =
    let
        tipoField =
            Form.getFieldAsString (prefix ++ ".tipo") form
    in
    div [] <|
        [ div [ class "field mb-5" ]
            [ label [ class "label" ]
                [ text "Tipo" ]
            , div [ class "control" ]
                [ span [ class "select" ]
                    [ Html.map PagoForm <|
                        FormInput.selectInput
                            [ ( "", "Seleccionar distribución" )
                            , ( "monto_equitativo", "Equitativo" )
                            , ( "montos_especificos", "Especifico" )
                            , ( "repartija", "Repartija" )
                            ]
                            tipoField
                            []
                    ]
                ]
            ]
        , text "Este es un coso re simpatico"
        ]
            ++ (case tipoField.value of
                    Just "repartija" ->
                        [ repartijaForm prefix form ]

                    Just "montos_especificos" ->
                        let
                            montosIndexes =
                                Form.getListIndexes (prefix ++ ".montos") form
                        in
                        [ div [ class "container mb-2" ] <|
                            [ label [ class "label" ] [ text "Pagadores" ]
                            , div [ class "container mb-2" ] <|
                                (montosIndexes
                                    |> List.map
                                        (\i ->
                                            let
                                                montoField =
                                                    Form.getFieldAsString (prefix ++ ".montos." ++ String.fromInt i ++ ".monto") form

                                                participanteField =
                                                    Form.getFieldAsString (prefix ++ ".montos." ++ String.fromInt i ++ ".participante") form
                                            in
                                            div [ class "field has-addons" ]
                                                [ p [ class "control" ]
                                                    [ Html.map PagoForm <|
                                                        FormInput.textInput
                                                            montoField
                                                            [ class "input"
                                                            , type_ "text"
                                                            , placeholder "200.0"
                                                            , classList [ ( "is-danger", hasErrorField montoField ) ]
                                                            ]
                                                    ]
                                                , p [ class "control" ]
                                                    [ span [ class "select" ]
                                                        [ Html.map PagoForm <|
                                                            FormInput.selectInput
                                                                (( "", "" ) :: List.map (\p -> ( p.participanteId, p.participanteNombre )) participantes)
                                                                participanteField
                                                                []
                                                        ]
                                                    ]
                                                , button [ class "button is-outlined is-danger", type_ "button", onClick <| PagoForm <| Form.RemoveItem (prefix ++ ".montos") i ]
                                                    [ Icons.toHtml [] Icons.trash2
                                                    ]
                                                ]
                                        )
                                )
                                    ++ [ div [ class "container" ] <|
                                            [ button
                                                [ class "button is-outlined is-primary is-flex pr-5"
                                                , style "gap" ".5rem"
                                                , onClick <| PagoForm <| Form.Append (prefix ++ ".montos")
                                                , type_ "button"
                                                ]
                                                [ Icons.toHtml [] Icons.plus
                                                , span []
                                                    [ text "Agregar" ]
                                                ]
                                            ]
                                       ]
                            ]
                        ]

                    Just "monto_equitativo" ->
                        [ div [ class "container mb-2" ] <|
                            [ label [ class "label" ] [ text "Pagadores" ]
                            , text ""
                            , div [ class "checkboxes" ]
                                (participantes
                                    |> List.map
                                        (\p ->
                                            let
                                                participanteField =
                                                    Form.getFieldAsBool (prefix ++ ".participantes." ++ p.participanteId) form
                                            in
                                            label [ class "checkbox" ]
                                                [ text <| p.participanteNombre
                                                , Html.map PagoForm <|
                                                    FormInput.checkboxInput
                                                        participanteField
                                                        []
                                                ]
                                        )
                                )
                            ]
                        ]

                    _ ->
                        []
               )


repartijaForm : String -> Form CustomFormError Pago -> Html Msg
repartijaForm prefix form =
    let
        montoField =
            Form.getFieldAsString (prefix ++ ".extra") form

        itemsIndexes =
            Form.getListIndexes (prefix ++ ".items") form
    in
    div []
        [ div [ class "container" ]
            [ table [ class "table is-fullwidth" ]
                [ thead []
                    [ tr []
                        [ th [ class "pl-0" ] [ text "Item" ]
                        , th [] [ text "Monto total" ]
                        , th [] [ text "Cantidad" ]
                        , th [] []
                        ]
                    ]
                , tbody [] <|
                    List.map
                        (\i -> repartijaItemForm i prefix form)
                        itemsIndexes
                ]
            ]
        , div [ class "container mb-5" ] <|
            [ button
                [ class "button is-outlined is-primary is-align-items-center is-flex pr-5"
                , style "gap" ".5rem"
                , onClick <| PagoForm <| Form.Append <| prefix ++ ".items"
                , type_ "button"
                ]
                [ Icons.toHtml [] Icons.plus
                , span []
                    [ text "Agregar item" ]
                ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ]
                [ text "Propina" ]
            , div [ class "control" ]
                [ Html.map PagoForm <|
                    FormInput.textInput montoField
                        [ class "input"
                        , type_ "text"
                        , placeholder "1000"
                        , classList [ ( "is-danger", hasErrorField montoField ) ]
                        ]
                , errorForField montoField
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
    tr []
        [ td [ class "control pl-0" ]
            [ Html.map PagoForm <|
                FormInput.textInput nombreField
                    [ class "input"
                    , type_ "text"
                    , placeholder "Birrita"
                    , classList [ ( "is-danger", hasErrorField nombreField ) ]
                    ]
            ]
        , td [ class "control" ]
            [ Html.map PagoForm <|
                FormInput.textInput montoField
                    [ class "input"
                    , type_ "text"
                    , placeholder "20000"
                    , classList [ ( "is-danger", hasErrorField montoField ) ]
                    ]
            ]
        , td [ class "control" ]
            [ Html.map PagoForm <|
                FormInput.textInput cantidadField
                    [ class "input"
                    , type_ "text"
                    , placeholder "4"
                    , classList [ ( "is-danger", hasErrorField cantidadField ) ]
                    ]
            ]
        , td [ class "control" ]
            [ button [ class "button is-danger is-outlined", type_ "button", onClick <| PagoForm <| Form.RemoveItem (prefix ++ ".items") i ]
                [ Icons.toHtml [] Icons.trash2
                ]
            ]
        ]
