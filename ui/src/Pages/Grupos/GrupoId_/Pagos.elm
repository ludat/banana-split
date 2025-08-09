module Pages.Grupos.GrupoId_.Pagos exposing (Model, Msg, page)

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
import Generated.Api as Api
    exposing
        ( Distribucion
        , Grupo
        , Monto
        , Netos
        , Pago
        , Parte(..)
        , Participante
        , ParticipanteId
        , ShallowGrupo
        , ULID
        )
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit)
import Http
import Layouts
import Models.Grupo exposing (lookupNombreParticipante)
import Models.Monto as Monto
import Models.Pago as Pago
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Numeric.Decimal as Decimal
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Utils.Form exposing (CustomFormError(..))
import Utils.Toasts as Toasts exposing (pushToast)
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
    | ChangePagoPopoverState PagoPopoverState
    | AddedPagoResponse (Result Http.Error Pago)
    | UpdatedPagoResponse (Result Http.Error Pago)
    | DeletePago ULID
    | IniciarCrearPago
    | ShowPagoDetails ULID
    | DeletePagoResponse (Result Http.Error ULID)
    | NetosUpdated (WebData Netos)


type PagoPopoverState
    = EditingPago ULID
    | CreatingNewPago
    | Closed


type alias Model =
    { grupoId : String
    , pagoPopoverState : PagoPopoverState
    , pagoForm : Form CustomFormError Pago
    , editingPagoNeto : WebData Netos
    }


initialPagoForm : Maybe ULID -> List Participante -> Form CustomFormError Pago
initialPagoForm maybeCreador participantes =
    Form.initial
        ((case maybeCreador of
            Just creador ->
                [ ( "pagadores", FormField.list [ participanteIdToField creador ] ) ]

            Nothing ->
                []
         )
            ++ [ ( "deudores"
                 , participantes
                    |> List.map
                        (\participante ->
                            participanteIdToField <| participante.participanteId
                        )
                    |> FormField.list
                 )
               ]
        )
        validatePago


participanteIdToField : String -> FormField.Field
participanteIdToField participanteId =
    FormField.group
        [ ( "cuota", FormField.string "1" )
        , ( "monto", FormField.string "100.0" )
        , ( "participante", FormField.string participanteId )
        , ( "tipo", FormField.string "ponderado" )
        ]


init : ULID -> Store -> ( Model, Effect Msg )
init grupoId store =
    ( { grupoId = grupoId
      , pagoPopoverState = Closed
      , pagoForm = Form.initial [] validatePago
      , editingPagoNeto = NotAsked
      }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Store.ensurePagos grupoId store
        ]
    )


validatePago : Validation CustomFormError Pago
validatePago =
    V.succeed Pago
        |> V.andMap (V.succeed emptyUlid)
        |> V.andMap (V.field "monto" Monto.validateMonto)
        |> V.andMap (V.succeed False)
        |> V.andMap (V.field "nombre" (V.string |> V.andThen nonEmpty))
        |> V.andMap (V.succeed { id = emptyUlid, tipo = Api.TipoDistribucionMontoEquitativo <| { id = emptyUlid, participantes = [] } })
        |> V.andMap (V.succeed { id = emptyUlid, tipo = Api.TipoDistribucionMontoEquitativo <| { id = emptyUlid, participantes = [] } })



-- UPDATE


update : Store -> Maybe ULID -> Msg -> Model -> ( Model, Effect Msg )
update store userId msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        AddedPagoResponse (Ok pago) ->
            ( { model | pagoPopoverState = Closed }
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshPagos model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Se creó el pago"
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
            ( { model | pagoPopoverState = Closed }
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshPagos model.grupoId
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
                    case model.pagoPopoverState of
                        Closed ->
                            ( model, Effect.none )

                        EditingPago pagoId ->
                            ( { model
                                | pagoForm = Form.update validatePago Form.Submit model.pagoForm
                              }
                            , Effect.sendCmd <|
                                Api.putGrupoByIdPagosByPagoId
                                    id
                                    pagoId
                                    pago
                                    UpdatedPagoResponse
                            )

                        CreatingNewPago ->
                            ( { model
                                | pagoForm = Form.update validatePago Form.Submit model.pagoForm
                              }
                            , Effect.sendCmd <|
                                Api.postGrupoByIdPagos
                                    id
                                    pago
                                    AddedPagoResponse
                            )

                ( _, _ ) ->
                    ( { model
                        | pagoForm = Form.update validatePago Form.Submit model.pagoForm
                      }
                    , Effect.none
                    )

        PagoForm formMsg ->
            case formMsg of
                Form.Append prefix ->
                    let
                        nextForm =
                            model.pagoForm
                                |> Form.update validatePago formMsg

                        maximumIndex =
                            List.maximum <| Form.getListIndexes prefix nextForm

                        nextNextForm =
                            case maximumIndex of
                                Just n ->
                                    nextForm
                                        |> Form.update
                                            validatePago
                                            (Form.Input
                                                (prefix ++ "." ++ String.fromInt n ++ "." ++ "tipo")
                                                Form.Text
                                                (FormField.String "ponderado")
                                            )
                                        |> Form.update
                                            validatePago
                                            (Form.Input
                                                (prefix ++ "." ++ String.fromInt n ++ "." ++ "cuota")
                                                Form.Text
                                                (FormField.String "1")
                                            )
                                        |> Form.update
                                            validatePago
                                            (Form.Input
                                                (prefix ++ "." ++ String.fromInt n ++ "." ++ "monto")
                                                Form.Text
                                                (FormField.String "100.0")
                                            )

                                Nothing ->
                                    nextForm

                        nextNextNextForm =
                            case ( maximumIndex, store |> Store.getGrupo model.grupoId ) of
                                ( Just n, Success { participantes } ) ->
                                    case List.head participantes of
                                        Nothing ->
                                            nextNextForm

                                        Just participante ->
                                            nextNextForm
                                                |> Form.update
                                                    validatePago
                                                    (Form.Input
                                                        (prefix ++ "." ++ String.fromInt n ++ "." ++ "participante")
                                                        Form.Text
                                                        (FormField.String participante.participanteId)
                                                    )

                                ( _, _ ) ->
                                    nextNextForm
                    in
                    ( { model
                        | pagoForm = nextNextNextForm
                      }
                    , Effect.none
                    )
                        |> andThenUpdateNetosFromForm model.pagoForm

                _ ->
                    ( { model | pagoForm = Form.update validatePago formMsg model.pagoForm }
                    , Effect.none
                    )
                        |> andThenUpdateNetosFromForm model.pagoForm

        DeletePago pagoId ->
            case store |> Store.getGrupo model.grupoId of
                NotAsked ->
                    ( model, Effect.none )

                Loading ->
                    ( model, Effect.none )

                Failure _ ->
                    ( model, Effect.none )

                Success grupo ->
                    ( model
                    , Effect.batch
                        [ Effect.sendCmd <| Api.deleteGrupoByIdPagosByPagoId grupo.id pagoId DeletePagoResponse
                        ]
                    )

        DeletePagoResponse result ->
            case result of
                Ok pagoBorradoId ->
                    ( model
                    , Effect.batch
                        [ Store.refreshGrupo model.grupoId
                        , Store.refreshResumen model.grupoId
                        , pushToast ToastSuccess "Pago borrado"
                        ]
                    )

                Err e ->
                    ( model, pushToast ToastDanger "Falle al borrar el pago" )

        ChangePagoPopoverState pagoPopoverState ->
            ( { model
                | pagoPopoverState = pagoPopoverState
                , pagoForm =
                    case pagoPopoverState of
                        Closed ->
                            model.pagoForm

                        EditingPago pago ->
                            let
                                parteToForm parte =
                                    case parte of
                                        MontoFijo montoRaw participanteId ->
                                            let
                                                monto =
                                                    Monto.toDecimal montoRaw
                                            in
                                            FormField.group
                                                [ Form.setString "tipo" "fijo"
                                                , Form.setString "monto" (Decimal.toString monto)
                                                , Form.setString "participante" participanteId
                                                ]

                                        Ponderado int participanteId ->
                                            FormField.group
                                                [ Form.setString "tipo" "ponderado"
                                                , Form.setString "cuota" (String.fromInt int)
                                                , Form.setString "participante" participanteId
                                                ]

                                montoPago =
                                    Monto.zero
                            in
                            Form.initial [] validatePago

                        CreatingNewPago ->
                            case Store.getGrupo model.grupoId store |> RemoteData.toMaybe of
                                Just grupo ->
                                    initialPagoForm userId grupo.participantes

                                Nothing ->
                                    Form.initial [] validatePago
              }
            , Effect.none
            )
                |> andThenUpdateNetosFromForm model.pagoForm

        NetosUpdated webData ->
            ( { model | editingPagoNeto = webData }
            , Effect.none
            )

        IniciarCrearPago ->
            ( model
            , Effect.batch
                [ Effect.pushRoutePath <| Path.Grupos_GrupoId__Pagos_New { grupoId = model.grupoId }
                ]
            )

        ShowPagoDetails ulid ->
            ( model
            , Effect.batch
                [ Effect.pushRoutePath <| Path.Grupos_GrupoId__Pagos_PagoId_ { grupoId = model.grupoId, pagoId = ulid }
                ]
            )


andThenUpdateNetosFromForm : Form CustomFormError Pago -> ( Model, Effect Msg ) -> ( Model, Effect Msg )
andThenUpdateNetosFromForm pagoForm ( model, oldEffects ) =
    ( model
    , case ( Form.getOutput pagoForm, Form.getOutput model.pagoForm ) of
        ( Just oldPago, Just pago ) ->
            if oldPago /= pago then
                Effect.batch
                    [ oldEffects
                    , Effect.sendMsg <| NetosUpdated Loading
                    ]

            else
                Effect.none

        ( Nothing, Just pago ) ->
            Effect.batch
                [ oldEffects
                , Effect.sendMsg <| NetosUpdated Loading
                ]

        ( _, Nothing ) ->
            Effect.batch
                [ oldEffects
                , Effect.sendMsg <| NetosUpdated NotAsked
                ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Store -> Model -> View Msg
view store model =
    case ( store |> Store.getGrupo model.grupoId, store |> Store.getPagos model.grupoId ) of
        ( Success grupo, Success pagos ) ->
            { title = grupo.nombre
            , body =
                [ div [ class "container columns is-mobile is-justify-content-end px-4 pt-2 pb-1 m-0" ]
                    [ button [ class "button mx-3", onClick IniciarCrearPago ] [ text "Agregar pago" ]
                    ]
                , div
                    [ class "container columns is-flex-wrap-wrap px-4 pb-4 pt-1" ]
                    (pagos
                        |> List.map
                            (\pago ->
                                div [ class "column is-one-third" ]
                                    [ div [ class "card" ]
                                        [ header [ class "card-header" ]
                                            [ p [ class "card-header-title py-2 px-4" ]
                                                [ text pago.nombre ]
                                            , if pago.isValid then
                                                text ""

                                              else
                                                button
                                                    [ class "card-header-icon"
                                                    , attribute "aria-label" "more options"
                                                    ]
                                                    [ span
                                                        [ class "icon has-tooltip-multiline has-tooltip-danger has-text-danger"
                                                        , attribute "data-tooltip" "Este pago es invalido asi que no se cuenta para las deudas."
                                                        ]
                                                        [ Icons.toHtml [] Icons.alertCircle
                                                        ]
                                                    ]
                                            ]
                                        , div [ class "card-content" ]
                                            [ p [ class "title is-3 m-0" ]
                                                [ text "$ "
                                                , text <| Monto.toString pago.monto
                                                ]
                                            , p [ style "display" "none" ]
                                                [ let
                                                    pagador2Text pagador =
                                                        pagador
                                                            |> lookupNombreParticipante grupo
                                                  in
                                                  case Pago.getPagadores pago of
                                                    [] ->
                                                        text <| "pagado por nadie!"

                                                    [ pagador ] ->
                                                        text <| "pagado por " ++ pagador2Text pagador

                                                    [ pagador1, pagador2 ] ->
                                                        text <| ("pagado  por " ++ pagador2Text pagador1 ++ " y " ++ pagador2Text pagador2)

                                                    [ pagador1, pagador2, pagador3 ] ->
                                                        text <| ("pagado por " ++ pagador2Text pagador1 ++ ", " ++ pagador2Text pagador2 ++ " y " ++ pagador2Text pagador3)

                                                    pagador1 :: pagador2 :: rest ->
                                                        text <| ("pagado por " ++ pagador2Text pagador1 ++ ", " ++ pagador2Text pagador2 ++ " y " ++ String.fromInt (List.length rest) ++ " personas mas")
                                                ]
                                            ]
                                        , footer [ class "card-footer" ]
                                            [ button [ class "card-footer-item", onClick <| ShowPagoDetails pago.pagoId ]
                                                [ Icons.toHtml [] Icons.edit
                                                ]
                                            , button [ class "card-footer-item", onClick <| DeletePago pago.pagoId ]
                                                [ Icons.toHtml [] Icons.trash2
                                                ]
                                            ]
                                        ]
                                    ]
                            )
                    )
                , pagosModal grupo model
                ]
            }

        ( _, _ ) ->
            { title = "Impossible"
            , body = []
            }


extractPagadorFromParte : Parte -> ParticipanteId
extractPagadorFromParte parte =
    case parte of
        MontoFijo _ participanteId ->
            participanteId

        Ponderado _ participanteId ->
            participanteId


pagosModal : ShallowGrupo -> Model -> Html Msg
pagosModal grupo model =
    div
        (class "modal"
            :: (case model.pagoPopoverState of
                    Closed ->
                        []

                    EditingPago _ ->
                        [ class "is-active" ]

                    CreatingNewPago ->
                        [ class "is-active" ]
               )
        )
        [ div
            [ class "modal-background"
            , onClick <| ChangePagoPopoverState Closed
            ]
            []
        , div
            [ class "modal-card"
            ]
            [ header
                [ class "modal-card-head"
                ]
                [ p
                    [ class "modal-card-title"
                    ]
                    [ text "Agregar pago" ]
                , button
                    [ class "delete"
                    , attribute "aria-label" "close"
                    , onClick <| ChangePagoPopoverState Closed
                    ]
                    []
                ]
            , section
                [ class "modal-card-body"
                ]
                [ pagosForm grupo.participantes model.pagoForm
                , case model.editingPagoNeto of
                    Success netos ->
                        viewNetosBarras grupo netos

                    NotAsked ->
                        text ""

                    Loading ->
                        text "cargando netos"

                    Failure _ ->
                        text "falle consiguiendo los netos"
                ]
            , footer
                [ class "modal-card-foot"
                ]
                [ div
                    [ class "buttons"
                    ]
                    [ button
                        [ class "button is-success"
                        , onClick <| PagoForm <| Form.Submit
                        ]
                        (case model.pagoPopoverState of
                            Closed ->
                                [ text "Imposibru" ]

                            EditingPago _ ->
                                [ text "Editar pago" ]

                            CreatingNewPago ->
                                [ text "Crear pago" ]
                        )
                    , button
                        [ class "button"
                        , onClick <| ChangePagoPopoverState Closed
                        ]
                        [ text "Cancelar" ]
                    ]
                ]
            ]
        ]


pagosForm : List Participante -> Form CustomFormError Pago -> Html Msg
pagosForm participantes form =
    let
        errorFor field =
            case field.liveError of
                Just FormError.Empty ->
                    p [ class "help is-danger" ] [ text "No puede ser vacio" ]

                Just FormError.InvalidString ->
                    p [ class "help is-danger" ] [ text "String invalido" ]

                Just FormError.InvalidEmail ->
                    p [ class "help is-danger" ] [ text "Email invalido" ]

                Just FormError.InvalidFormat ->
                    p [ class "help is-danger" ] [ text "Formato invalido" ]

                Just FormError.InvalidInt ->
                    p [ class "help is-danger" ] [ text "Entero invalido" ]

                Just FormError.InvalidFloat ->
                    p [ class "help is-danger" ] [ text "Numero con coma invalido" ]

                Just FormError.InvalidBool ->
                    p [ class "help is-danger" ] [ text "Booleano invalido" ]

                Just (FormError.SmallerIntThan _) ->
                    p [ class "help is-danger" ] [ text "Mas chico que" ]

                Just (FormError.GreaterIntThan _) ->
                    p [ class "help is-danger" ] [ text "Mas grande que" ]

                Just (FormError.SmallerFloatThan _) ->
                    p [ class "help is-danger" ] [ text "Mas chico que" ]

                Just (FormError.GreaterFloatThan _) ->
                    p [ class "help is-danger" ] [ text "Mas grande que" ]

                Just (FormError.ShorterStringThan _) ->
                    p [ class "help is-danger" ] [ text "Mas corto que" ]

                Just (FormError.LongerStringThan _) ->
                    p [ class "help is-danger" ] [ text "Longer than" ]

                Just FormError.NotIncludedIn ->
                    p [ class "help is-danger" ] [ text "Not included in" ]

                Just (FormError.CustomError _) ->
                    p [ class "help is-danger" ] [ text "Jajan't" ]

                Nothing ->
                    text ""

        hasError field =
            case field.liveError of
                Just _ ->
                    True

                Nothing ->
                    False

        nombreField =
            Form.getFieldAsString "nombre" form

        montoField =
            Form.getFieldAsString "monto" form
    in
    Html.form [ class "mb-6", onSubmit <| PagoForm Form.Submit ]
        [ div [ class "field mb-5" ]
            [ label [ class "label" ]
                [ text "Nombre" ]
            , div [ class "control" ]
                [ Html.map PagoForm <|
                    FormInput.textInput nombreField
                        [ class "input"
                        , type_ "text"
                        , placeholder "Pago de deudas"
                        , classList [ ( "is-danger", hasError nombreField ) ]
                        ]
                , errorFor nombreField
                ]
            ]
        , div [ class "field mb-5" ]
            [ label [ class "label" ]
                [ text "Monto" ]
            , div [ class "control" ]
                [ Html.map PagoForm <|
                    FormInput.textInput montoField
                        [ class "input"
                        , type_ "text"
                        , placeholder "10000"
                        , classList [ ( "is-danger", hasError montoField ) ]
                        ]
                , errorFor montoField
                ]
            ]
        , div [ class "container mb-2" ]
            [ label [ class "label" ] [ text "Pagadores" ]
            , div [] <| List.map (\i -> parteForm participantes "pagadores" i form) (Form.getListIndexes "pagadores" form)
            ]
        , div [ class "container mb-5" ] <|
            [ button
                [ class "button is-outlined is-primary is-flex pr-5"
                , style "gap" ".5rem"
                , onClick <| PagoForm <| Form.Append "pagadores"
                , type_ "button"
                ]
                [ Icons.toHtml [] Icons.plus
                , span []
                    [ text "Agregar pagador" ]
                ]
            ]
        , div [ class "container mb-2" ]
            [ label [ class "label" ] [ text "Deudores" ]
            , div [] <| List.map (\i -> parteForm participantes "deudores" i form) (Form.getListIndexes "deudores" form)
            ]
        , div [ class "container" ] <|
            [ button
                [ class "button is-outlined is-primary is-flex pr-5"
                , style "gap" ".5rem"
                , onClick <| PagoForm <| Form.Append "deudores"
                , type_ "button"
                ]
                [ Icons.toHtml [] Icons.plus
                , span []
                    [ text "Agregar deudor" ]
                ]
            ]
        ]


parteForm : List Participante -> String -> Int -> Form CustomFormError Pago -> Html Msg
parteForm participantes prefix i form =
    let
        errorFor field =
            case field.liveError of
                Just FormError.Empty ->
                    p [ class "help is-danger" ] [ text "No puede ser vacio" ]

                Just _ ->
                    p [ class "help is-danger" ] [ text "Algo esta maloso" ]

                Nothing ->
                    text ""

        hasError field =
            case field.liveError of
                Just _ ->
                    True

                Nothing ->
                    False

        tipoField =
            Form.getFieldAsString (prefix ++ "." ++ String.fromInt i ++ ".tipo") form

        montoField =
            Form.getFieldAsString (prefix ++ "." ++ String.fromInt i ++ ".monto") form

        cuotaField =
            Form.getFieldAsString (prefix ++ "." ++ String.fromInt i ++ ".cuota") form

        participanteField =
            Form.getFieldAsString (prefix ++ "." ++ String.fromInt i ++ ".participante") form
    in
    div [ class "field has-addons" ]
        [ p [ class "control" ]
            [ span [ class "select" ]
                [ Html.map PagoForm <|
                    FormInput.selectInput
                        [ ( "ponderado", "Cuota" )
                        , ( "fijo", "$" )
                        ]
                        tipoField
                        []
                ]
            ]
        , div [ class "control" ]
            [ case tipoField.value of
                Just "ponderado" ->
                    Html.map PagoForm <|
                        FormInput.textInput cuotaField
                            [ class "input"
                            , type_ "text"
                            , placeholder "CUOTA"
                            , classList [ ( "is-danger", hasError cuotaField ) ]
                            ]

                Just "fijo" ->
                    Html.map PagoForm <|
                        FormInput.textInput montoField
                            [ class "input"
                            , type_ "text"
                            , placeholder "FIJO"
                            , classList [ ( "is-danger", hasError montoField ) ]
                            ]

                _ ->
                    Html.map PagoForm <|
                        FormInput.textInput montoField
                            [ class "input"
                            , type_ "text"
                            , placeholder "FIJO"
                            , classList [ ( "is-danger", hasError montoField ) ]
                            ]
            ]
        , p [ class "control" ]
            [ span [ class "select" ]
                [ Html.map PagoForm <|
                    FormInput.selectInput
                        (List.map (\p -> ( p.participanteId, p.participanteNombre )) participantes)
                        participanteField
                        []
                ]
            ]
        , p [ class "control" ]
            [ button [ class "button is-outlined is-danger", type_ "button", onClick <| PagoForm <| Form.RemoveItem prefix i ]
                [ Icons.toHtml [] Icons.trash2
                ]
            ]
        ]
