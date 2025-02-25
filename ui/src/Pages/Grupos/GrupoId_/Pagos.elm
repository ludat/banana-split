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
import Generated.Api as Api exposing (Grupo, Monto, Netos, Pago, Parte(..), Participante, ParticipanteId, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit)
import Http
import Layouts
import Models.Grupo exposing (lookupNombreParticipante, lookupParticipante)
import Models.Monto exposing (monto2Decimal, validateMonto)
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Numeric.Decimal as Decimal
import Numeric.Decimal.Rounding as Decimal
import Numeric.Nat as Nat
import Numeric.Rational as Rational
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import Utils.Form exposing (CustomFormError(..))
import Utils.Toasts exposing (pushToast)
import Utils.Toasts.Types exposing (ToastLevel(..))
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
                    { navBarContent = Just <| NavBar.navBar route.params.grupoId shared.store route.path
                    }
            )



-- INIT


type Msg
    = NoOp
    | PagoForm Form.Msg
    | ChangePagoPopoverState PagoPopoverState
    | AddedPago Pago
    | UpdatedPago Pago
    | DeletePago ULID
    | DeletePagoResponse (Result Http.Error ULID)
    | NetosUpdated (WebData Netos)


type PagoPopoverState
    = EditingPago Pago
    | CreatingNewPago
    | Closed


type alias Model =
    { grupoId : String
    , pagoPopoverState : PagoPopoverState
    , pagoForm : Form CustomFormError Pago
    , editingPagoNeto : WebData Netos
    }


init : ULID -> Store -> ( Model, Effect Msg )
init grupoId store =
    ( { grupoId = grupoId
      , pagoPopoverState = Closed
      , pagoForm = Form.initial [] validatePago
      , editingPagoNeto = NotAsked
      }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        ]
    )


validatePago : Validation CustomFormError Pago
validatePago =
    V.succeed Pago
        |> V.andMap (V.succeed "00000000000000000000000000")
        |> V.andMap (V.field "monto" validateMonto)
        |> V.andMap (V.field "nombre" (V.string |> V.andThen nonEmpty))
        |> V.andMap (V.field "deudores" (V.list validateParte |> V.andThen nonEmptyList))
        |> V.andMap (V.field "pagadores" (V.list validateParte |> V.andThen nonEmptyList))


nonEmptyList : List a -> Validation CustomFormError (List a)
nonEmptyList l =
    if l == [] then
        V.fail <| V.customError <| StringError "Tiene que haber algun pagador/deudor"

    else
        V.succeed l


validateParte : Validation CustomFormError Parte
validateParte =
    V.field "tipo" V.string
        |> V.andThen
            (\t ->
                case t of
                    "ponderado" ->
                        V.succeed Ponderado
                            |> V.andMap (V.field "cuota" V.int)
                            |> V.andMap (V.field "participante" V.string)

                    "fijo" ->
                        V.succeed MontoFijo
                            |> V.andMap (V.field "monto" validateMonto)
                            |> V.andMap (V.field "participante" V.string)

                    _ ->
                        V.fail <| FormError.value FormError.Empty
            )



-- UPDATE


update : Store -> Msg -> Model -> ( Model, Effect Msg )
update store msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        AddedPago pago ->
            ( model
            , Effect.batch
                [ Store.refreshNetos model.grupoId store
                , Store.refreshGrupo model.grupoId store
                ]
            )

        UpdatedPago pago ->
            ( model
            , Effect.batch
                [ Store.refreshNetos model.grupoId store
                , Store.refreshGrupo model.grupoId store
                ]
            )

        PagoForm Form.Submit ->
            case ( Form.getOutput model.pagoForm, store |> Store.getGrupo model.grupoId ) of
                ( Just pago, Success { grupoId } ) ->
                    case model.pagoPopoverState of
                        Closed ->
                            ( model, Effect.none )

                        EditingPago oldPago ->
                            ( { model
                                | pagoForm = Form.update validatePago Form.Submit model.pagoForm
                              }
                            , Effect.sendCmd <|
                                Api.putGrupoByIdPagosByPagoId
                                    grupoId
                                    oldPago.pagoId
                                    pago
                                    (\r ->
                                        case r of
                                            Ok newPago ->
                                                UpdatedPago newPago

                                            Err error ->
                                                NoOp
                                    )
                            )

                        CreatingNewPago ->
                            ( { model
                                | pagoForm = Form.update validatePago Form.Submit model.pagoForm
                              }
                            , Effect.sendCmd <|
                                Api.postGrupoByIdPagos
                                    grupoId
                                    pago
                                    (\r ->
                                        case r of
                                            Ok newPago ->
                                                AddedPago newPago

                                            Err error ->
                                                NoOp
                                    )
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
                        [ Effect.sendCmd <| Api.deleteGrupoByIdPagosByPagoId grupo.grupoId pagoId DeletePagoResponse
                        ]
                    )

        DeletePagoResponse result ->
            case result of
                Ok pagoBorradoId ->
                    ( model
                    , Effect.batch
                        [ Store.refreshGrupo model.grupoId store
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
                                                    monto2Decimal montoRaw
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
                                    monto2Decimal pago.monto
                            in
                            Form.initial
                                (List.concat
                                    [ [ Form.setString "monto" (Decimal.toString montoPago)
                                      , Form.setString "nombre" pago.nombre
                                      ]
                                    , [ Form.setList "deudores"
                                            (List.map
                                                parteToForm
                                                pago.deudores
                                            )
                                      ]
                                    , [ Form.setList "pagadores"
                                            (List.map
                                                parteToForm
                                                pago.pagadores
                                            )
                                      ]
                                    ]
                                )
                                validatePago

                        CreatingNewPago ->
                            Form.initial [] validatePago
              }
            , Effect.none
            )
                |> andThenUpdateNetosFromForm model.pagoForm

        NetosUpdated webData ->
            ( { model | editingPagoNeto = webData }
            , Effect.none
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
                    , Effect.sendCmd <| Api.postPagos pago (RemoteData.fromResult >> NetosUpdated)
                    ]

            else
                Effect.none

        ( Nothing, Just pago ) ->
            Effect.batch
                [ oldEffects
                , Effect.sendMsg <| NetosUpdated Loading
                , Effect.sendCmd <| Api.postPagos pago (RemoteData.fromResult >> NetosUpdated)
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
    case store |> Store.getGrupo model.grupoId of
        NotAsked ->
            { title = "Impossible"
            , body = []
            }

        Loading ->
            { title = "Cargando"
            , body =
                [ div [ class "container" ]
                    [ section [ class "section" ]
                        [ text "Cargando..."
                        ]
                    ]
                ]
            }

        Failure e ->
            { title = "Fallo"
            , body = []
            }

        Success grupo ->
            { title = grupo.grupoNombre
            , body =
                [ div [ class "container" ]
                    (grupo.pagos
                        |> List.map
                            (\pago ->
                                div [ class "card" ]
                                    [ p []
                                        [ text pago.nombre
                                        , text " ($"
                                        , text (Decimal.toString <| monto2Decimal pago.monto)
                                        , text ")"
                                        , button [ class "button", onClick <| ChangePagoPopoverState <| EditingPago pago ]
                                            [ Icons.toHtml [] Icons.edit
                                            ]
                                        , button [ class "delete", onClick <| DeletePago pago.pagoId ] []
                                        ]
                                    , p []
                                        [ let
                                            pagador2Text pagador =
                                                pagador
                                                    |> extractPagadorFromParte
                                                    |> lookupNombreParticipante grupo
                                          in
                                          case pago.pagadores of
                                            [] ->
                                                text <| "pagador por nadie!"

                                            [ pagador ] ->
                                                text <| "pagador por " ++ pagador2Text pagador

                                            [ pagador1, pagador2 ] ->
                                                text <| ("pagador por " ++ pagador2Text pagador1 ++ " y " ++ pagador2Text pagador2)

                                            [ pagador1, pagador2, pagador3 ] ->
                                                text <| ("pagador por " ++ pagador2Text pagador1 ++ ", " ++ pagador2Text pagador2 ++ " y " ++ pagador2Text pagador3)

                                            pagador1 :: pagador2 :: rest ->
                                                text <| ("pagador por " ++ pagador2Text pagador1 ++ ", " ++ pagador2Text pagador2 ++ " y " ++ String.fromInt (List.length rest) ++ " personas mas")

                                        --, pre [] [ text <| Debug.toString pago ]
                                        ]
                                    ]
                            )
                    )
                , div [ class "container" ]
                    [ button [ class "button", onClick <| ChangePagoPopoverState CreatingNewPago ] [ text "Agregar pago" ]
                    ]
                , pagosModal grupo model
                ]
            }


extractPagadorFromParte : Parte -> ParticipanteId
extractPagadorFromParte parte =
    case parte of
        MontoFijo _ participanteId ->
            participanteId

        Ponderado _ participanteId ->
            participanteId


pagosModal : Grupo -> Model -> Html Msg
pagosModal grupo model =
    div
        ([ class "modal"
         ]
            ++ (case model.pagoPopoverState of
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

                    Failure e ->
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
                        [ text "Crear pago" ]
                    , button
                        [ class "button"
                        , onClick <| ChangePagoPopoverState Closed
                        ]
                        [ text "Cancel" ]
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
    Html.form [ onSubmit <| PagoForm Form.Submit ]
        [ div [ class "field" ]
            [ label [ class "label" ]
                [ text "Nombre" ]
            , div [ class "control" ]
                [ Html.map PagoForm <|
                    FormInput.textInput nombreField
                        [ class "input"
                        , type_ "text"
                        , placeholder "After del viernes"
                        , classList [ ( "is-danger", hasError nombreField ) ]
                        ]
                , errorFor nombreField
                ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ]
                [ text "Monto" ]
            , div [ class "control" ]
                [ Html.map PagoForm <|
                    FormInput.textInput montoField
                        [ class "input"
                        , type_ "text"
                        , placeholder "After del viernes"
                        , classList [ ( "is-danger", hasError montoField ) ]
                        ]
                , errorFor montoField
                ]
            ]
        , div [ class "container" ]
            [ div [] [ text "Pagadores" ]
            , div [] <| List.map (\i -> parteForm participantes "pagadores" i form) (Form.getListIndexes "pagadores" form)
            ]
        , div [ class "container" ] <|
            [ button
                [ class "button"
                , onClick <| PagoForm <| Form.Append "pagadores"
                , type_ "button"
                ]
                [ text "Agregar parte" ]
            ]
        , div [ class "container" ]
            [ div [] [ text "Deudores" ]
            , div [] <| List.map (\i -> parteForm participantes "deudores" i form) (Form.getListIndexes "deudores" form)
            ]
        , div [ class "container" ] <|
            [ button
                [ class "button"
                , onClick <| PagoForm <| Form.Append "deudores"
                , type_ "button"
                ]
                [ text "Agregar parte" ]
            ]

        --, div [ class "control" ]
        --    [ button
        --        [ class "button is-primary"
        --        ]
        --        [ text "Crear" ]
        --    ]
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
            [ button [ class "button", type_ "button", onClick <| PagoForm <| Form.RemoveItem prefix i ]
                [ text "borrame"
                ]
            ]
        ]
