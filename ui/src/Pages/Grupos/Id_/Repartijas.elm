module Pages.Grupos.Id_.Repartijas exposing (Model, Msg, page)

import Components.NavBar as NavBar
import Effect exposing (Effect)
import Form exposing (Form)
import Form.Error as FormError
import Form.Input as FormInput
import Form.Validate as V
import Generated.Api as Api exposing (Participante, Repartija, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit)
import Http
import Layouts
import Models.Monto exposing (validateMonto)
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared
import Utils.Form exposing (CustomFormError)
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts
import View exposing (View)


page : Shared.Model -> Route { id : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init shared.store route.params.id
        , update = update shared.store
        , subscriptions = subscriptions
        , view = view shared.store
        }
        |> Page.withLayout
            (\m ->
                Layouts.Default
                    { navBarContent = Just <| NavBar.navBar route.params.id shared.store route.path
                    }
            )



-- INIT


type alias Model =
    { grupoId : ULID, repartijaForm : Form CustomFormError Repartija }


init : Store -> ULID -> ( Model, Effect Msg )
init store grupoId =
    ( { grupoId = grupoId
      , repartijaForm = Form.initial [] validateRepartija
      }
    , Store.ensureGrupo grupoId store
    )


validateRepartija : V.Validation CustomFormError Repartija
validateRepartija =
    V.succeed Repartija
        |> V.andMap (V.succeed "00000000000000000000000000")
        |> V.andMap (V.field "nombre" (V.string |> V.andThen V.nonEmpty))
        |> V.andMap (V.field "monto" validateMonto)
        |> V.andMap (V.succeed [])



-- UPDATE


type Msg
    = NoOp String
    | RepartijaForm Form.Msg
    | CreateRepartijaSuccess Repartija
    | CreateRepartijaFailed Http.Error


update : Store -> Msg -> Model -> ( Model, Effect Msg )
update store msg model =
    case msg of
        NoOp s ->
            ( model
            , Effect.none
            )

        RepartijaForm Form.Submit ->
            case Form.getOutput model.repartijaForm of
                Just repartija ->
                    ( { model | repartijaForm = Form.update validateRepartija Form.Submit model.repartijaForm }
                    , Effect.sendCmd <|
                        Api.postGrupoByIdRepartijas model.grupoId
                            repartija
                            (\r ->
                                case r of
                                    Ok newRepartija ->
                                        CreateRepartijaSuccess newRepartija

                                    Err e ->
                                        CreateRepartijaFailed e
                            )
                    )

                Nothing ->
                    ( model
                    , Effect.none
                    )

        RepartijaForm formMsg ->
            ( { model
                | repartijaForm = Form.update validateRepartija formMsg model.repartijaForm
              }
            , Effect.none
            )

        CreateRepartijaSuccess repartija ->
            ( model
            , Toasts.pushToast Toasts.ToastSuccess "Repartija creada con éxito"
            )

        CreateRepartijaFailed e ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "Fallo la creacion de la repartija"
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Store -> Model -> View Msg
view store model =
    case Store.getGrupo model.grupoId store of
        Success grupo ->
            { title = "Pages.Grupos.Id_.Repartijas"
            , body =
                [ repartijaForm grupo.participantes model.repartijaForm

                --, pre [] [ text <| Debug.toString <| Form.getOutput model.repartijaForm ]
                ]
            }

        _ ->
            { title = "Repartijas"
            , body = [ text "nada por aqui" ]
            }


repartijaForm : List Participante -> Form CustomFormError Repartija -> Html Msg
repartijaForm participantes form =
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
    Html.form [ onSubmit <| RepartijaForm Form.Submit ]
        [ div [ class "field" ]
            [ label [ class "label" ]
                [ text "Nombre" ]
            , div [ class "control" ]
                [ Html.map RepartijaForm <|
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
                [ Html.map RepartijaForm <|
                    FormInput.textInput montoField
                        [ class "input"
                        , type_ "text"
                        , placeholder "After del viernes"
                        , classList [ ( "is-danger", hasError montoField ) ]
                        ]
                , errorFor montoField
                ]
            ]

        --, div [ class "container" ]
        --    [ div [] [ text "Deudores" ]
        --    , div [] <| List.map (\i -> parteForm participantes "deudores" i form) (Form.getListIndexes "deudores" form)
        --    ]
        --, div [ class "container" ] <|
        --    [ button
        --        [ class "button"
        --        , onClick <| RepartijaForm <| Form.Append "items"
        --        , type_ "button"
        --        ]
        --        [ text "Agregar parte" ]
        --    ]
        , div [ class "control" ]
            [ button
                [ class "button is-primary"
                ]
                [ text "Crear" ]
            ]
        ]



--parteForm : List Participante -> String -> Int -> Form CustomFormError Pago -> Html Msg
--parteForm participantes prefix i form =
--    let
--        errorFor field =
--            case field.liveError of
--                Just FormError.Empty ->
--                    p [ class "help is-danger" ] [ text "No puede ser vacio" ]
--
--                Just _ ->
--                    p [ class "help is-danger" ] [ text "Algo esta maloso" ]
--
--                Nothing ->
--                    text ""
--
--        hasError field =
--            case field.liveError of
--                Just _ ->
--                    True
--
--                Nothing ->
--                    False
--
--        tipoField =
--            Form.getFieldAsString (prefix ++ "." ++ String.fromInt i ++ ".tipo") form
--
--        montoField =
--            Form.getFieldAsString (prefix ++ "." ++ String.fromInt i ++ ".monto") form
--
--        cuotaField =
--            Form.getFieldAsString (prefix ++ "." ++ String.fromInt i ++ ".cuota") form
--
--        participanteField =
--            Form.getFieldAsString (prefix ++ "." ++ String.fromInt i ++ ".participante") form
--    in
--    div [ class "field has-addons" ]
--        [ p [ class "control" ]
--            [ span [ class "select" ]
--                [ Html.map PagoForm <|
--                    FormInput.selectInput
--                        [ ( "ponderado", "Cuota" )
--                        , ( "fijo", "$" )
--                        ]
--                        tipoField
--                        []
--                ]
--            ]
--        , div [ class "control" ]
--            [ case tipoField.value of
--                Just "ponderado" ->
--                    Html.map PagoForm <|
--                        FormInput.textInput cuotaField
--                            [ class "input"
--                            , type_ "text"
--                            , placeholder "CUOTA"
--                            , classList [ ( "is-danger", hasError cuotaField ) ]
--                            ]
--
--                Just "fijo" ->
--                    Html.map PagoForm <|
--                        FormInput.textInput montoField
--                            [ class "input"
--                            , type_ "text"
--                            , placeholder "FIJO"
--                            , classList [ ( "is-danger", hasError montoField ) ]
--                            ]
--
--                _ ->
--                    Html.map PagoForm <|
--                        FormInput.textInput montoField
--                            [ class "input"
--                            , type_ "text"
--                            , placeholder "FIJO"
--                            , classList [ ( "is-danger", hasError montoField ) ]
--                            ]
--            ]
--        , p [ class "control" ]
--            [ span [ class "select" ]
--                [ Html.map PagoForm <|
--                    FormInput.selectInput
--                        (List.map (\p -> ( p.participanteId, p.participanteNombre )) participantes)
--                        participanteField
--                        []
--                ]
--            ]
--        , p [ class "control" ]
--            [ button [ class "button", type_ "button", onClick <| PagoForm <| Form.RemoveItem prefix i ]
--                [ text "borrame"
--                ]
--            ]
--        ]
