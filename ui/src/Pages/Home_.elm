module Pages.Home_ exposing (Model, Msg, page)

import Browser.Navigation exposing (pushUrl)
import Effect exposing (Effect, pushRoutePath)
import Form exposing (Form)
import Form.Error as Form
import Form.Input as FormInput
import Form.Validate as Validate exposing (..)
import Generated.Api as Api exposing (CreateGrupoParams)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick, onSubmit)
import Http
import Layouts
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Route.Path as Path
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (\m -> Layouts.Default { navBarContent = Just navBar })


navBar : Bool -> Html msg
navBar navBarOpen =
    div
        [ classList [ ( "is-active", navBarOpen ) ]
        , class "navbar-menu"
        ]
        [ div [ class "navbar-start" ]
            []
        ]


type alias Model =
    { form : Form () CreateGrupoParams
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { form = Form.initial [] validate
      }
    , Effect.none
    )


type Msg
    = NoOp
    | UpdateForm Form.Msg
    | GrupoCreated Api.Grupo


validate : Validation () CreateGrupoParams
validate =
    succeed CreateGrupoParams
        |> andMap (field "nombre" (string |> andThen nonEmpty))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        UpdateForm Form.Submit ->
            case Form.getOutput model.form of
                Just createGrupoParams ->
                    ( { model | form = Form.update validate Form.Submit model.form }
                    , Effect.sendCmd <|
                        Api.postApiGrupo createGrupoParams
                            (\r ->
                                case r of
                                    Ok grupo ->
                                        GrupoCreated grupo

                                    Err error ->
                                        NoOp
                            )
                    )

                Nothing ->
                    ( { model | form = Form.update validate Form.Submit model.form }
                    , Effect.none
                    )

        UpdateForm formMsg ->
            ( { model | form = Form.update validate formMsg model.form }
            , Effect.none
            )

        GrupoCreated grupo ->
            ( { model | form = Form.initial [] validate }
            , pushRoutePath <| Path.Grupos_Id_ { id = grupo.grupoId }
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> View Msg
view model =
    let
        errorFor field =
            case field.liveError of
                Just Form.Empty ->
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

        nombreField =
            Form.getFieldAsString "nombre" model.form
    in
    { title = ""
    , body =
        [ div [ class "container" ]
            [ section [ class "section" ]
                [ Html.form [ onSubmit <| UpdateForm Form.Submit ]
                    [ div [ class "field" ]
                        [ label [ class "label" ]
                            [ text "Nombre" ]
                        , div [ class "control" ]
                            [ Html.map UpdateForm <|
                                FormInput.textInput nombreField
                                    [ class "input"
                                    , type_ "text"
                                    , placeholder "After del viernes"
                                    , classList [ ( "is-danger", hasError nombreField ) ]
                                    ]
                            , errorFor nombreField
                            ]
                        ]
                    , div [ class "control" ]
                        [ button
                            [ class "button is-primary"
                            ]
                            [ text "Crear" ]
                        ]
                    ]
                ]
            ]
        ]
    }
