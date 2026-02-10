module Pages.Home_ exposing (Model, Msg, page)

import Components.Ui5 exposing (..)
import Effect exposing (Effect, pushRoutePath)
import Form exposing (Form, Msg(..))
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
import Utils.Form exposing (CustomFormError)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (\m -> Layouts.Default { navBarContent = Just navBar, grupo = NotAsked })


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
    { form : Form CustomFormError CreateGrupoParams
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { form = Form.initial [] validate
      }
    , Effect.setUnsavedChangesWarning False
    )


type Msg
    = NoOp
    | UpdateForm Form.Msg
    | GrupoCreated Api.Grupo


validate : Validation CustomFormError CreateGrupoParams
validate =
    succeed CreateGrupoParams
        |> andMap (field "nombre" (string |> andThen nonEmpty))
        |> andMap (field "participante" (string |> andThen nonEmpty))


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
                        Api.postGrupo createGrupoParams
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
            , Effect.batch
                [ -- Automatically select the first participante as current user
                  case grupo.participantes of
                    participante :: _ ->
                        Effect.saveCurrentUser grupo.id participante.participanteId

                    [] ->
                        Effect.none
                , pushRoutePath <| Path.Grupos_Id_ { id = grupo.id }
                ]
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> View Msg
view model =
    let
        nombreField =
            Form.getFieldAsString "nombre" model.form

        participanteField =
            Form.getFieldAsString "participante" model.form
    in
    { title = ""
    , body =
        [ Html.form [ onSubmit <| UpdateForm Submit ]
            [ Html.node "ui5-form"
                [ Attr.attribute "header-text" "Crear grupo"
                , Attr.attribute "layout" "S1 M1 L1 XL1"
                , Attr.attribute "label-span" "S12 M12 L12 XL12"
                ]
                [ Html.map UpdateForm <|
                    ui5TextFormItem
                        nombreField
                        { required = True
                        , label = "Nombre"
                        , placeholder = Just "After del viernes, Vacaciones a Calamuchita"
                        }
                , Html.map UpdateForm <|
                    ui5TextFormItem
                        participanteField
                        { required = True
                        , label = "Participante"
                        , placeholder = Just "Juan"
                        }
                , Html.node "ui5-button"
                    [ Attr.attribute "design" "Emphasized"
                    , onClick <| UpdateForm Submit
                    ]
                    [ text "Crear" ]
                ]
            ]
        ]
    }
