module Pages.Home_ exposing (Model, Msg, page)

import Components.NavBar exposing (navBarItem)
import Components.Ui5 as Ui5
import Effect exposing (Effect, pushRoutePath)
import Form exposing (Form, Msg(..))
import Form.Validate exposing (Validation, andMap, andThen, field, nonEmpty, string, succeed)
import Generated.Api as Api exposing (CreateGrupoParams)
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (onClick)
import Layouts
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Utils.Form exposing (CustomFormError)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (\_ -> Layouts.Default { navBarContent = Just navBar, grupo = NotAsked })


navBar : Bool -> Html Shared.Msg
navBar _ =
    div
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "0.5rem"
        , style "width" "100%"
        ]
        [ navBarItem { currentPath = Path.Home_, path = Path.Home_, attrs = [ Ui5.slot "startContent" ] }
            [ text "🍌 Banana Split" ]
        ]


type alias Model =
    { form : Form CustomFormError CreateGrupoParams
    , createGrupoResponse : WebData Api.Grupo
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { form = Form.initial [] validate
      , createGrupoResponse = NotAsked
      }
    , Effect.setUnsavedChangesWarning False
    )


type Msg
    = NoOp
    | UpdateForm Form.Msg
    | GrupoCreatedRequest (WebData Api.Grupo)


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
                                        GrupoCreatedRequest (RemoteData.Success grupo)

                                    Err _ ->
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

        GrupoCreatedRequest (Success grupo) ->
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
subscriptions _ =
    Sub.none


view : Model -> View Msg
view model =
    let
        nombreField =
            Form.getFieldAsString "nombre" model.form

        participanteField =
            Form.getFieldAsString "participante" model.form
    in
    { title = "Banana split"
    , body =
        [ Ui5.form UpdateForm
            [ Attr.attribute "header-text" "Crear grupo"
            , Attr.attribute "layout" "S1 M1 L1 XL1"
            , Attr.attribute "label-span" "S12 M12 L12 XL12"
            ]
            [ Html.map UpdateForm <|
                Ui5.textFormItem
                    nombreField
                    { required = True
                    , label = "Nombre"
                    , placeholder = Just "After del viernes, Vacaciones a Calamuchita"
                    }
            , Html.map UpdateForm <|
                Ui5.textFormItem
                    participanteField
                    { required = True
                    , label = "Participante"
                    , placeholder = Just "Juan"
                    }
            , Ui5.button
                [ Attr.attribute "design" "Emphasized"
                , onClick <| UpdateForm Submit
                ]
                [ text "Crear" ]
            ]
        ]
    }
