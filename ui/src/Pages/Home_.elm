module Pages.Home_ exposing (Model, Msg, page)

import Components.Bootstrap as Bs
import Effect exposing (Effect, pushRoutePath)
import Form exposing (Form, Msg(..))
import Form.Field
import Form.Validate exposing (Validation, andMap, andThen, field, nonEmpty, string, succeed)
import Generated.Api as Api exposing (CreateGrupoParams)
import Html exposing (div, input, label, text)
import Html.Attributes as Attr exposing (class, classList, for, id, placeholder, required, type_)
import Html.Events exposing (onClick)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Utils.Form exposing (CustomFormError, errorForField, hasErrorField)
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (\_ -> Layouts.Default {})


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

        GrupoCreated grupo ->
            ( { model | form = Form.initial [] validate }
            , Effect.batch
                [ -- Automatically select the first participante as current user
                  case grupo.participantes of
                    participante :: _ ->
                        Effect.saveCurrentUser grupo.id participante.id

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
        [ div [ class "container py-4" ]
            [ div [ class "row justify-content-center" ]
                [ div [ class "col-12 col-md-6" ]
                    [ Bs.card []
                        [ Bs.cardHeader [] [ text "Crear grupo" ]
                        , Bs.cardBody []
                            [ div [ class "mb-3" ]
                                [ label [ for "nombre", class "form-label" ] [ text "Nombre" ]
                                , Html.map UpdateForm <|
                                    input
                                        [ id "nombre"
                                        , type_ "text"
                                        , classList [ ( "form-control", True ), ( "is-invalid", hasErrorField nombreField ) ]
                                        , placeholder "After del viernes, Vacaciones a Calamuchita"
                                        , required True
                                        , Attr.value (Maybe.withDefault "" nombreField.value)
                                        , Html.Events.onInput (Form.Input nombreField.path Form.Text << Form.Field.String)
                                        ]
                                        []
                                , div [ class "invalid-feedback" ] [ Html.map UpdateForm <| errorForField nombreField ]
                                ]
                            , div [ class "mb-3" ]
                                [ label [ for "participante", class "form-label" ] [ text "Participante" ]
                                , Html.map UpdateForm <|
                                    input
                                        [ id "participante"
                                        , type_ "text"
                                        , classList [ ( "form-control", True ), ( "is-invalid", hasErrorField participanteField ) ]
                                        , placeholder "Juan"
                                        , required True
                                        , Attr.value (Maybe.withDefault "" participanteField.value)
                                        , Html.Events.onInput (Form.Input participanteField.path Form.Text << Form.Field.String)
                                        ]
                                        []
                                , div [ class "invalid-feedback" ] [ Html.map UpdateForm <| errorForField participanteField ]
                                ]
                            , Bs.btn Bs.Primary
                                [ onClick <| UpdateForm Submit ]
                                [ text "Crear" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    }
