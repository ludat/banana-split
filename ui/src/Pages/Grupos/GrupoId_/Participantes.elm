module Pages.Grupos.GrupoId_.Participantes exposing (Model, Msg, page)

import Browser.Dom
import Components.Bootstrap as Bs
import Components.NavBar as NavBar
import Effect exposing (Effect)
import Form exposing (Form, Msg(..))
import Form.Field
import Form.Validate exposing (Validation, andMap, andThen, field, nonEmpty, string, succeed)
import Generated.Api as Api exposing (Participante, ParticipanteAddParams, ParticipanteId, ULID)
import Html exposing (Html, div, input, label, text)
import Html.Attributes as Attr exposing (class, classList, for, id, type_, value)
import Html.Events exposing (on, onClick, onInput, onSubmit)
import Http
import Json.Decode
import Layouts
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared
import Task
import Utils.Form exposing (CustomFormError, errorForField, hasErrorField)
import Utils.Toasts exposing (pushToast)
import Utils.Toasts.Types exposing (ToastLevel(..))
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init shared.store route.params.grupoId
        , update = update shared.store
        , subscriptions = subscriptions
        , view = view shared.store
        }
        |> Page.withLayout
            (\_ ->
                Layouts.Default
                    { navBarContent = Just <| NavBar.navBar (NavBar.modelFromShared shared route.params.grupoId) shared.store route.path
                    }
            )


type alias Model =
    { grupoId : String
    , participanteForm : Form CustomFormError ParticipanteAddParams
    }


type Msg
    = NoOp
    | ParticipanteForm Form.Msg
    | GotAddedParticipanteResponse (Result Http.Error Participante)
    | DeleteParticipante ParticipanteId
    | DeleteParticipanteResponse (Result Http.Error ULID)


init : Store -> ULID -> ( Model, Effect Msg )
init store grupoId =
    ( { grupoId = grupoId
      , participanteForm = Form.initial [] validateParticipante
      }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Effect.getCurrentUser grupoId
        , Effect.setUnsavedChangesWarning False
        ]
    )


validateParticipante : Validation CustomFormError ParticipanteAddParams
validateParticipante =
    succeed Api.ParticipanteAddParams
        |> andMap (field "nombre" (string |> andThen nonEmpty))



-- UPDATE


update : Store -> Msg -> Model -> ( Model, Effect Msg )
update store msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        ParticipanteForm Form.Submit ->
            case ( Form.getOutput model.participanteForm, store |> Store.getGrupo model.grupoId ) of
                ( Just participanteParams, Success { id } ) ->
                    ( { model
                        | participanteForm = Form.update validateParticipante Form.Submit model.participanteForm
                      }
                    , Effect.sendCmd <|
                        Api.postGrupoByIdParticipantes id
                            participanteParams
                            GotAddedParticipanteResponse
                    )

                _ ->
                    ( { model
                        | participanteForm = Form.update validateParticipante Form.Submit model.participanteForm
                      }
                    , Effect.none
                    )

        ParticipanteForm formMsg ->
            ( { model | participanteForm = Form.update validateParticipante formMsg model.participanteForm }
            , Effect.none
            )

        GotAddedParticipanteResponse response ->
            case response of
                Ok _ ->
                    ( { model
                        | participanteForm = Form.initial [] validateParticipante
                      }
                    , Effect.batch
                        [ Effect.sendCmd <| Task.attempt (\_ -> NoOp) <| Browser.Dom.focus "nombre"
                        , Store.refreshGrupo model.grupoId
                        ]
                    )

                Err _ ->
                    ( model
                    , Effect.batch
                        [ pushToast ToastDanger "Fallo la creación del usuario"
                        ]
                    )

        DeleteParticipante participanteId ->
            ( model
            , Effect.batch
                [ Effect.sendCmd <|
                    Api.deleteGrupoByIdParticipantesByParticipanteId
                        model.grupoId
                        participanteId
                        DeleteParticipanteResponse
                ]
            )

        DeleteParticipanteResponse result ->
            case result of
                Ok _ ->
                    ( model, Store.refreshGrupo model.grupoId )

                Err _ ->
                    ( model, pushToast ToastDanger "Fallo al borrar el participante." )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


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
                [ div [ class "container py-4 text-muted" ] [ text "Cargando..." ]
                ]
            }

        Failure _ ->
            { title = "Fallo"
            , body = []
            }

        Success grupo ->
            { title = grupo.nombre
            , body =
                [ div [ class "container py-4" ]
                    [ div [ class "row justify-content-center" ]
                        [ div [ class "col-lg-6" ]
                            [ div [ class "card mb-4" ]
                                [ div [ class "card-header" ] [ text "Participantes" ]
                                , Bs.listGroup [ class "list-group-flush" ]
                                    (grupo.participantes
                                        |> List.map
                                            (\p ->
                                                Bs.listGroupItem [ class "d-flex justify-content-between align-items-center" ]
                                                    [ text p.nombre
                                                    , Bs.btn Bs.Danger
                                                        [ onClick (DeleteParticipante p.id) ]
                                                        [ text "Borrar" ]
                                                    ]
                                            )
                                    )
                                ]
                            , Html.form
                                [ onSubmit (ParticipanteForm Form.Submit)
                                , class "card"
                                ]
                                [ div [ class "card-header" ] [ text "Agregar Participante" ]
                                , div [ class "card-body" ]
                                    [ Html.map ParticipanteForm <|
                                        viewNombreField (Form.getFieldAsString "nombre" model.participanteForm)
                                    , Bs.btn Bs.Primary
                                        [ onClick (ParticipanteForm Form.Submit) ]
                                        [ text "Agregar" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            }


viewNombreField : Form.FieldState CustomFormError String -> Html Form.Msg
viewNombreField nombreField =
    div [ class "mb-3" ]
        [ label [ for nombreField.path, class "form-label" ] [ text "Nombre" ]
        , input
            [ type_ "text"
            , id nombreField.path
            , class "form-control"
            , classList [ ( "is-invalid", hasErrorField nombreField ) ]
            , value (Maybe.withDefault "" nombreField.value)
            , Attr.placeholder "Juan"
            , onInput (\v -> Input nombreField.path Form.Text (Form.Field.String v))
            , on "focus" (Json.Decode.succeed (Focus nombreField.path))
            , on "blur" (Json.Decode.succeed (Blur nombreField.path))
            , Attr.required True
            ]
            []
        , if hasErrorField nombreField then
            div [ class "invalid-feedback" ] [ errorForField nombreField ]

          else
            text ""
        ]
