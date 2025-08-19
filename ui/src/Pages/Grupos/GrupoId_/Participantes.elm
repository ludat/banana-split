module Pages.Grupos.GrupoId_.Participantes exposing (Model, Msg, page)

import Browser.Dom
import Components.NavBar as NavBar
import Effect exposing (Effect)
import FeatherIcons as Icons
import Form exposing (Form)
import Form.Error as FormError
import Form.Input as FormInput
import Form.Validate exposing (Validation, andMap, andThen, field, nonEmpty, string, succeed)
import Generated.Api as Api exposing (Grupo, Participante, ParticipanteAddParams, ParticipanteId, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Http
import Layouts
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import Task
import Utils.Form exposing (CustomFormError)
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
            (\m ->
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
    | AddedParticipante Participante
    | DeleteParticipante ParticipanteId
    | DeleteParticipanteResponse (Result Http.Error ULID)


init : Store -> ULID -> ( Model, Effect Msg )
init store grupoId =
    ( { grupoId = grupoId
      , participanteForm = Form.initial [] validateParticipante
      }
    , Store.ensureGrupo grupoId store
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
                            (\r ->
                                case r of
                                    Ok participante ->
                                        AddedParticipante participante

                                    Err error ->
                                        NoOp
                            )
                    )

                ( _, _ ) ->
                    ( { model
                        | participanteForm = Form.update validateParticipante Form.Submit model.participanteForm
                      }
                    , Effect.none
                    )

        ParticipanteForm formMsg ->
            ( { model | participanteForm = Form.update validateParticipante formMsg model.participanteForm }
            , Effect.none
            )

        AddedParticipante _ ->
            ( { model
                | participanteForm = Form.initial [] validateParticipante
              }
            , Effect.batch
                [ Effect.sendCmd <| Task.attempt (\_ -> NoOp) <| Browser.Dom.focus "nombre"
                , Store.refreshGrupo model.grupoId
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
                Ok participanteBorrado ->
                    ( model, Store.refreshGrupo model.grupoId )

                Err e ->
                    ( model, pushToast ToastDanger "Fallo al borrar el participante." )


subscriptions : Model -> Sub Msg
subscriptions model =
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
            { title = grupo.nombre
            , body =
                [ div [ class "container px-4" ]
                    [ Html.form
                        [ onSubmit <| ParticipanteForm Form.Submit ]
                        [ table [ class "table" ]
                            [ thead []
                                [ tr []
                                    [ th [ class "px-2" ] [ text "Nombre" ]
                                    , th [] []
                                    ]
                                ]
                            , Html.Keyed.node "tbody"
                                []
                                ((grupo.participantes
                                    |> List.map
                                        (\p ->
                                            ( p.participanteId
                                            , tr []
                                                [ td [ class "is-vcentered px-4" ] [ text p.participanteNombre ]
                                                , td []
                                                    [ div [ class "button is-danger is-outlined", onClick <| DeleteParticipante p.participanteId ]
                                                        [ Icons.toHtml [] Icons.trash2 ]
                                                    ]
                                                ]
                                            )
                                        )
                                 )
                                    ++ [ ( "new", participantesForm model.participanteForm ) ]
                                )
                            ]
                        ]
                    ]
                ]
            }


participantesForm : Form CustomFormError ParticipanteAddParams -> Html Msg
participantesForm form =
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

        nombreField =
            Form.getFieldAsString "nombre" form
    in
    tr []
        [ td [ class "field" ]
            [ div [ class "control" ]
                [ Html.map ParticipanteForm <|
                    FormInput.textInput nombreField
                        [ class "input"
                        , type_ "text"
                        , placeholder "Juan"
                        , id nombreField.path
                        , classList [ ( "is-danger", hasError nombreField ) ]
                        ]
                , errorFor nombreField
                ]
            ]
        , td [ class "control" ]
            [ button
                [ class "button is-primary has-text-white" ]
                [ Icons.toHtml [] Icons.userPlus ]
            ]
        ]
