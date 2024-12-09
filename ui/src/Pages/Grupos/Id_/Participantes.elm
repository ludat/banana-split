module Pages.Grupos.Id_.Participantes exposing (Model, Msg, page)

import Browser.Dom
import Components.NavBar as NavBar
import Effect exposing (Effect)
import Form exposing (Form)
import Form.Error as FormError
import Form.Input as FormInput
import Form.Validate exposing (Validation, andMap, andThen, field, nonEmpty, string, succeed)
import Generated.Api as Api exposing (Grupo, Participante, ParticipanteAddParams, ParticipanteId, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Layouts
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import Task
import Utils.Form exposing (CustomFormError)
import View exposing (View)


page : Shared.Model -> Route { id : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.id
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout
            (\m ->
                Layouts.Default
                    { navBarContent = Just <| NavBar.navBar route.params.id m.remoteGrupo
                    }
            )


type alias Model =
    { remoteGrupo : WebData Grupo
    , participanteForm : Form CustomFormError ParticipanteAddParams
    }


type Msg
    = NoOp
    | GrupoResponse (WebData Grupo)
    | ParticipanteForm Form.Msg
    | AddedParticipante Participante
    | DeleteParticipante ParticipanteId
    | DeleteParticipanteResponse (Result Http.Error ULID)


init : ULID -> ( Model, Effect Msg )
init grupoId =
    ( { remoteGrupo = Loading
      , participanteForm = Form.initial [] validateParticipante
      }
    , Effect.batch
        [ Effect.sendCmd <| Api.getGrupoById grupoId (RemoteData.fromResult >> GrupoResponse)
        ]
    )


validateParticipante : Validation CustomFormError ParticipanteAddParams
validateParticipante =
    succeed Api.ParticipanteAddParams
        |> andMap (field "nombre" (string |> andThen nonEmpty))



-- UPDATE


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        GrupoResponse webData ->
            ( { model | remoteGrupo = webData }
            , Effect.none
            )

        ParticipanteForm Form.Submit ->
            case ( Form.getOutput model.participanteForm, model.remoteGrupo ) of
                ( Just participanteParams, Success { grupoId } ) ->
                    ( { model
                        | participanteForm = Form.update validateParticipante Form.Submit model.participanteForm
                      }
                    , Effect.sendCmd <|
                        Api.postGrupoByIdParticipantes grupoId
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

        AddedParticipante participante ->
            ( { model
                | remoteGrupo =
                    model.remoteGrupo
                        |> RemoteData.map (\grupo -> { grupo | participantes = grupo.participantes ++ [ participante ] })
                , participanteForm = Form.initial [] validateParticipante
              }
            , Effect.sendCmd <| Task.attempt (\_ -> NoOp) <| Browser.Dom.focus "nombre"
            )

        DeleteParticipante participanteId ->
            case model.remoteGrupo of
                NotAsked ->
                    ( model, Effect.none )

                Loading ->
                    ( model, Effect.none )

                Failure e ->
                    ( model, Effect.none )

                Success grupo ->
                    ( model
                    , Effect.batch
                        [ Effect.sendCmd <|
                            Api.deleteGrupoByIdParticipantesByParticipanteId
                                grupo.grupoId
                                participanteId
                                DeleteParticipanteResponse
                        ]
                    )

        DeleteParticipanteResponse result ->
            case result of
                Ok participanteBorrado ->
                    ( { model
                        | remoteGrupo =
                            model.remoteGrupo
                                |> RemoteData.map
                                    (\grupo ->
                                        { grupo
                                            | participantes =
                                                grupo.participantes
                                                    |> List.filter (\p -> p.participanteId /= participanteBorrado)
                                        }
                                    )
                      }
                    , Effect.none
                    )

                Err e ->
                    ( model, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> View Msg
view model =
    case model.remoteGrupo of
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
                    (grupo.participantes
                        |> List.map
                            (\p ->
                                div []
                                    [ text p.participanteNombre
                                    , button [ class "delete", onClick <| DeleteParticipante p.participanteId ] []
                                    ]
                            )
                    )
                , div [ class "container" ]
                    [ participantesForm model.participanteForm
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
    Html.form [ onSubmit <| ParticipanteForm Form.Submit ]
        [ div [ class "field" ]
            [ label [ class "label" ]
                [ text "Nombre" ]
            , div [ class "control" ]
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
        , div [ class "control" ]
            [ button
                [ class "button is-primary"
                ]
                [ text "Agregar Participante" ]
            ]
        ]
