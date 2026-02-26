module Pages.Grupos.GrupoId_.Participantes exposing (Model, Msg, page)

import Browser.Dom
import Components.NavBar as NavBar
import Components.Ui5 as Ui5
import Effect exposing (Effect)
import Form exposing (Form)
import Form.Validate exposing (Validation, andMap, andThen, field, nonEmpty, string, succeed)
import Generated.Api as Api exposing (Participante, ParticipanteAddParams, ParticipanteId, ULID)
import Html exposing (div, text)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick)
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
                    , grupo = Store.getGrupo m.grupoId shared.store
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
                [ div [] [ Ui5.text "Cargando..." ]
                ]
            }

        Failure _ ->
            { title = "Fallo"
            , body = []
            }

        Success grupo ->
            { title = grupo.nombre
            , body =
                [ Ui5.list
                    [ Attr.attribute "header-text" "Participantes"
                    , Attr.attribute "selection-mode" "Delete"
                    , on "item-delete"
                        (Json.Decode.at [ "detail", "item", "dataset", "id" ] Json.Decode.string
                            |> Json.Decode.map DeleteParticipante
                        )
                    ]
                    (grupo.participantes
                        |> List.map
                            (\p ->
                                Ui5.li
                                    [ Attr.attribute "data-id" p.participanteId ]
                                    [ text p.participanteNombre ]
                            )
                    )
                , Ui5.form ParticipanteForm
                    [ Attr.attribute "header-text" "Agregar Participante"
                    , Attr.attribute "labelSpan" "S12 M12 L12 XL12"
                    ]
                    [ Html.map ParticipanteForm <|
                        Ui5.textFormItem (Form.getFieldAsString "nombre" model.participanteForm)
                            { placeholder = Just "Juan"
                            , label = "Nombre"
                            , required = True
                            }
                    , Ui5.button
                        [ Attr.attribute "design" "Emphasized"
                        , onClick <| ParticipanteForm Form.Submit
                        ]
                        [ text "Agregar" ]
                    ]
                ]
            }
