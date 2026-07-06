module Pages.Grupos.GrupoId_.Participantes exposing (Model, Msg, page)

import Browser.Dom
import Components.Bootstrap as Bs
import Effect exposing (Effect)
import Form exposing (Form, Msg(..))
import Form.Field
import Form.Validate exposing (Validation, andMap, andThen, field, nonEmpty, string, succeed)
import Generated.Api as Api exposing (Participante, ParticipanteAddParams, ParticipanteId, ULID, User)
import Html exposing (Html, div, i, input, label, span, text)
import Html.Attributes as Attr exposing (class, classList, for, id, type_, value)
import Html.Events exposing (on, onClick, onInput, onSubmit)
import Http
import Json.Decode
import Layouts
import Models.Grupo exposing (isOwnedBy, ownedParticipante)
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import Shared.Msg
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
        , view = view shared.store shared.currentUser
        }
        |> Page.withLayout (\_ -> Layouts.Default_Grupo {})


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
    | ClaimParticipante ULID
    | UnclaimParticipante ULID


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

        ClaimParticipante participanteId ->
            ( model
            , Effect.sendSharedMsg <|
                Shared.Msg.ClaimParticipante { grupoId = model.grupoId, participanteId = participanteId }
            )

        UnclaimParticipante participanteId ->
            ( model
            , Effect.sendSharedMsg <|
                Shared.Msg.UnclaimParticipante { grupoId = model.grupoId, participanteId = participanteId }
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Store -> WebData User -> Model -> View Msg
view store currentUser model =
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
            let
                -- Whether the logged-in user already owns a participante in this
                -- grupo. A user owns at most one, so while true no other row
                -- offers "Reclamar".
                ownsOne =
                    case currentUser of
                        Success u ->
                            ownedParticipante u.id grupo /= Nothing

                        _ ->
                            False
            in
            { title = grupo.nombre
            , body =
                [ div [ class "container py-4" ]
                    [ div [ class "row justify-content-center" ]
                        [ div [ class "col-lg-6" ]
                            [ div [ class "card mb-4" ]
                                [ div [ class "card-header" ] [ text "Participantes" ]
                                , Bs.listGroup [ class "list-group-flush" ]
                                    (grupo.participantes
                                        |> List.map (viewParticipanteItem currentUser ownsOne)
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


{-| A participante row: the name with a line describing the linked account below
it, plus (for logged-in users) a claim/unclaim action and the delete button.
`ownsOne` is whether the logged-in user already owns a participante in the grupo.
-}
viewParticipanteItem : WebData User -> Bool -> Participante -> Html Msg
viewParticipanteItem currentUser ownsOne p =
    Bs.listGroupItem [ class "d-flex justify-content-between align-items-center gap-2" ]
        [ div [ class "d-flex flex-column" ]
            [ span [] [ text p.nombre ]
            , viewOwnerLine currentUser p
            ]
        , div [ class "d-flex align-items-center gap-2" ]
            [ viewClaimAction currentUser ownsOne p
            , Bs.btn Bs.Danger
                [ onClick (DeleteParticipante p.id) ]
                [ text "Borrar" ]
            ]
        ]


{-| The small line under a participante's name describing who claimed it: "Vos"
for the logged-in user's own claim, the account's name + email for someone
else's, or "sin cuenta" when unclaimed.
-}
viewOwnerLine : WebData User -> Participante -> Html Msg
viewOwnerLine currentUser p =
    case p.user of
        Just owner ->
            let
                isMe =
                    case currentUser of
                        Success u ->
                            owner.id == u.id

                        _ ->
                            False
            in
            if isMe then
                span [ class "badge text-bg-success align-self-start" ] [ text "Vos" ]

            else
                span [ class "small text-muted" ]
                    [ i [ class "bi bi-person-check me-1" ] []
                    , text (owner.nombre ++ " · " ++ owner.email)
                    ]

        Nothing ->
            span [ class "small text-muted fst-italic" ] [ text "Sin cuenta" ]


{-| The claim/unclaim button for a participante, shown only to logged-in users:
"Dejar de reclamar" for the one they own, "Reclamar" for an unclaimed one (only
while they don't already own another in the grupo), and nothing for one already
claimed by someone else.
-}
viewClaimAction : WebData User -> Bool -> Participante -> Html Msg
viewClaimAction currentUser ownsOne p =
    case currentUser of
        Success u ->
            if isOwnedBy u.id p then
                Bs.btn Bs.Secondary
                    [ class "btn-sm", onClick (UnclaimParticipante p.id) ]
                    [ text "Dejar de reclamar" ]

            else
                case p.user of
                    Just _ ->
                        text ""

                    Nothing ->
                        if ownsOne then
                            text ""

                        else
                            Bs.btn Bs.Primary
                                [ class "btn-sm", onClick (ClaimParticipante p.id) ]
                                [ text "Reclamar" ]

        _ ->
            text ""


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
