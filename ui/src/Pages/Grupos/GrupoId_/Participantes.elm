module Pages.Grupos.GrupoId_.Participantes exposing (Accion, Confirmacion, Edicion, Model, Msg, page)

import Browser.Dom
import Components.Bootstrap as Bs
import Effect exposing (Effect)
import Form exposing (Form, Msg(..))
import Form.Field
import Form.Validate exposing (Validation, andMap, andThen, field, nonEmpty, string, succeed)
import Generated.Api as Api exposing (Participante, ParticipanteAddParams, ParticipanteId, ULID, User)
import Html exposing (Html, button, div, i, input, label, li, span, text, ul)
import Html.Attributes as Attr exposing (class, classList, for, id, type_, value)
import Html.Events exposing (on, onClick, onInput, onSubmit)
import Http
import Json.Decode
import Layouts
import Models.Grupo exposing (datosDeCobro, isOwnedBy, ownedParticipante)
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import Shared.Msg
import Task
import Utils.Form exposing (CustomFormError, errorForField, hasErrorField)
import Utils.Telemetry as Telemetry
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
    , edicion : Maybe Edicion
    , confirmacion : Maybe Confirmacion
    }


{-| Las acciones del menú que piden confirmación antes de hacerse.
-}
type Accion
    = Borrar
    | Reclamar
    | DejarDeReclamar


type alias Confirmacion =
    { accion : Accion
    , participante : Participante
    }


{-| El popup para cambiarle el nombre a un participante.
-}
type alias Edicion =
    { participanteId : ULID
    , nombre : String
    , datosDeCobro : String

    -- Los de la cuenta que lo reclamó, que se usan si se deja vacío.
    , datosDeLaCuenta : Maybe String
    , enviando : Bool
    , error : Maybe String
    }


type Msg
    = NoOp
    | PedirConfirmacion Accion Participante
    | CancelarConfirmacion
    | Confirmar
    | AbrirEdicion Participante
    | CerrarEdicion
    | NombreEdicionCambiado String
    | DatosDeCobroCambiados String
    | GuardarEdicion
    | GotEdicion (Result Http.Error Participante)
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
      , edicion = Nothing
      , confirmacion = Nothing
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

        PedirConfirmacion accion p ->
            ( { model | confirmacion = Just { accion = accion, participante = p } }, Effect.none )

        CancelarConfirmacion ->
            ( { model | confirmacion = Nothing }, Effect.none )

        Confirmar ->
            case model.confirmacion of
                Just { accion, participante } ->
                    update store
                        (case accion of
                            Borrar ->
                                DeleteParticipante participante.id

                            Reclamar ->
                                ClaimParticipante participante.id

                            DejarDeReclamar ->
                                UnclaimParticipante participante.id
                        )
                        { model | confirmacion = Nothing }

                Nothing ->
                    ( model, Effect.none )

        AbrirEdicion p ->
            ( { model
                | edicion =
                    Just
                        { participanteId = p.id
                        , nombre = p.nombre
                        , datosDeCobro = Maybe.withDefault "" p.datosDeCobro
                        , datosDeLaCuenta = p.user |> Maybe.andThen .datosDeCobro
                        , enviando = False
                        , error = Nothing
                        }
              }
            , Effect.sendCmd <| Task.attempt (\_ -> NoOp) <| Browser.Dom.focus "edicion-nombre"
            )

        CerrarEdicion ->
            ( { model | edicion = Nothing }, Effect.none )

        DatosDeCobroCambiados datos ->
            ( { model | edicion = model.edicion |> Maybe.map (\e -> { e | datosDeCobro = datos }) }
            , Effect.none
            )

        NombreEdicionCambiado nombre ->
            ( { model | edicion = model.edicion |> Maybe.map (\e -> { e | nombre = nombre, error = Nothing }) }
            , Effect.none
            )

        GuardarEdicion ->
            case model.edicion of
                Just edicion ->
                    if edicion.enviando then
                        ( model, Effect.none )

                    else
                        guardarEdicion store edicion model

                Nothing ->
                    ( model, Effect.none )

        GotEdicion result ->
            case ( model.edicion, result ) of
                ( Just _, Ok p ) ->
                    ( { model | edicion = Nothing }
                    , Effect.batch
                        [ Store.refreshGrupo model.grupoId
                        , pushToast ToastSuccess ("Ahora se llama " ++ p.nombre)
                        ]
                    )

                ( Just edicion, Err error ) ->
                    ( { model
                        | edicion =
                            Just
                                { edicion
                                    | enviando = False
                                    , error =
                                        Just <|
                                            case error of
                                                Http.BadStatus 409 ->
                                                    nombreRepetido

                                                _ ->
                                                    "No pudimos cambiar el nombre, probá de nuevo"
                                }
                      }
                    , Effect.none
                    )

                ( Nothing, _ ) ->
                    ( model, Effect.none )

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

                -- Sigue apareciendo en algún gasto o transferencia.
                Err (Http.BadStatus 409) ->
                    ( model
                    , pushToast ToastDanger "Este participante está siendo usado, no se puede borrar hasta que nadie lo referencie."
                    )

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


guardarEdicion : Store -> Edicion -> Model -> ( Model, Effect Msg )
guardarEdicion store edicion model =
    let
        nombre =
            String.trim edicion.nombre
    in
    if String.isEmpty nombre then
        ( { model | edicion = Just { edicion | error = Just "Poné un nombre" } }, Effect.none )

    else if nombreDeOtro store model.grupoId edicion.participanteId nombre then
        ( { model | edicion = Just { edicion | error = Just nombreRepetido } }, Effect.none )

    else
        ( { model | edicion = Just { edicion | enviando = True, error = Nothing } }
        , Effect.sendCmd <|
            Api.putGrupoByIdParticipantesByParticipanteId model.grupoId
                edicion.participanteId
                { nombre = nombre
                , datosDeCobro =
                    if String.isEmpty (String.trim edicion.datosDeCobro) then
                        Nothing

                    else
                        Just (String.trim edicion.datosDeCobro)
                }
                GotEdicion
        )


{-| Si otro participante del grupo ya se llama así: los nombres son únicos en
el grupo. El backend también lo rechaza, esto es para avisar sin ir y volver.
-}
nombreDeOtro : Store -> ULID -> ULID -> String -> Bool
nombreDeOtro store grupoId participanteId nombre =
    case Store.getGrupo grupoId store of
        Success grupo ->
            List.any (\p -> p.id /= participanteId && p.nombre == nombre) grupo.participantes

        _ ->
            False


nombreRepetido : String
nombreRepetido =
    "Ya hay un participante con ese nombre"


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
                            [ div [ id "participantes-lista", class "card mb-4" ]
                                [ div [ class "card-header" ] [ text "Participantes" ]
                                , Bs.listGroup [ class "list-group-flush" ]
                                    (grupo.participantes
                                        |> List.map (viewParticipanteItem currentUser ownsOne)
                                    )
                                ]
                            , Html.form
                                [ id "participantes-agregar"
                                , onSubmit (ParticipanteForm Form.Submit)
                                , class "card"
                                ]
                                [ div [ class "card-header" ] [ text "Agregar Participante" ]
                                , div [ class "card-body" ]
                                    [ Html.map ParticipanteForm <|
                                        viewNombreField (Form.getFieldAsString "nombre" model.participanteForm)
                                    , Bs.btn Bs.Primary
                                        [ id "agregar-participante"
                                        , Telemetry.click "participantes.add"
                                        , type_ "submit"
                                        ]
                                        [ text "Agregar" ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                , viewEdicionModal model.edicion
                , viewConfirmacionModal model.confirmacion
                ]
            }


viewConfirmacionModal : Maybe Confirmacion -> Html Msg
viewConfirmacionModal confirmacion =
    let
        { titulo, pregunta, boton, variante, telemetria } =
            case confirmacion of
                Just { accion, participante } ->
                    case accion of
                        Borrar ->
                            { titulo = "Borrar participante"
                            , pregunta = "¿Seguro que querés borrar a " ++ participante.nombre ++ "? No se puede deshacer."
                            , boton = "Borrar"
                            , variante = Bs.Danger
                            , telemetria = "participantes.delete_confirm"
                            }

                        Reclamar ->
                            { titulo = "Reclamar participante"
                            , pregunta = "¿Sos " ++ participante.nombre ++ "? Va a quedar asociado a tu cuenta."
                            , boton = "Sí, soy yo"
                            , variante = Bs.Primary
                            , telemetria = "participantes.claim_confirm"
                            }

                        DejarDeReclamar ->
                            { titulo = "Dejar de reclamar"
                            , pregunta = participante.nombre ++ " va a dejar de estar asociado a tu cuenta."
                            , boton = "Dejar de reclamar"
                            , variante = Bs.Primary
                            , telemetria = "participantes.unclaim_confirm"
                            }

                Nothing ->
                    { titulo = "", pregunta = "", boton = "", variante = Bs.Primary, telemetria = "" }
    in
    Bs.modal
        { id = "modal-confirmar-participante"
        , isOpen = confirmacion /= Nothing
        , onClose = CancelarConfirmacion
        , centered = True
        , title = titulo
        , body = [ Html.p [ class "mb-0" ] [ text pregunta ] ]
        , footer =
            [ Bs.btn Bs.Transparent
                [ Telemetry.click "participantes.confirm_cancel", onClick CancelarConfirmacion ]
                [ text "Cancelar" ]
            , Bs.btn variante [ Telemetry.click telemetria, onClick Confirmar ] [ text boton ]
            ]
        }


viewEdicionModal : Maybe Edicion -> Html Msg
viewEdicionModal edicion =
    let
        enviando =
            edicion |> Maybe.map .enviando |> Maybe.withDefault False
    in
    Bs.modal
        { id = "modal-editar-participante"
        , isOpen = edicion /= Nothing
        , onClose =
            if enviando then
                NoOp

            else
                CerrarEdicion
        , centered = True
        , title = "Editar participante"
        , body =
            case edicion of
                Just e ->
                    [ Html.form [ id "edicion-form", onSubmit GuardarEdicion ]
                        [ label [ class "form-label", for "edicion-nombre" ] [ text "Nombre" ]
                        , input
                            [ id "edicion-nombre"
                            , type_ "text"
                            , classList [ ( "form-control", True ), ( "is-invalid", e.error /= Nothing ) ]
                            , value e.nombre
                            , onInput NombreEdicionCambiado
                            , Attr.disabled e.enviando
                            ]
                            []
                        , case e.error of
                            Just error ->
                                div [ class "invalid-feedback" ] [ text error ]

                            Nothing ->
                                text ""
                        , label [ class "form-label mt-3", for "edicion-datos-de-cobro" ] [ text "¿Cómo quiere recibir la plata?" ]
                        , Html.textarea
                            [ id "edicion-datos-de-cobro"
                            , class "form-control"
                            , Attr.rows 3
                            , Attr.placeholder "Alias: juan.mp, CBU..., en efectivo"
                            , value e.datosDeCobro
                            , onInput DatosDeCobroCambiados
                            , Attr.disabled e.enviando
                            ]
                            []
                        , div [ class "form-text", Attr.style "white-space" "pre-line" ]
                            [ text <|
                                case e.datosDeLaCuenta of
                                    Just datos ->
                                        "Si lo dejás vacío se usan los de su cuenta:\n" ++ datos

                                    Nothing ->
                                        "Opcional. Lo ve el resto del grupo para saber cómo pagarle."
                            ]
                        ]
                    ]

                Nothing ->
                    []
        , footer =
            [ Bs.btn Bs.Transparent
                [ Telemetry.click "participantes.edit_cancel"
                , onClick CerrarEdicion
                , Attr.disabled enviando
                ]
                [ text "Cancelar" ]
            , Bs.btn Bs.Primary
                [ type_ "submit"
                , Attr.attribute "form" "edicion-form"
                , Telemetry.click "participantes.edit_save"
                , Attr.disabled enviando
                ]
                [ text "Guardar" ]
            ]
        }


{-| A participante row: the name with a line describing the linked account below
it, and a "⋮" menu with its actions (claim/unclaim for logged-in users, edit,
delete). `ownsOne` is whether the logged-in user already owns a participante in
the grupo.
-}
viewParticipanteItem : WebData User -> Bool -> Participante -> Html Msg
viewParticipanteItem currentUser ownsOne p =
    Bs.listGroupItem [ class "d-flex justify-content-between align-items-center gap-2" ]
        [ div [ class "d-flex flex-column" ]
            [ span [] [ text p.nombre ]
            , viewOwnerLine currentUser p
            , case datosDeCobro p of
                Just datos ->
                    span [ class "small text-muted mt-1", Attr.style "white-space" "pre-line" ]
                        [ i [ class "bi bi-wallet2 me-1" ] [], text datos ]

                Nothing ->
                    text ""
            ]
        , div [ class "dropdown" ]
            [ button
                [ type_ "button"
                , class "btn btn-sm btn-outline-secondary"
                , Attr.attribute "data-bs-toggle" "dropdown"
                , Attr.attribute "aria-expanded" "false"
                , Attr.attribute "aria-label" ("Acciones para " ++ p.nombre)
                , Telemetry.click "participantes.menu"
                ]
                [ i [ class "bi bi-three-dots-vertical" ] [] ]
            , ul [ class "dropdown-menu dropdown-menu-end shadow" ]
                (viewClaimAction currentUser ownsOne p
                    ++ [ menuItem
                            { telemetria = "participantes.edit"
                            , msg = AbrirEdicion p
                            , icono = "bi-pencil"
                            , label = "Editar"
                            , clase = ""
                            }
                       , li [] [ Html.hr [ class "dropdown-divider" ] [] ]
                       , menuItem
                            { telemetria = "participantes.delete"
                            , msg = PedirConfirmacion Borrar p
                            , icono = "bi-trash"
                            , label = "Borrar"
                            , clase = "text-danger"
                            }
                       ]
                )
            ]
        ]


menuItem : { telemetria : String, msg : Msg, icono : String, label : String, clase : String } -> Html Msg
menuItem { telemetria, msg, icono, label, clase } =
    li []
        [ button
            [ type_ "button"
            , class ("dropdown-item " ++ clase)
            , Telemetry.click telemetria
            , onClick msg
            ]
            [ i [ class ("bi me-2 " ++ icono) ] []
            , text label
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


{-| The claim/unclaim menu item for a participante, shown only to logged-in
users: "Dejar de reclamar" for the one they own, "Reclamar" for an unclaimed one
(only while they don't already own another in the grupo), and nothing for one
already claimed by someone else.
-}
viewClaimAction : WebData User -> Bool -> Participante -> List (Html Msg)
viewClaimAction currentUser ownsOne p =
    case currentUser of
        Success u ->
            if isOwnedBy u.id p then
                [ menuItem
                    { telemetria = "participantes.unclaim"
                    , msg = PedirConfirmacion DejarDeReclamar p
                    , icono = "bi-person-dash"
                    , label = "Dejar de reclamar"
                    , clase = ""
                    }
                ]

            else
                case p.user of
                    Just _ ->
                        []

                    Nothing ->
                        if ownsOne then
                            []

                        else
                            [ menuItem
                                { telemetria = "participantes.claim"
                                , msg = PedirConfirmacion Reclamar p
                                , icono = "bi-person-check"
                                , label = "Reclamar (soy yo)"
                                , clase = ""
                                }
                            ]

        _ ->
            []


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
