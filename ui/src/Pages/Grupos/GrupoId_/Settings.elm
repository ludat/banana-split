module Pages.Grupos.GrupoId_.Settings exposing (Model, Msg, page)

import Components.Bootstrap as Bs
import Effect exposing (Effect)
import Form exposing (Form, Msg(..))
import Form.Field
import Form.Init as Form
import Form.Validate as V exposing (Validation)
import Generated.Api as Api exposing (ShallowGrupo, ULID, UpdateGrupoParams, User)
import Html exposing (Html, a, button, div, i, input, label, option, select, span, text)
import Html.Attributes as Attr exposing (class, classList, disabled, for, id, selected, type_, value)
import Html.Events exposing (on, onClick, onInput, onSubmit)
import Http
import Json.Decode
import Layouts
import Models.Grupo exposing (ownedParticipante)
import Models.Moneda as Moneda
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import Process
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Task
import Utils.Form exposing (CustomFormError, errorForField, hasErrorField)
import Utils.Http exposing (viewHttpError)
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init shared.store route.params.grupoId
        , update = update shared.store
        , subscriptions = subscriptions
        , view = view shared.origin shared.currentUser shared.store
        }
        |> Page.withLayout (\_ -> Layouts.Default_Grupo {})


type alias Model =
    { grupoId : String
    , ajustesForm : Form CustomFormError UpdateGrupoParams
    }


type Msg
    = FreezeGrupo
    | FreezeGrupoResponse (Result Http.Error ShallowGrupo)
    | UnfreezeGrupo
    | UnfreezeGrupoResponse (Result Http.Error ShallowGrupo)
    | AjustesForm Form.Msg
    | UpdateGrupoResponse (Result Http.Error ShallowGrupo)
    | CheckIfGrupoIsPresent
    | ShareEmailAddress String


init : Store -> ULID -> ( Model, Effect Msg )
init store grupoId =
    ( { grupoId = grupoId
      , ajustesForm = Form.initial [] validateUpdateGrupoParams
      }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Store.ensureResumen grupoId store
        , Effect.getCurrentUser grupoId
        , Effect.setUnsavedChangesWarning False
        , waitAndCheckGrupo
        ]
    )


validateUpdateGrupoParams : Validation CustomFormError UpdateGrupoParams
validateUpdateGrupoParams =
    V.succeed UpdateGrupoParams
        |> V.andMap (V.field "nombre" (V.string |> V.andThen V.nonEmpty))
        |> V.andMap (V.field "moneda" Moneda.validate)


waitAndCheckGrupo : Effect Msg
waitAndCheckGrupo =
    Effect.sendCmd <| Task.perform (\_ -> CheckIfGrupoIsPresent) (Process.sleep 100)


seedAjustesForm : ShallowGrupo -> Form CustomFormError UpdateGrupoParams
seedAjustesForm grupo =
    Form.initial
        [ Form.setString "nombre" grupo.nombre
        , Form.setString "moneda" (Moneda.toString grupo.monedaPorDefecto)
        ]
        validateUpdateGrupoParams


update : Store -> Msg -> Model -> ( Model, Effect Msg )
update store msg model =
    case msg of
        FreezeGrupo ->
            ( model
            , Effect.sendCmd <|
                Api.postGrupoByIdFreeze
                    model.grupoId
                    FreezeGrupoResponse
            )

        FreezeGrupoResponse (Ok _) ->
            ( model
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshGrupo model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Grupo congelado"
                ]
            )

        FreezeGrupoResponse (Err _) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "No se pudo congelar el grupo"
            )

        UnfreezeGrupo ->
            ( model
            , Effect.sendCmd <|
                Api.deleteGrupoByIdFreeze
                    model.grupoId
                    UnfreezeGrupoResponse
            )

        UnfreezeGrupoResponse (Ok _) ->
            ( model
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshGrupo model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Grupo descongelado"
                ]
            )

        UnfreezeGrupoResponse (Err _) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "No se pudo descongelar el grupo"
            )

        AjustesForm Form.Submit ->
            case Form.getOutput model.ajustesForm of
                Just params ->
                    ( { model
                        | ajustesForm = Form.update validateUpdateGrupoParams Form.Submit model.ajustesForm
                      }
                    , Effect.sendCmd <|
                        Api.putGrupoById model.grupoId params UpdateGrupoResponse
                    )

                Nothing ->
                    ( { model
                        | ajustesForm = Form.update validateUpdateGrupoParams Form.Submit model.ajustesForm
                      }
                    , Effect.none
                    )

        AjustesForm formMsg ->
            ( { model
                | ajustesForm = Form.update validateUpdateGrupoParams formMsg model.ajustesForm
              }
            , Effect.none
            )

        UpdateGrupoResponse (Ok _) ->
            ( model
            , Effect.batch
                [ Store.refreshGrupo model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Grupo actualizado"
                ]
            )

        UpdateGrupoResponse (Err _) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "No se pudo actualizar el grupo"
            )

        CheckIfGrupoIsPresent ->
            case Store.getGrupo model.grupoId store of
                Success grupo ->
                    ( { model
                        | ajustesForm = seedAjustesForm grupo
                      }
                    , Effect.none
                    )

                NotAsked ->
                    ( model, waitAndCheckGrupo )

                Loading ->
                    ( model, waitAndCheckGrupo )

                Failure _ ->
                    ( model, Effect.none )

        ShareEmailAddress address ->
            ( model
            , Effect.share { title = "Cargar gastos por email", url = address }
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : String -> WebData User -> Store -> Model -> View Msg
view origin currentUser store model =
    case store |> Store.getGrupo model.grupoId of
        NotAsked ->
            { title = "Loading..."
            , body = []
            }

        Loading ->
            { title = "Cargando"
            , body =
                [ div [ class "container py-4 text-muted" ] [ text "Cargando..." ]
                ]
            }

        Failure e ->
            { title = "Fallo"
            , body =
                [ div [ class "container py-4" ] [ viewHttpError e ]
                ]
            }

        Success grupo ->
            { title = grupo.nombre ++ " - Configuración"
            , body =
                [ div [ class "container py-4" ]
                    [ div [ class "row justify-content-center" ]
                        [ div [ class "col-lg-8" ]
                            [ viewAjustesSection grupo model.ajustesForm
                            , viewEmailSection origin currentUser grupo
                            , viewFreezeSection grupo
                            ]
                        ]
                    ]
                ]
            }


viewAjustesSection : ShallowGrupo -> Form CustomFormError UpdateGrupoParams -> Html Msg
viewAjustesSection currentGrupo form =
    let
        dirty =
            Form.getOutput form /= Just { nombre = currentGrupo.nombre, monedaPorDefecto = currentGrupo.monedaPorDefecto }

        nombreField =
            Form.getFieldAsString "nombre" form

        monedaField =
            Form.getFieldAsString "moneda" form
    in
    Html.form
        [ onSubmit (AjustesForm Form.Submit)
        , class "card mb-4"
        ]
        [ div [ class "card-header" ] [ text "Ajustes generales" ]
        , div [ class "card-body" ]
            [ Html.map AjustesForm <|
                viewTextFormItem "Nombre" True nombreField
            , Html.map AjustesForm <|
                viewSelectFormItem "Moneda por defecto"
                    True
                    (Moneda.todas |> List.map (\m -> ( Moneda.toString m, Moneda.nombre m )))
                    monedaField
            , Bs.btn Bs.Primary
                [ disabled (not dirty || Form.getOutput form == Nothing)
                , onClick (AjustesForm Form.Submit)
                ]
                [ text "Guardar" ]
            ]
        ]


{-| The per-grupo inbound-email address. Sending an email here (from the
account's own address) creates a pago in this grupo. The email domain matches
the app's own domain, so we derive it from `origin`.
-}
grupoEmailAddress : String -> ULID -> String
grupoEmailAddress origin grupoId =
    "gasto+" ++ grupoId ++ "@" ++ emailDomain origin


emailDomain : String -> String
emailDomain origin =
    origin
        |> String.split "//"
        |> List.drop 1
        |> List.head
        |> Maybe.withDefault origin


viewEmailSection : String -> WebData User -> ShallowGrupo -> Html Msg
viewEmailSection origin currentUser grupo =
    let
        address =
            grupoEmailAddress origin grupo.id

        -- The webhook resolves the sender to a user account and then requires
        -- that user to own a participante in this grupo, so the address is only
        -- useful to someone logged in who has claimed their participante here.
        canUseEmail =
            case currentUser of
                Success user ->
                    ownedParticipante user.id grupo /= Nothing

                _ ->
                    False
    in
    div [ class "card mb-4" ]
        [ div [ class "card-header d-flex align-items-center gap-2" ]
            [ text "Cargar gastos por email"
            , if canUseEmail then
                text ""

              else
                span [ class "badge text-bg-secondary" ]
                    [ i [ class "bi bi-lock-fill me-1" ] [], text "Solo para miembros" ]
            ]
        , div [ class "card-body" ]
            [ div [ class "mb-3 text-muted" ]
                [ text "Reenviá a esta dirección los mails de tus compras —tickets, pedidos, confirmaciones de pago— o describí un gasto vos mismo, y lo cargamos como gasto automáticamente. Mandalo "
                , Html.strong [] [ text "desde la dirección de tu cuenta" ]
                , text "."
                ]
            , div
                [ class "input-group"
                , classList [ ( "opacity-50", not canUseEmail ) ]
                ]
                [ input
                    [ type_ "text"
                    , class "form-control"
                    , disabled True
                    , value address
                    ]
                    []
                , button
                    [ type_ "button"
                    , class "btn btn-outline-secondary"
                    , disabled (not canUseEmail)
                    , onClick (ShareEmailAddress address)
                    ]
                    [ text "Copiar" ]
                ]
            , if canUseEmail then
                text ""

              else
                div [ class "alert alert-secondary d-flex align-items-start gap-2 mt-3 mb-0" ]
                    [ i [ class "bi bi-lock-fill mt-1" ] []
                    , span []
                        (case currentUser of
                            Success _ ->
                                [ text "Reclamá tu participante en este grupo para poder cargar gastos por email." ]

                            _ ->
                                [ text "Cargá gastos sin abrir la app: "
                                , a [ Path.href Path.Login ] [ text "iniciá sesión o registrate" ]
                                , text " y reclamá tu participante en el grupo."
                                ]
                        )
                    ]
            ]
        ]


viewFreezeSection : ShallowGrupo -> Html Msg
viewFreezeSection grupo =
    div [ class "card" ]
        [ div [ class "card-header" ] [ text "Congelar grupo" ]
        , div [ class "card-body" ]
            [ div [ class "mb-3" ]
                [ text <|
                    if grupo.isFrozen then
                        "Este grupo está congelado. Las deudas están fijas y no se pueden agregar, editar ni eliminar pagos."

                    else
                        "Congelar el grupo fija las deudas actuales. No se podrán agregar, editar ni eliminar pagos mientras esté congelado."
                ]
            , viewFreezeButton grupo
            ]
        ]


viewFreezeButton : ShallowGrupo -> Html Msg
viewFreezeButton grupo =
    if grupo.isFrozen then
        button
            [ type_ "button"
            , class "btn btn-outline-secondary"
            , onClick UnfreezeGrupo
            ]
            [ text "Descongelar" ]

    else
        Bs.btn Bs.Primary
            [ onClick FreezeGrupo ]
            [ text "Congelar" ]


viewTextFormItem : String -> Bool -> Form.FieldState CustomFormError String -> Html Form.Msg
viewTextFormItem labelText isRequired field =
    div [ class "mb-3" ]
        [ label [ for field.path, class "form-label" ] [ text labelText ]
        , input
            [ type_ "text"
            , id field.path
            , class "form-control"
            , classList [ ( "is-invalid", hasErrorField field ) ]
            , value (Maybe.withDefault "" field.value)
            , onInput (\v -> Input field.path Form.Text (Form.Field.String v))
            , on "focus" (Json.Decode.succeed (Focus field.path))
            , on "blur" (Json.Decode.succeed (Blur field.path))
            , Attr.required isRequired
            ]
            []
        , if hasErrorField field then
            div [ class "invalid-feedback" ] [ errorForField field ]

          else
            text ""
        ]


viewSelectFormItem : String -> Bool -> List ( String, String ) -> Form.FieldState CustomFormError String -> Html Form.Msg
viewSelectFormItem labelText isRequired options field =
    div [ class "mb-3" ]
        [ label [ for field.path, class "form-label" ] [ text labelText ]
        , select
            [ id field.path
            , class "form-select"
            , classList [ ( "is-invalid", hasErrorField field ) ]
            , onInput (\v -> Input field.path Form.Select (Form.Field.String v))
            , on "focus" (Json.Decode.succeed (Focus field.path))
            , on "blur" (Json.Decode.succeed (Blur field.path))
            , Attr.required isRequired
            ]
            (options
                |> List.map
                    (\( val, labelStr ) ->
                        option
                            [ value val
                            , selected (field.value == Just val)
                            ]
                            [ text labelStr ]
                    )
            )
        , if hasErrorField field then
            div [ class "invalid-feedback" ] [ errorForField field ]

          else
            text ""
        ]
