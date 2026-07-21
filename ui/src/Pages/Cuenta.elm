module Pages.Cuenta exposing (Model, Msg, page)

import Components.Bootstrap as Bs
import Effect exposing (Effect)
import Form exposing (Form, Msg(..))
import Form.Field
import Form.Init as Form
import Form.Validate as V exposing (Validation)
import Generated.Api as Api exposing (UpdateMeParams, User)
import Html exposing (Html, div, input, label, p, text)
import Html.Attributes as Attr exposing (class, classList, disabled, for, id, type_)
import Html.Events exposing (on, onInput, onSubmit)
import Http
import Json.Decode
import Layouts
import Page exposing (Page)
import Process
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Shared.Msg
import Task
import Utils.Form exposing (CustomFormError, errorForField, hasErrorField)
import Utils.Toasts.Types exposing (ToastLevel(..))
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init shared
        , update = update shared
        , subscriptions = \_ -> Sub.none
        , view = view shared
        }
        |> Page.withLayout (\_ -> Layouts.Default {})


type alias Model =
    { form : Form CustomFormError UpdateMeParams
    , saving : Bool
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    -- The current user resolves asynchronously into shared.currentUser, so if
    -- it isn't there yet we poll until it lands and then seed the form.
    case shared.currentUser of
        Success user ->
            ( { form = seedForm user, saving = False }
            , Effect.none
            )

        _ ->
            ( { form = Form.initial [] validateUpdateMeParams, saving = False }
            , waitAndCheckUser
            )


validateUpdateMeParams : Validation CustomFormError UpdateMeParams
validateUpdateMeParams =
    V.succeed UpdateMeParams
        |> V.andMap
            (V.field "nombre"
                (V.string
                    |> V.map String.trim
                    |> V.andThen V.nonEmpty
                )
            )


seedForm : User -> Form CustomFormError UpdateMeParams
seedForm user =
    Form.initial
        [ Form.setString "nombre" user.nombre ]
        validateUpdateMeParams


waitAndCheckUser : Effect Msg
waitAndCheckUser =
    Effect.sendCmd <| Task.perform (\_ -> CheckIfUserIsPresent) (Process.sleep 100)


type Msg
    = FormMsg Form.Msg
    | Saved (Result Http.Error User)
    | CheckIfUserIsPresent


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    case msg of
        FormMsg Form.Submit ->
            let
                form =
                    Form.update validateUpdateMeParams Form.Submit model.form
            in
            case Form.getOutput model.form of
                Just params ->
                    if model.saving then
                        ( { model | form = form }, Effect.none )

                    else
                        ( { model | form = form, saving = True }
                        , Effect.sendCmd (Api.putMe params Saved)
                        )

                Nothing ->
                    ( { model | form = form }, Effect.none )

        FormMsg formMsg ->
            ( { model | form = Form.update validateUpdateMeParams formMsg model.form }
            , Effect.none
            )

        Saved (Ok user) ->
            ( { model | saving = False, form = seedForm user }
            , Effect.batch
                [ Effect.sendSharedMsg (Shared.Msg.CurrentUserLoaded (Ok user))
                , Effect.sendToast { level = ToastSuccess, content = "Guardamos tus cambios" }
                ]
            )

        Saved (Err _) ->
            ( { model | saving = False }
            , Effect.sendToast { level = ToastDanger, content = "No pudimos guardar los cambios" }
            )

        CheckIfUserIsPresent ->
            case shared.currentUser of
                Success user ->
                    ( { model | form = seedForm user }
                    , Effect.none
                    )

                NotAsked ->
                    ( model, waitAndCheckUser )

                Loading ->
                    ( model, waitAndCheckUser )

                Failure _ ->
                    ( model, Effect.none )


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Mi cuenta"
    , body =
        [ div [ class "container py-4" ]
            [ div [ class "row justify-content-center" ]
                [ div [ class "col-12 col-md-8 col-lg-6" ]
                    [ Bs.card []
                        [ Bs.cardHeader [] [ text "Mi cuenta" ]
                        , Bs.cardBody [] [ viewBody shared.currentUser model ]
                        ]
                    ]
                ]
            ]
        ]
    }


viewBody : WebData User -> Model -> Html Msg
viewBody currentUser model =
    case currentUser of
        Success user ->
            viewForm user model

        Loading ->
            p [ class "text-secondary mb-0" ] [ text "Cargando…" ]

        _ ->
            p [ class "mb-0" ]
                [ text "Necesitás iniciar sesión para ver tu cuenta. "
                , Html.a [ Path.href Path.Login ] [ text "Iniciar sesión" ]
                ]


viewForm : User -> Model -> Html Msg
viewForm user model =
    let
        nombreField =
            Form.getFieldAsString "nombre" model.form
    in
    Html.form [ onSubmit (FormMsg Form.Submit) ]
        [ div [ class "mb-3" ]
            [ label [ for "email", class "form-label" ] [ text "Email" ]
            , input
                [ id "email"
                , type_ "text"
                , class "form-control"
                , Attr.value user.email
                , disabled True
                ]
                []
            , div [ class "form-text" ]
                [ text "El email es tu identidad de acceso y no se puede cambiar por ahora." ]
            ]
        , Html.map FormMsg <|
            viewTextFormItem "Nombre" True nombreField
        , Bs.btn Bs.Primary
            [ type_ "submit", disabled model.saving ]
            [ text
                (if model.saving then
                    "Guardando…"

                 else
                    "Guardar"
                )
            ]
        ]


viewTextFormItem : String -> Bool -> Form.FieldState CustomFormError String -> Html Form.Msg
viewTextFormItem labelText isRequired field =
    div [ class "mb-3" ]
        [ label [ for field.path, class "form-label" ] [ text labelText ]
        , input
            [ type_ "text"
            , id field.path
            , class "form-control"
            , classList [ ( "is-invalid", hasErrorField field ) ]
            , Attr.value (Maybe.withDefault "" field.value)
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
