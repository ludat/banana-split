module Pages.Cuenta exposing (Model, Msg, page)

import Components.Bootstrap as Bs
import Dict
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
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Shared.Msg
import Utils.Form exposing (CustomFormError, errorForField, hasErrorField)
import Utils.Telemetry as Telemetry
import Utils.Toasts.Types exposing (ToastLevel(..))
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }
        |> Page.withLayout (\_ -> Layouts.Default {})


type alias Model =
    { form : Form CustomFormError UpdateMeParams
    , saving : Bool
    , user : WebData User
    }


init : () -> ( Model, Effect Msg )
init () =
    -- Not shared.currentUser: at startup that one comes from the session token,
    -- which has no datosDeCobro and may be stale, and this form edits both.
    ( { form = Form.initial [] validateUpdateMeParams, saving = False, user = Loading }
    , Effect.sendCmd (Api.getMe (RemoteData.fromResult >> GotUser))
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
        |> V.andMap (V.field "datosDeCobro" (V.maybe V.string))


seedForm : User -> Form CustomFormError UpdateMeParams
seedForm user =
    Form.initial
        [ Form.setString "nombre" user.nombre
        , Form.setString "datosDeCobro" (Maybe.withDefault "" user.datosDeCobro)
        ]
        validateUpdateMeParams


type Msg
    = FormMsg Form.Msg
    | Saved (Result Http.Error User)
    | GotUser (WebData User)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
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
            ( { model | saving = False, form = seedForm user, user = Success user }
            , Effect.batch
                [ Effect.sendSharedMsg (Shared.Msg.CurrentUserLoaded (Ok user))
                , Effect.sendToast { level = ToastSuccess, content = "Guardamos tus cambios" }
                ]
            )

        Saved (Err _) ->
            ( { model | saving = False }
            , Effect.sendToast { level = ToastDanger, content = "No pudimos guardar los cambios" }
            )

        GotUser (Success user) ->
            ( { model | form = seedForm user, user = Success user }
            , Effect.none
            )

        GotUser user ->
            ( { model | user = user }
            , Effect.none
            )


view : Model -> View Msg
view model =
    { title = "Mi cuenta"
    , body =
        [ div [ class "container py-4" ]
            [ div [ class "row justify-content-center" ]
                [ div [ class "col-12 col-md-8 col-lg-6" ]
                    [ Bs.card []
                        [ Bs.cardHeader [] [ text "Mi cuenta" ]
                        , Bs.cardBody [] [ viewBody model.user model ]
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
                , Html.a
                    [ Route.href
                        { path = Path.Login
                        , query = Dict.singleton "redirect" (Path.toString Path.Cuenta)
                        , hash = Nothing
                        }
                    ]
                    [ text "Iniciar sesión" ]
                ]


viewForm : User -> Model -> Html Msg
viewForm user model =
    let
        nombreField =
            Form.getFieldAsString "nombre" model.form
    in
    Html.form [ id "cuenta-form", onSubmit (FormMsg Form.Submit) ]
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
        , Html.map FormMsg <|
            viewDatosDeCobro (Form.getFieldAsString "datosDeCobro" model.form)
        , Bs.btn Bs.Primary
            [ type_ "submit", Telemetry.click "cuenta.save", disabled model.saving ]
            [ text
                (if model.saving then
                    "Guardando…"

                 else
                    "Guardar"
                )
            ]
        ]


viewDatosDeCobro : Form.FieldState CustomFormError String -> Html Form.Msg
viewDatosDeCobro field =
    div [ class "mb-3" ]
        [ label [ for field.path, class "form-label" ] [ text "¿Cómo querés recibir la plata?" ]
        , input
            [ type_ "text"
            , id field.path
            , class "form-control"
            , Attr.placeholder "Alias: juan.mp, CBU..., en efectivo"
            , Attr.value (Maybe.withDefault "" field.value)
            , onInput (\v -> Input field.path Form.Text (Form.Field.String v))
            , on "focus" (Json.Decode.succeed (Focus field.path))
            , on "blur" (Json.Decode.succeed (Blur field.path))
            ]
            []
        , div [ class "form-text" ]
            [ text "Se muestra en todos los grupos donde te reclamaste, salvo que en alguno cargues otros datos para ese participante." ]
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
