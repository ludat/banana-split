module Pages.Login exposing (Model, Msg, Step, page)

import Components.Bootstrap as Bs
import Effect exposing (Effect)
import Form exposing (Form, Msg(..))
import Form.Field
import Form.Validate as V exposing (Validation)
import Generated.Api as Api exposing (LoginChallenge, RequestCodeParams, User, VerifyResult(..))
import Html exposing (Attribute, Html, div, input, label, p, text)
import Html.Attributes as Attr exposing (class, classList, disabled, for, id, placeholder, type_)
import Html.Events exposing (on, onClick, onInput, onSubmit)
import Http
import Json.Decode
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Shared.Msg
import Utils.Form exposing (CustomFormError, errorForField, hasErrorField)
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
        |> Page.withLayout (\_ -> Layouts.Minimal {})



-- A single email-first flow: the user asks for a code by email (getting back a
-- signed challenge that commits to it), proves ownership by entering the code,
-- and then either lands logged in (existing account) or is walked through the
-- name step to create a new one. Which of the two only becomes known once the
-- code is confirmed, so the register step is reached in-place, never up front.


type Step
    = EnterEmail
    | EnterCode
    | EnterName


type alias Model =
    { loginForm : Form CustomFormError RequestCodeParams
    , confirmationForm : Form CustomFormError String
    , registrationForm : Form CustomFormError String
    , email : String
    , challenge : String
    , registrationToken : String
    , step : Step
    , submitting : Bool
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { loginForm = Form.initial [] validateLogin
      , confirmationForm = Form.initial [] validateConfirmation
      , registrationForm = Form.initial [] validateRegistration
      , email = ""
      , challenge = ""
      , registrationToken = ""
      , step = EnterEmail
      , submitting = False
      }
    , Effect.none
    )


validateLogin : Validation CustomFormError RequestCodeParams
validateLogin =
    V.succeed RequestCodeParams
        |> V.andMap (V.field "email" trimmedNonEmpty)


validateConfirmation : Validation CustomFormError String
validateConfirmation =
    V.field "code" trimmedNonEmpty


validateRegistration : Validation CustomFormError String
validateRegistration =
    V.field "nombre" trimmedNonEmpty


trimmedNonEmpty : Validation CustomFormError String
trimmedNonEmpty =
    V.string
        |> V.map String.trim
        |> V.andThen V.nonEmpty


type Msg
    = LoginFormMsg Form.Msg
    | ConfirmationFormMsg Form.Msg
    | RegistrationFormMsg Form.Msg
    | GotChallenge (Result Http.Error LoginChallenge)
    | GotVerifyResult (Result Http.Error VerifyResult)
    | GotRegistered (Result Http.Error User)
    | BackToEmail


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        LoginFormMsg Form.Submit ->
            let
                loginForm =
                    Form.update validateLogin Form.Submit model.loginForm
            in
            case Form.getOutput model.loginForm of
                Just params ->
                    if model.submitting then
                        ( { model | loginForm = loginForm }, Effect.none )

                    else
                        ( { model | loginForm = loginForm, email = params.email, submitting = True }
                        , Effect.sendCmd (Api.postAuthRequestcode params GotChallenge)
                        )

                Nothing ->
                    ( { model | loginForm = loginForm }, Effect.none )

        LoginFormMsg formMsg ->
            ( { model | loginForm = Form.update validateLogin formMsg model.loginForm }
            , Effect.none
            )

        ConfirmationFormMsg Form.Submit ->
            let
                confirmationForm =
                    Form.update validateConfirmation Form.Submit model.confirmationForm
            in
            case Form.getOutput model.confirmationForm of
                Just code ->
                    if model.submitting then
                        ( { model | confirmationForm = confirmationForm }, Effect.none )

                    else
                        ( { model | confirmationForm = confirmationForm, submitting = True }
                        , Effect.sendCmd
                            (Api.postAuthVerify
                                { challenge = model.challenge, code = code }
                                GotVerifyResult
                            )
                        )

                Nothing ->
                    ( { model | confirmationForm = confirmationForm }, Effect.none )

        ConfirmationFormMsg formMsg ->
            ( { model | confirmationForm = Form.update validateConfirmation formMsg model.confirmationForm }
            , Effect.none
            )

        RegistrationFormMsg Form.Submit ->
            let
                registrationForm =
                    Form.update validateRegistration Form.Submit model.registrationForm
            in
            case Form.getOutput model.registrationForm of
                Just nombre ->
                    if model.submitting then
                        ( { model | registrationForm = registrationForm }, Effect.none )

                    else
                        ( { model | registrationForm = registrationForm, submitting = True }
                        , Effect.sendCmd
                            (Api.postAuthRegister
                                { registrationToken = model.registrationToken, nombre = nombre }
                                GotRegistered
                            )
                        )

                Nothing ->
                    ( { model | registrationForm = registrationForm }, Effect.none )

        RegistrationFormMsg formMsg ->
            ( { model | registrationForm = Form.update validateRegistration formMsg model.registrationForm }
            , Effect.none
            )

        GotChallenge (Ok { challenge }) ->
            ( { model
                | submitting = False
                , step = EnterCode
                , challenge = challenge
                , confirmationForm = Form.initial [] validateConfirmation
              }
            , Effect.sendToast { level = ToastSuccess, content = "Te enviamos un código para confirmar" }
            )

        GotChallenge (Err _) ->
            ( { model | submitting = False }
            , Effect.sendToast { level = ToastDanger, content = "No pudimos enviar el código, probá de nuevo" }
            )

        GotVerifyResult (Ok (VerifyLoggedIn user)) ->
            ( { model | submitting = False }, logIn user )

        GotVerifyResult (Ok (VerifyNeedsRegistration registrationToken)) ->
            ( { model
                | submitting = False
                , step = EnterName
                , registrationToken = registrationToken
                , registrationForm = Form.initial [] validateRegistration
              }
            , Effect.sendToast { level = ToastSuccess, content = "Es tu primera vez, elegí un nombre para tu cuenta" }
            )

        GotVerifyResult (Err err) ->
            ( { model | submitting = False }
            , Effect.sendToast { level = ToastDanger, content = verifyError err }
            )

        GotRegistered (Ok user) ->
            ( { model | submitting = False }, logIn user )

        GotRegistered (Err err) ->
            ( { model | submitting = False }
            , Effect.sendToast { level = ToastDanger, content = registerError err }
            )

        BackToEmail ->
            ( { model
                | step = EnterEmail
                , challenge = ""
                , registrationToken = ""
                , submitting = False
                , confirmationForm = Form.initial [] validateConfirmation
                , registrationForm = Form.initial [] validateRegistration
              }
            , Effect.none
            )


logIn : User -> Effect Msg
logIn user =
    Effect.batch
        [ Effect.sendSharedMsg (Shared.Msg.CurrentUserLoaded (Ok user))
        , Effect.pushRoutePath Path.Home_
        ]


{-| Verify drops its response body on error, so we key off the status. A new
email no longer errors here — it comes back as a 200 'VerifyNeedsRegistration'.
-}
verifyError : Http.Error -> String
verifyError err =
    case err of
        Http.BadStatus 429 ->
            "Demasiados intentos. Esperá unos minutos y volvé a intentar."

        _ ->
            "Código inválido o vencido"


registerError : Http.Error -> String
registerError err =
    case err of
        Http.BadStatus 409 ->
            "Ya existe una cuenta con ese email. Iniciá sesión."

        Http.BadStatus 401 ->
            "Tu registro venció, volvé a empezar."

        _ ->
            "No pudimos crear tu cuenta, probá de nuevo"


view : Model -> View Msg
view model =
    { title =
        case model.step of
            EnterName ->
                "Crear cuenta"

            _ ->
                "Iniciar sesión"
    , body =
        [ div [ class "container py-4" ]
            [ div [ class "row justify-content-center" ]
                [ div [ class "col-12 col-md-6 col-lg-4" ]
                    [ Bs.card []
                        [ Bs.cardHeader []
                            [ text
                                (case model.step of
                                    EnterName ->
                                        "Crear cuenta"

                                    _ ->
                                        "Iniciar sesión"
                                )
                            ]
                        , Bs.cardBody []
                            [ case model.step of
                                EnterEmail ->
                                    viewEmailStep model

                                EnterCode ->
                                    viewCodeStep model

                                EnterName ->
                                    viewNameStep model
                            ]
                        ]
                    ]
                ]
            ]
        ]
    }


viewEmailStep : Model -> Html Msg
viewEmailStep model =
    Html.form [ onSubmit (LoginFormMsg Form.Submit) ]
        [ Html.map LoginFormMsg <|
            viewTextField
                { label = "Email"
                , placeholder = "juan@ejemplo.com"
                , help = Just "Te enviamos un código para confirmar que sos vos."
                , inputAttrs = []
                }
                (Form.getFieldAsString "email" model.loginForm)
        , Bs.btn Bs.Primary
            [ type_ "submit", disabled model.submitting ]
            [ text
                (if model.submitting then
                    "Enviando…"

                 else
                    "Enviar código"
                )
            ]
        ]


viewCodeStep : Model -> Html Msg
viewCodeStep model =
    Html.form [ onSubmit (ConfirmationFormMsg Form.Submit) ]
        [ Html.map ConfirmationFormMsg <|
            viewTextField
                { label = "Código"
                , placeholder = "123456"
                , help = Just ("Enviado a " ++ model.email ++ ". Vence en 5 minutos.")
                , inputAttrs =
                    [ Attr.attribute "inputmode" "numeric"
                    , Attr.attribute "autocomplete" "one-time-code"
                    ]
                }
                (Form.getFieldAsString "code" model.confirmationForm)
        , div [ class "d-flex gap-2" ]
            [ Bs.btn Bs.Primary
                [ type_ "submit", disabled model.submitting ]
                [ text
                    (if model.submitting then
                        "Confirmando…"

                     else
                        "Confirmar"
                    )
                ]
            , Bs.btn Bs.Transparent
                [ type_ "button", onClick BackToEmail ]
                [ text "Cambiar email" ]
            ]
        ]


viewNameStep : Model -> Html Msg
viewNameStep model =
    Html.form [ onSubmit (RegistrationFormMsg Form.Submit) ]
        [ p [ class "mb-3" ]
            [ text ("Confirmamos " ++ model.email ++ ". Elegí un nombre para tu cuenta.") ]
        , Html.map RegistrationFormMsg <|
            viewTextField
                { label = "Nombre"
                , placeholder = "Juan"
                , help = Nothing
                , inputAttrs = []
                }
                (Form.getFieldAsString "nombre" model.registrationForm)
        , div [ class "d-flex gap-2" ]
            [ Bs.btn Bs.Primary
                [ type_ "submit", disabled model.submitting ]
                [ text
                    (if model.submitting then
                        "Creando…"

                     else
                        "Crear cuenta"
                    )
                ]
            , Bs.btn Bs.Transparent
                [ type_ "button", onClick BackToEmail ]
                [ text "Cambiar email" ]
            ]
        ]


viewTextField :
    { label : String
    , placeholder : String
    , help : Maybe String
    , inputAttrs : List (Attribute Form.Msg)
    }
    -> Form.FieldState CustomFormError String
    -> Html Form.Msg
viewTextField config field =
    div [ class "mb-3" ]
        [ label [ for field.path, class "form-label" ] [ text config.label ]
        , input
            ([ type_ "text"
             , id field.path
             , class "form-control"
             , placeholder config.placeholder
             , classList [ ( "is-invalid", hasErrorField field ) ]
             , Attr.value (Maybe.withDefault "" field.value)
             , onInput (\v -> Input field.path Form.Text (Form.Field.String v))
             , on "focus" (Json.Decode.succeed (Focus field.path))
             , on "blur" (Json.Decode.succeed (Blur field.path))
             ]
                ++ config.inputAttrs
            )
            []
        , case config.help of
            Just helpText ->
                div [ class "form-text" ] [ text helpText ]

            Nothing ->
                text ""
        , if hasErrorField field then
            div [ class "invalid-feedback" ] [ errorForField field ]

          else
            text ""
        ]
