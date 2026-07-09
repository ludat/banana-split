module Pages.Login exposing (Model, Msg, Step, page)

import Components.Bootstrap as Bs
import Effect exposing (Effect)
import Generated.Api as Api exposing (LoginChallenge, User, VerifyResult(..))
import Html exposing (Html, div, form, label, p, text)
import Html.Attributes as Attr exposing (class, for, id, placeholder, type_)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Layouts
import Page exposing (Page)
import RemoteData
import Route exposing (Route)
import Route.Path as Path
import Shared
import Shared.Msg
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
    { email : String
    , code : String
    , nombre : String
    , challenge : String
    , registrationToken : String
    , step : Step
    , submitting : Bool
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { email = ""
      , code = ""
      , nombre = ""
      , challenge = ""
      , registrationToken = ""
      , step = EnterEmail
      , submitting = False
      }
    , Effect.none
    )


type Msg
    = EmailChanged String
    | CodeChanged String
    | NombreChanged String
    | SubmitEmail
    | GotChallenge (Result Http.Error LoginChallenge)
    | SubmitCode
    | GotVerifyResult (Result Http.Error VerifyResult)
    | SubmitRegister
    | GotRegistered (Result Http.Error User)
    | BackToEmail


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        EmailChanged email ->
            ( { model | email = email }, Effect.none )

        CodeChanged code ->
            ( { model | code = code }, Effect.none )

        NombreChanged nombre ->
            ( { model | nombre = nombre }, Effect.none )

        SubmitEmail ->
            if String.trim model.email == "" then
                ( model, Effect.none )

            else
                ( { model | submitting = True }
                , Effect.sendCmd (Api.postAuthRequestcode { email = model.email } GotChallenge)
                )

        GotChallenge (Ok { challenge }) ->
            ( { model | submitting = False, step = EnterCode, challenge = challenge }
            , Effect.sendToast { level = ToastSuccess, content = "Te enviamos un código para confirmar" }
            )

        GotChallenge (Err _) ->
            ( { model | submitting = False }
            , Effect.sendToast { level = ToastDanger, content = "No pudimos enviar el código, probá de nuevo" }
            )

        SubmitCode ->
            if String.trim model.code == "" then
                ( model, Effect.none )

            else
                ( { model | submitting = True }
                , Effect.sendCmd
                    (Api.postAuthVerify
                        { challenge = model.challenge, code = String.trim model.code }
                        GotVerifyResult
                    )
                )

        GotVerifyResult (Ok (VerifyLoggedIn user)) ->
            ( { model | submitting = False }, logIn user )

        GotVerifyResult (Ok (VerifyNeedsRegistration registrationToken)) ->
            ( { model | submitting = False, step = EnterName, registrationToken = registrationToken }
            , Effect.sendToast { level = ToastSuccess, content = "Es tu primera vez, elegí un nombre para tu cuenta" }
            )

        GotVerifyResult (Err err) ->
            ( { model | submitting = False }
            , Effect.sendToast { level = ToastDanger, content = verifyError err }
            )

        SubmitRegister ->
            if String.trim model.nombre == "" then
                ( model, Effect.none )

            else
                ( { model | submitting = True }
                , Effect.sendCmd
                    (Api.postAuthRegister
                        { registrationToken = model.registrationToken, nombre = String.trim model.nombre }
                        GotRegistered
                    )
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
                , code = ""
                , nombre = ""
                , challenge = ""
                , registrationToken = ""
                , submitting = False
              }
            , Effect.none
            )


logIn : User -> Effect Msg
logIn user =
    Effect.batch
        [ Effect.sendSharedMsg (Shared.Msg.GotCurrentUser (RemoteData.Success user))
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
    form [ onSubmit SubmitEmail ]
        [ div [ class "mb-3" ]
            [ label [ for "email", class "form-label" ] [ text "Email" ]
            , Html.input
                [ id "email"
                , type_ "text"
                , class "form-control"
                , placeholder "juan@ejemplo.com"
                , Attr.value model.email
                , onInput EmailChanged
                ]
                []
            , div [ class "form-text" ]
                [ text "Te enviamos un código para confirmar que sos vos." ]
            ]
        , Bs.btn Bs.Primary
            [ type_ "submit", Attr.disabled model.submitting ]
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
    form [ onSubmit SubmitCode ]
        [ div [ class "mb-3" ]
            [ label [ for "code", class "form-label" ] [ text "Código" ]
            , Html.input
                [ id "code"
                , type_ "text"
                , Attr.attribute "inputmode" "numeric"
                , Attr.attribute "autocomplete" "one-time-code"
                , class "form-control"
                , placeholder "123456"
                , Attr.value model.code
                , onInput CodeChanged
                ]
                []
            , div [ class "form-text" ]
                [ text ("Enviado a " ++ model.email ++ ". Vence en 5 minutos.") ]
            ]
        , div [ class "d-flex gap-2" ]
            [ Bs.btn Bs.Primary
                [ type_ "submit", Attr.disabled model.submitting ]
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
    form [ onSubmit SubmitRegister ]
        [ p [ class "mb-3" ]
            [ text ("Confirmamos " ++ model.email ++ ". Elegí un nombre para tu cuenta.") ]
        , div [ class "mb-3" ]
            [ label [ for "nombre", class "form-label" ] [ text "Nombre" ]
            , Html.input
                [ id "nombre"
                , type_ "text"
                , class "form-control"
                , placeholder "Juan"
                , Attr.value model.nombre
                , onInput NombreChanged
                ]
                []
            ]
        , div [ class "d-flex gap-2" ]
            [ Bs.btn Bs.Primary
                [ type_ "submit", Attr.disabled model.submitting ]
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
