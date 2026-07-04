module Pages.Signup exposing (Model, Msg, page)

import Components.Bootstrap as Bs
import Effect exposing (Effect)
import Generated.Api as Api exposing (LoginChallenge, User)
import Html exposing (Html, a, div, form, label, p, text)
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



-- Signup is a two step flow: a new user provides their name + email (getting
-- back a signed challenge that commits to a code), then confirms with challenge
-- + code. The account is created only once the code is confirmed.


type Step
    = EnterDetails
    | EnterCode


type alias Model =
    { nombre : String
    , email : String
    , code : String
    , challenge : String
    , step : Step
    , submitting : Bool
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { nombre = "", email = "", code = "", challenge = "", step = EnterDetails, submitting = False }
    , Effect.none
    )


type Msg
    = NombreChanged String
    | EmailChanged String
    | CodeChanged String
    | SubmitDetails
    | GotChallenge (Result Http.Error LoginChallenge)
    | SubmitCode
    | GotVerifyResult (Result Http.Error User)
    | BackToDetails


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NombreChanged nombre ->
            ( { model | nombre = nombre }, Effect.none )

        EmailChanged email ->
            ( { model | email = email }, Effect.none )

        CodeChanged code ->
            ( { model | code = code }, Effect.none )

        SubmitDetails ->
            if String.trim model.nombre == "" || String.trim model.email == "" then
                ( model, Effect.none )

            else
                ( { model | submitting = True }
                , Effect.sendCmd
                    (Api.postAuthSignup
                        { nombre = String.trim model.nombre, email = model.email }
                        GotChallenge
                    )
                )

        GotChallenge (Ok { challenge }) ->
            ( { model | submitting = False, step = EnterCode, challenge = challenge }
            , Effect.sendToast { level = ToastSuccess, content = "Te enviamos un código para confirmar" }
            )

        GotChallenge (Err err) ->
            ( { model | submitting = False }
            , Effect.sendToast { level = ToastDanger, content = signupError err }
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

        GotVerifyResult (Ok user) ->
            ( { model | submitting = False }
            , Effect.batch
                [ Effect.sendSharedMsg (Shared.Msg.GotCurrentUser (RemoteData.Success user))
                , Effect.pushRoutePath Path.Home_
                ]
            )

        GotVerifyResult (Err _) ->
            ( { model | submitting = False }
            , Effect.sendToast { level = ToastDanger, content = "Código inválido o vencido" }
            )

        BackToDetails ->
            ( { model | step = EnterDetails, code = "", challenge = "", submitting = False }
            , Effect.none
            )


{-| The signup step-1 endpoint drops the response body on error, so we map the
status code back to the message the backend would have shown.
-}
signupError : Http.Error -> String
signupError err =
    case err of
        Http.BadStatus 409 ->
            "Ya existe una cuenta con ese email. Iniciá sesión."

        Http.BadStatus 400 ->
            "Ingresá tu nombre"

        _ ->
            "No pudimos enviar el código, probá de nuevo"


view : Model -> View Msg
view model =
    { title = "Crear cuenta"
    , body =
        [ div [ class "container py-4" ]
            [ div [ class "row justify-content-center" ]
                [ div [ class "col-12 col-md-6 col-lg-4" ]
                    [ Bs.card []
                        [ Bs.cardHeader [] [ text "Crear cuenta" ]
                        , Bs.cardBody []
                            [ case model.step of
                                EnterDetails ->
                                    viewDetailsStep model

                                EnterCode ->
                                    viewCodeStep model
                            ]
                        ]
                    ]
                ]
            ]
        ]
    }


viewDetailsStep : Model -> Html Msg
viewDetailsStep model =
    div []
        [ form [ onSubmit SubmitDetails ]
            [ div [ class "mb-3" ]
                [ label [ for "nombre", class "form-label" ] [ text "Nombre" ]
                , Html.input
                    [ id "nombre"
                    , type_ "text"
                    , class "form-control"
                    , placeholder "Juan Pérez"
                    , Attr.value model.nombre
                    , onInput NombreChanged
                    ]
                    []
                ]
            , div [ class "mb-3" ]
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
                    [ text "Te enviamos un código para confirmar tu email." ]
                ]
            , Bs.btn Bs.Primary
                [ type_ "submit", Attr.disabled model.submitting ]
                [ text
                    (if model.submitting then
                        "Enviando…"

                     else
                        "Crear cuenta"
                    )
                ]
            ]
        , p [ class "mt-3 mb-0 text-center" ]
            [ text "¿Ya tenés cuenta? "
            , a [ Path.href Path.Login ] [ text "Iniciá sesión" ]
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
                [ text ("Enviado a " ++ model.email ++ ". Vence en 15 minutos.") ]
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
                [ type_ "button", onClick BackToDetails ]
                [ text "Cambiar datos" ]
            ]
        ]
