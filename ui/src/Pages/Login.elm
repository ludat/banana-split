module Pages.Login exposing (Model, Msg, page)

import Components.Bootstrap as Bs
import Effect exposing (Effect)
import Generated.Api as Api exposing (LoginChallenge, User)
import Html exposing (Html, div, form, label, text)
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



-- Login is a two step flow: request a code by email (getting back a signed
-- challenge that commits to it), then confirm with challenge + code.


type Step
    = EnterEmail
    | EnterCode


type alias Model =
    { email : String
    , code : String
    , challenge : String
    , step : Step
    , submitting : Bool
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { email = "", code = "", challenge = "", step = EnterEmail, submitting = False }
    , Effect.none
    )


type Msg
    = EmailChanged String
    | CodeChanged String
    | SubmitEmail
    | GotChallenge (Result Http.Error LoginChallenge)
    | SubmitCode
    | GotVerifyResult (Result Http.Error User)
    | BackToEmail


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        EmailChanged email ->
            ( { model | email = email }, Effect.none )

        CodeChanged code ->
            ( { model | code = code }, Effect.none )

        SubmitEmail ->
            if String.trim model.email == "" then
                ( model, Effect.none )

            else
                ( { model | submitting = True }
                , Effect.sendCmd (Api.postAuthLogin { email = model.email } GotChallenge)
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

        BackToEmail ->
            ( { model | step = EnterEmail, code = "", challenge = "", submitting = False }
            , Effect.none
            )


view : Model -> View Msg
view model =
    { title = "Iniciar sesión"
    , body =
        [ div [ class "container py-4" ]
            [ div [ class "row justify-content-center" ]
                [ div [ class "col-12 col-md-6 col-lg-4" ]
                    [ Bs.card []
                        [ Bs.cardHeader [] [ text "Iniciar sesión" ]
                        , Bs.cardBody []
                            [ case model.step of
                                EnterEmail ->
                                    viewEmailStep model

                                EnterCode ->
                                    viewCodeStep model
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
                [ text "Te enviamos un código. Si no existe, creamos la cuenta automáticamente." ]
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
                [ type_ "button", onClick BackToEmail ]
                [ text "Cambiar email" ]
            ]
        ]
