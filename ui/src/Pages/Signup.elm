module Pages.Signup exposing (Model, Msg, page)

import Components.Bootstrap as Bs
import Effect exposing (Effect)
import Form exposing (Form, Msg(..))
import Form.Field
import Form.Validate as V exposing (Validation)
import Generated.Api as Api exposing (LoginChallenge, SignupParams, User)
import Html exposing (Html, a, div, input, label, p, text)
import Html.Attributes as Attr exposing (class, classList, for, id, placeholder, type_)
import Html.Events exposing (on, onClick, onInput, onSubmit)
import Http
import Json.Decode
import Layouts
import Page exposing (Page)
import RemoteData
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



-- Signup is a two step flow: a new user provides their name + email (getting
-- back a signed challenge that commits to a code), then confirms with challenge
-- + code. The account is created only once the code is confirmed.


type Step
    = EnterDetails
    | EnterCode


type alias Model =
    { detailsForm : Form CustomFormError SignupParams
    , codeForm : Form CustomFormError String
    , challenge : String
    , step : Step
    , submitting : Bool
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { detailsForm = Form.initial [] validateDetails
      , codeForm = Form.initial [] validateCode
      , challenge = ""
      , step = EnterDetails
      , submitting = False
      }
    , Effect.none
    )


validateDetails : Validation CustomFormError SignupParams
validateDetails =
    V.succeed SignupParams
        |> V.andMap
            (V.field "nombre"
                (V.string
                    |> V.map String.trim
                    |> V.andThen V.nonEmpty
                )
            )
        |> V.andMap (V.field "email" (V.string |> V.andThen V.nonEmpty))


validateCode : Validation CustomFormError String
validateCode =
    V.field "code"
        (V.string
            |> V.map String.trim
            |> V.andThen V.nonEmpty
        )


type Msg
    = DetailsFormMsg Form.Msg
    | CodeFormMsg Form.Msg
    | GotChallenge (Result Http.Error LoginChallenge)
    | GotVerifyResult (Result Http.Error User)
    | BackToDetails


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        DetailsFormMsg Form.Submit ->
            let
                detailsForm =
                    Form.update validateDetails Form.Submit model.detailsForm
            in
            case Form.getOutput model.detailsForm of
                Just params ->
                    if model.submitting then
                        ( { model | detailsForm = detailsForm }, Effect.none )

                    else
                        ( { model | detailsForm = detailsForm, submitting = True }
                        , Effect.sendCmd (Api.postAuthSignup params GotChallenge)
                        )

                Nothing ->
                    ( { model | detailsForm = detailsForm }, Effect.none )

        DetailsFormMsg formMsg ->
            ( { model | detailsForm = Form.update validateDetails formMsg model.detailsForm }
            , Effect.none
            )

        CodeFormMsg Form.Submit ->
            let
                codeForm =
                    Form.update validateCode Form.Submit model.codeForm
            in
            case Form.getOutput model.codeForm of
                Just code ->
                    if model.submitting then
                        ( { model | codeForm = codeForm }, Effect.none )

                    else
                        ( { model | codeForm = codeForm, submitting = True }
                        , Effect.sendCmd
                            (Api.postAuthVerify
                                { challenge = model.challenge, code = code }
                                GotVerifyResult
                            )
                        )

                Nothing ->
                    ( { model | codeForm = codeForm }, Effect.none )

        CodeFormMsg formMsg ->
            ( { model | codeForm = Form.update validateCode formMsg model.codeForm }
            , Effect.none
            )

        GotChallenge (Ok { challenge }) ->
            ( { model | submitting = False, step = EnterCode, challenge = challenge }
            , Effect.sendToast { level = ToastSuccess, content = "Te enviamos un código para confirmar" }
            )

        GotChallenge (Err err) ->
            ( { model | submitting = False }
            , Effect.sendToast { level = ToastDanger, content = challengeError err }
            )

        GotVerifyResult (Ok user) ->
            ( { model | submitting = False }
            , Effect.batch
                [ Effect.sendSharedMsg (Shared.Msg.GotCurrentUser (RemoteData.Success user))
                , Effect.pushRoutePath Path.Home_
                ]
            )

        GotVerifyResult (Err err) ->
            ( { model | submitting = False }
            , Effect.sendToast { level = ToastDanger, content = verifyError err }
            )

        BackToDetails ->
            ( { model
                | step = EnterDetails
                , codeForm = Form.initial [] validateCode
                , challenge = ""
                , submitting = False
              }
            , Effect.none
            )


{-| Step 1 never checks whether the email is taken (that would leak account
existence before the caller proves they own the address), so the only error it
reports is the empty-name 400.
-}
challengeError : Http.Error -> String
challengeError err =
    case err of
        Http.BadStatus 400 ->
            "Ingresá tu nombre"

        _ ->
            "No pudimos enviar el código, probá de nuevo"


{-| That the email is already taken is only revealed at verify time, after the
code proves the caller owns it. The endpoint drops the response body on error,
so we key off the status.
-}
verifyError : Http.Error -> String
verifyError err =
    case err of
        Http.BadStatus 409 ->
            "Ya existe una cuenta con ese email. Iniciá sesión."

        _ ->
            "Código inválido o vencido"


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
    let
        nombreField =
            Form.getFieldAsString "nombre" model.detailsForm

        emailField =
            Form.getFieldAsString "email" model.detailsForm
    in
    div []
        [ Html.form [ onSubmit (DetailsFormMsg Form.Submit) ]
            [ Html.map DetailsFormMsg <|
                viewTextFormItem "Nombre"
                    [ placeholder "Juan Pérez" ]
                    nombreField
                    Nothing
            , Html.map DetailsFormMsg <|
                viewTextFormItem "Email"
                    [ placeholder "juan@ejemplo.com" ]
                    emailField
                    (Just "Te enviamos un código para confirmar tu email.")
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
    let
        codeField =
            Form.getFieldAsString "code" model.codeForm

        email =
            Form.getFieldAsString "email" model.detailsForm
                |> .value
                |> Maybe.withDefault ""
    in
    Html.form [ onSubmit (CodeFormMsg Form.Submit) ]
        [ Html.map CodeFormMsg <|
            viewTextFormItem "Código"
                [ placeholder "123456"
                , Attr.attribute "inputmode" "numeric"
                , Attr.attribute "autocomplete" "one-time-code"
                ]
                codeField
                (Just ("Enviado a " ++ email ++ ". Vence en 15 minutos."))
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


viewTextFormItem : String -> List (Html.Attribute Form.Msg) -> Form.FieldState CustomFormError String -> Maybe String -> Html Form.Msg
viewTextFormItem labelText extraAttrs field helpText =
    div [ class "mb-3" ]
        [ label [ for field.path, class "form-label" ] [ text labelText ]
        , input
            ([ type_ "text"
             , id field.path
             , class "form-control"
             , classList [ ( "is-invalid", hasErrorField field ) ]
             , Attr.value (Maybe.withDefault "" field.value)
             , onInput (\v -> Input field.path Form.Text (Form.Field.String v))
             , on "focus" (Json.Decode.succeed (Focus field.path))
             , on "blur" (Json.Decode.succeed (Blur field.path))
             ]
                ++ extraAttrs
            )
            []
        , if hasErrorField field then
            div [ class "invalid-feedback" ] [ errorForField field ]

          else
            text ""
        , case helpText of
            Just help ->
                div [ class "form-text" ] [ text help ]

            Nothing ->
                text ""
        ]
