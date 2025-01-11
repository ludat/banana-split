module Layouts.Default exposing (Model, Msg, Props, layout)

import Css
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Layout exposing (Layout)
import Route exposing (Route)
import Shared
import Utils.Toasts as Toasts
import Utils.Toasts.Types exposing (Toast, ToastLevel(..), ToastMsg, Toasts)
import View exposing (View)


type alias Props =
    { navBarContent : Maybe (Bool -> Html Msg)
    }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = \() -> init props
        , update = update
        , view = view props.navBarContent shared.toasties
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { navBarOpen : Bool
    }


init : Props -> ( Model, Effect Msg )
init props =
    ( { navBarOpen = False
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp
    | ToggleNavBar
    | ToastyMsg ToastMsg


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ToggleNavBar ->
            ( { model
                | navBarOpen = not model.navBarOpen
              }
            , Effect.none
            )

        NoOp ->
            ( model
            , Effect.none
            )

        ToastyMsg toastyMsg ->
            ( model
            , Effect.sendToastyMsg toastyMsg
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Maybe (Bool -> Html Msg) -> Toasts -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view navBarFunction toasties { toContentMsg, model, content } =
    { title =
        if content.title == "" then
            "Banana split"

        else
            "Banana Split | " ++ content.title
    , body =
        [ nav
            [ class "navbar"
            , attribute "role" "navigation"
            , attribute "aria-label" "main navigation"
            ]
          <|
            [ div [ class "navbar-brand" ]
                [ a [ class "navbar-item", href "/" ] [ text "🍌 Banana Split" ]
                , span
                    [ attribute "role" "button"
                    , class "navbar-burger"
                    , classList [ ( "is-active", model.navBarOpen ) ]
                    , attribute "aria-label" "menu"
                    , attribute "aria-expanded" "true"
                    , onClick <| toContentMsg ToggleNavBar
                    ]
                    [ span [ attribute "aria-hidden" "true" ] []
                    , span [ attribute "aria-hidden" "true" ] []
                    , span [ attribute "aria-hidden" "true" ] []
                    , span [ attribute "aria-hidden" "true" ] []
                    ]
                ]
            , case navBarFunction of
                Just navBarF ->
                    Html.map toContentMsg <| navBarF model.navBarOpen

                Nothing ->
                    text ""
            ]
        , div [ Css.toasts_container ]
            [ Html.map toContentMsg <|
                Toasts.view Toasts.config renderToast ToastyMsg toasties
            ]
        , div [] content.body
        ]
    }


renderToast : Toast -> Html Msg
renderToast toast =
    div [ Css.toast ]
        [ div
            [ class "notification"
            , case toast.level of
                ToastNoLevel ->
                    class ""

                ToastSuccess ->
                    class "is-success"

                ToastInfo ->
                    class "is-info"

                ToastWarning ->
                    class "is-warning"

                ToastDanger ->
                    class "is-danger"
            ]
            [ text toast.content ]
        ]
