module Layouts.Default exposing (Model, Msg, Props, layout)

import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Layout exposing (Layout)
import Route exposing (Route)
import Shared
import Svg
import Svg.Attributes as SvgAttr
import View exposing (View)


type alias Props =
    { navBarContent : Maybe (Bool -> Html Msg)
    }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = \() -> init props
        , update = update
        , view = view props.navBarContent
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Maybe (Bool -> Html Msg) -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view navBarFunction { toContentMsg, model, content } =
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
        , div [] content.body
        ]
    }
