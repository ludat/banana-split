module Layouts.Minimal exposing (Model, Msg, Props, layout)

import Components.Toasts
import Effect exposing (Effect)
import Html
import Html.Attributes exposing (class)
import Layout exposing (Layout)
import Route exposing (Route)
import Shared.Model as Shared
import Utils.Toasts.Types exposing (ToastMsg, Toasts)
import View exposing (View)


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout _ shared _ =
    Layout.new
        { init = \() -> init
        , update = update
        , view = view shared.toasties
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : ( Model, Effect Msg )
init =
    ( {}, Effect.none )



-- UPDATE


type Msg
    = ToastMsg ToastMsg


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ToastMsg toastMsg ->
            ( model
            , Effect.sendToastMsg toastMsg
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view :
    Toasts
    -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model }
    -> View contentMsg
view toasts { toContentMsg, content } =
    { title =
        if content.title == "" then
            "Banana split"

        else
            content.title
    , body =
        [ Html.map toContentMsg <|
            Components.Toasts.view ToastMsg toasts
        , Html.div [ class "container-fluid py-3" ] content.body
        ]
    }
