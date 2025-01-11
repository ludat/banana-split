module Utils.Toasts exposing (..)

import Effect exposing (Effect)
import Html exposing (Html)
import Shared.Model
import Shared.Msg exposing (Msg(..))
import Toasty
import Utils.Toasts.Types exposing (Toast, ToastContent, ToastLevel, Toasts)


config : Toasty.Config msg
config =
    Toasty.config


initialState : Toasts
initialState =
    Toasty.initialState


pushToast : ToastLevel -> ToastContent -> Effect msg
pushToast level toastContent =
    Effect.sendToast
        { level = level
        , content = toastContent
        }


update :
    Toasty.Config msg
    -> (Toasty.Msg a -> msg)
    -> Toasty.Msg a
    -> { m | toasties : Toasty.Stack a }
    -> ( { m | toasties : Toasty.Stack a }, Cmd msg )
update =
    Toasty.update


addToast :
    Toasty.Config msg
    -> (Toasty.Msg a -> msg)
    -> a
    -> ( { m | toasties : Toasty.Stack a }, Cmd msg )
    -> ( { m | toasties : Toasty.Stack a }, Cmd msg )
addToast =
    Toasty.addToast


view :
    Toasty.Config msg
    -> (a -> Html msg)
    -> (Toasty.Msg a -> msg)
    -> Toasty.Stack a
    -> Html msg
view =
    Toasty.view
