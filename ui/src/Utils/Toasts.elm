module Utils.Toasts exposing (addToast, config, initialState, pushToast, update, view)

import Effect exposing (Effect)
import Html exposing (Html)
import Html.Attributes exposing (class)
import Toasty
import Utils.Toasts.Types exposing (ToastContent, ToastLevel, Toasts)


config : Toasty.Config msg
config =
    Toasty.config
        |> Toasty.containerAttrs [ class "list-unstyled p-0 m-0" ]
        |> Toasty.itemAttrs [ class "mb-2" ]


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
