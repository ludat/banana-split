module Utils.Toasts.Types exposing (Toast, ToastContent, ToastLevel(..), ToastMsg, Toasts)

import Toasty


type alias Toast =
    { level : ToastLevel
    , content : String
    }


type alias ToastContent =
    String


type ToastLevel
    = ToastSuccess
    | ToastDanger


type alias ToastMsg =
    Toasty.Msg Toast


type alias Toasts =
    Toasty.Stack Toast
