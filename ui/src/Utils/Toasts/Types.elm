module Utils.Toasts.Types exposing (..)

import Toasty


type alias Toast =
    { level : ToastLevel
    , content : String
    }


type alias ToastContent =
    String


type ToastLevel
    = ToastNoLevel
    | ToastSuccess
    | ToastInfo
    | ToastWarning
    | ToastDanger


type alias ToastMsg =
    Toasty.Msg Toast


type alias Toasts =
    Toasty.Stack Toast
