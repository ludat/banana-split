module Components.Toasts exposing (view)

import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Utils.Toasts as Toasts
import Utils.Toasts.Types exposing (Toast, ToastLevel(..), ToastMsg, Toasts)


{-| The fixed toast container, shared by every layout so toasts behave the same
regardless of which chrome (or none) the page renders. The caller wraps the
Toasty messages into its own Msg so it can forward them to Shared.
-}
view : (ToastMsg -> msg) -> Toasts -> Html msg
view toMsg toasts =
    div
        [ class "position-fixed bottom-0 start-50 translate-middle-x p-3"
        , Attr.style "z-index" "1090"
        , Attr.style "pointer-events" "none"
        ]
        [ Toasts.view Toasts.config renderToast toMsg toasts ]


renderToast : Toast -> Html msg
renderToast toast =
    let
        bgClass =
            case toast.level of
                ToastSuccess ->
                    "text-bg-success"

                ToastDanger ->
                    "text-bg-danger"
    in
    div
        [ class ("toast show align-items-center border-0 " ++ bgClass)
        , Attr.style "pointer-events" "auto"
        , Attr.attribute "role" "alert"
        , Attr.attribute "aria-live" "assertive"
        , Attr.attribute "aria-atomic" "true"
        ]
        [ div [ class "toast-body" ] [ text toast.content ] ]
