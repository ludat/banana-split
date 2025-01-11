module Shared.Msg exposing (Msg(..))

{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}

import Models.Store.Types exposing (StoreMsg)
import Utils.Toasts.Types exposing (Toast, ToastMsg)


type Msg
    = NoOp
    | AddToast Toast
    | ToastyMsg ToastMsg
    | StoreMsg StoreMsg
