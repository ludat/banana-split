module Shared.Model exposing (Model)

{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}

import Models.Store.Types exposing (Store)
import Utils.Toasts.Types exposing (Toast, Toasts)


type alias Model =
    { toasties : Toasts
    , store : Store
    }
