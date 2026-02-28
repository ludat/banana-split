module Shared.Model exposing (Model)

{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}

import Date exposing (Date)
import Generated.Api exposing (ULID)
import Models.Store.Types exposing (Store)
import Time
import Utils.Toasts.Types exposing (Toasts)


type alias Model =
    { toasties : Toasts
    , store : Store
    , userId : Maybe ULID
    , now : Time.Posix
    , zone : Time.Zone
    , lastReadChangelog : Maybe Date
    }
