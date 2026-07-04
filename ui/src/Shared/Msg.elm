module Shared.Msg exposing (Msg(..))

{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}

import Generated.Api exposing (ULID, User)
import Models.Store.Types exposing (StoreMsg)
import RemoteData exposing (WebData)
import Route.Path as Route
import Time exposing (Posix)
import Utils.Toasts.Types exposing (Toast, ToastMsg)


type Msg
    = NoOp
    | AddToast Toast
    | ToastMsg ToastMsg
    | StoreMsg StoreMsg
    | NavigateTo Route.Path
    | SetCurrentUser { grupoId : ULID, userId : ULID }
    | CurrentUserLoaded { grupoId : ULID, userId : Maybe ULID }
    | GotCurrentUser (WebData User)
    | Logout
    | LoggedOut
    | MarkChangelogRead
    | Tick Posix
