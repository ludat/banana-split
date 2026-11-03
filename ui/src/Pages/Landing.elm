module Pages.Landing exposing (Model, Msg, page)

{-| La landing a pedido, aunque se haya iniciado sesión. Es la misma página que
`/` (ver `Pages.Home_.Modo`).
-}

import Layouts
import Page exposing (Page)
import Pages.Home_ as Home
import Route exposing (Route)
import Shared


type alias Model =
    Home.Model


type alias Msg =
    Home.Msg


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new (Home.config Home.SiempreLanding shared)
        |> Page.withLayout (\_ -> Layouts.Default {})
