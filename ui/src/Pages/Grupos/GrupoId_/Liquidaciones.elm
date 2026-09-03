module Pages.Grupos.GrupoId_.Liquidaciones exposing (Model, Msg, page)

{-| La sección pasó a llamarse "transferencias". Esta página solo existe para
que los links viejos —los que alguien compartió o tiene en favoritos— sigan
llegando a algún lado, y redirige apenas se monta.

Reemplaza la entrada en el historial en vez de agregar una, así el botón de
volver no rebota entre la ruta vieja y la nueva.

-}

import Effect exposing (Effect)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path as Path
import Shared
import View


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page _ route =
    Page.new
        { init = \() -> init route.params.grupoId
        , update = \_ model -> ( model, Effect.none )
        , subscriptions = \_ -> Sub.none
        , view = \_ -> View.none
        }


type alias Model =
    {}


type alias Msg =
    ()


init : String -> ( Model, Effect Msg )
init grupoId =
    ( {}
    , Effect.replaceRoutePath (Path.Grupos_GrupoId__Transferencias { grupoId = grupoId })
    )
