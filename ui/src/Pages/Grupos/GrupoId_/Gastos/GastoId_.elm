module Pages.Grupos.GrupoId_.Gastos.GastoId_ exposing (Model, Msg, page)

{-| Ver y editar un gasto dejó de ser una pantalla y pasó a ser el popup sobre
la lista de gastos. Esta página solo existe para que los links viejos —los que
alguien compartió o tiene en favoritos— sigan llegando al gasto, y redirige
apenas se monta a la lista con ese gasto abierto.

Reemplaza la entrada en el historial en vez de agregar una, así el botón de
volver no rebota entre la ruta vieja y la nueva.

-}

import Components.PagoDetalleModal as PagoDetalleModal
import Effect exposing (Effect)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path as Path
import Shared
import View


page : Shared.Model -> Route { grupoId : String, gastoId : String } -> Page Model Msg
page _ route =
    Page.new
        { init = \() -> init route.params.grupoId route.params.gastoId
        , update = \_ model -> ( model, Effect.none )
        , subscriptions = \_ -> Sub.none
        , view = \_ -> View.none
        }


type alias Model =
    {}


type alias Msg =
    ()


init : String -> String -> ( Model, Effect Msg )
init grupoId gastoId =
    ( {}
    , Effect.replaceRoute <|
        PagoDetalleModal.rutaGasto (Path.Grupos_GrupoId__Gastos { grupoId = grupoId }) gastoId
    )
