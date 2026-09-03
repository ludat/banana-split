module Pages.Grupos.GrupoId_.Pagos exposing (Model, Msg, page)

{-| La sección pasó a llamarse "gastos". Esta página solo existe para que los
links viejos —los que alguien compartió o tiene en favoritos— sigan llegando a
algún lado, y redirige apenas se monta.

Reemplaza la entrada en el historial en vez de agregar una, así el botón de
volver no rebota entre la ruta vieja y la nueva.

El modal de detalle abre según un query param, que también se renombró: acá
traducimos `?pago=<id>` al `?gasto=<id>` que espera la página nueva, así el
resto del código solo conoce el nombre nuevo.

-}

import Dict exposing (Dict)
import Effect exposing (Effect)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path as Path
import Shared
import View


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page _ route =
    Page.new
        { init = \() -> init route.params.grupoId route.query route.hash
        , update = \_ model -> ( model, Effect.none )
        , subscriptions = \_ -> Sub.none
        , view = \_ -> View.none
        }


type alias Model =
    {}


type alias Msg =
    ()


init : String -> Dict String String -> Maybe String -> ( Model, Effect Msg )
init grupoId query hash =
    ( {}
    , Effect.replaceRoute
        { path = Path.Grupos_GrupoId__Gastos { grupoId = grupoId }
        , query = renombrarParamPago query
        , hash = hash
        }
    )


renombrarParamPago : Dict String String -> Dict String String
renombrarParamPago query =
    case Dict.get "pago" query of
        Just pagoId ->
            query
                |> Dict.remove "pago"
                |> Dict.insert "gasto" pagoId

        Nothing ->
            query
