module Pages.NotFound_ exposing (Model, Msg, page)

import Components.ErrorDePagina as ErrorDePagina
import Effect
import Html exposing (a, text)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Route.Path as Path
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = \() -> ( {}, Effect.none )
        , update = \_ model -> ( model, Effect.none )
        , subscriptions = \_ -> Sub.none
        , view = \_ -> view
        }
        |> Page.withLayout (\_ -> Layouts.Default {})


type alias Model =
    {}


type alias Msg =
    ()


view : View Msg
view =
    { title = "Página inexistente"
    , body =
        [ ErrorDePagina.view
            { tipo = ErrorDePagina.NoExiste
            , titulo = "Esta página no existe"
            , detalle =
                [ text "Revisá que el link esté bien, o "
                , a [ Path.href Path.Home_ ] [ text "volvé al inicio" ]
                , text "."
                ]
            }
        ]
    }
