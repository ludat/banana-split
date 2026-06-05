module Pages.Grupos.GrupoId_.Pagos exposing (Model, Msg, page)

import Components.Bootstrap as Bs
import Date
import Effect exposing (Effect)
import Generated.Api exposing (Moneda, ShallowGrupo, ShallowPago, ULID)
import Html exposing (Html, a, div, i, text)
import Html.Attributes exposing (class, style)
import Layouts
import Models.Moneda as Moneda
import Models.Monto as Monto
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Route.Path as Path
import Shared
import Utils.Day
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.grupoId shared.store
        , update = update
        , subscriptions = subscriptions
        , view = view shared.store
        }
        |> Page.withLayout (\_ -> Layouts.Default_Grupo {})


type alias Model =
    { grupoId : String
    }


init : ULID -> Store -> ( Model, Effect Msg )
init grupoId store =
    ( { grupoId = grupoId }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Store.ensurePagos grupoId store
        , Effect.getCurrentUser grupoId
        ]
    )


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Effect Msg )
update _ model =
    ( model, Effect.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Store -> Model -> View Msg
view store model =
    case store |> Store.getGrupo model.grupoId of
        NotAsked ->
            { title = "Loading...", body = [] }

        Loading ->
            { title = "Cargando"
            , body = [ div [ class "container-fluid py-4 text-muted" ] [ text "Cargando..." ] ]
            }

        Failure _ ->
            { title = "Fallo", body = [] }

        Success grupo ->
            { title = grupo.nombre
            , body =
                [ div [ class "container-fluid py-3" ]
                    [ viewPagos store model grupo ]
                ]
            }


viewPagos : Store -> Model -> ShallowGrupo -> Html Msg
viewPagos store model grupo =
    case store |> Store.getPagos model.grupoId of
        NotAsked ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Loading ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Failure _ ->
            Bs.alert Bs.AlertDanger [] [ text "Error cargando los pagos." ]

        Success pagos ->
            if List.isEmpty pagos then
                Bs.alert Bs.AlertInfo
                    []
                    [ text "Todavía no hay pagos registrados. "
                    , a [ Path.href <| Path.Grupos_GrupoId__Pagos_New { grupoId = grupo.id } ]
                        [ text "¡Agregá el primer pago para empezar a dividir gastos!" ]
                    ]

            else
                Bs.card []
                    [ Bs.listGroup [ class "list-group-flush" ]
                        (pagos
                            |> List.sortWith (\a b -> Date.compare b.fecha a.fecha)
                            |> List.map (viewPago model.grupoId grupo.monedaPorDefecto)
                        )
                    ]


viewPago : ULID -> Moneda -> ShallowPago -> Html Msg
viewPago grupoId monedaPorDefecto pago =
    Bs.listGroupItem []
        [ div [ class "d-flex align-items-center gap-3" ]
            [ div
                [ class "text-center border rounded px-2 py-1 flex-shrink-0"
                , style "min-width" "2.5rem"
                ]
                [ div [ class "text-muted text-uppercase lh-1", style "font-size" "0.6em" ]
                    [ text (Utils.Day.mesAbreviado pago.fecha) ]
                , div [ class "fw-bold lh-1" ] [ text (String.fromInt (Date.day pago.fecha)) ]
                ]
            , if not pago.isValid then
                i [ class "bi bi-exclamation-triangle-fill text-warning flex-shrink-0" ] []

              else
                text ""
            , div [ class "flex-grow-1 text-truncate" ] [ text pago.nombre ]
            , div [ class "text-nowrap text-muted small" ]
                [ text (Moneda.simbolo monedaPorDefecto pago.moneda ++ " " ++ Monto.toString pago.monto) ]
            , a
                [ Path.href <| Path.Grupos_GrupoId__Pagos_PagoId_ { grupoId = grupoId, pagoId = pago.pagoId }
                , class "btn btn-sm btn-outline-secondary flex-shrink-0"
                ]
                [ i [ class "bi bi-arrow-right" ] [] ]
            ]
        ]
