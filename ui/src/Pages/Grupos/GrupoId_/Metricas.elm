module Pages.Grupos.GrupoId_.Metricas exposing (Model, Msg, page)

import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.Bootstrap as Bs
import Components.MonedaSelector as MonedaSelector exposing (MonedaSeleccionada(..))
import Date
import Effect exposing (Effect)
import Generated.Api exposing (BigOne, BusiestDia, BusiestMes, MetricasPorMoneda, Moneda, ParticipanteId, ShallowGrupo, ULID)
import Html exposing (Html, a, div, h5, p, span, text)
import Html.Attributes exposing (class)
import Layouts
import Models.Grupo exposing (GrupoLike, lookupNombreParticipante)
import Models.Monto as Monto
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Route.Path as Path
import Shared
import Utils.Day as Day
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
    , monedaSeleccionada : MonedaSeleccionada
    }


init : ULID -> Store -> ( Model, Effect Msg )
init grupoId store =
    ( { grupoId = grupoId
      , monedaSeleccionada = MonedaDefaultDelGrupo
      }
    , Effect.batch
        [ Store.ensureMetricas grupoId store
        , Store.ensureGrupo grupoId store
        , Effect.getCurrentUser grupoId
        ]
    )


type Msg
    = SelectMoneda Moneda


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SelectMoneda moneda ->
            ( { model | monedaSeleccionada = MonedaSeleccionadaPorUsuario moneda }
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Store -> Model -> View Msg
view store model =
    case store |> Store.getGrupo model.grupoId of
        NotAsked ->
            { title = "Loading...", body = [] }

        Loading ->
            { title = "Cargando", body = [ div [ class "container-fluid py-4 text-muted" ] [ text "Cargando..." ] ] }

        Failure _ ->
            { title = "Fallo", body = [] }

        Success grupo ->
            { title = grupo.nombre
            , body =
                [ div [ class "container-fluid py-3" ]
                    [ viewContent store model grupo ]
                ]
            }


viewContent : Store -> Model -> ShallowGrupo -> Html Msg
viewContent store model grupo =
    case store |> Store.getMetricas model.grupoId of
        NotAsked ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Loading ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Failure _ ->
            Bs.alert Bs.AlertDanger [] [ text "Error cargando las métricas del grupo." ]

        Success metricas ->
            let
                monedasDisponibles : List Moneda
                monedasDisponibles =
                    metricas.porMoneda
                        |> List.map Tuple.first
                        |> List.filter (\m -> m /= grupo.monedaPorDefecto)
                        |> (::) grupo.monedaPorDefecto

                monedaSeleccionada : Moneda
                monedaSeleccionada =
                    MonedaSelector.resolve model.monedaSeleccionada grupo.monedaPorDefecto

                metricasPorMoneda : Maybe MetricasPorMoneda
                metricasPorMoneda =
                    metricas.porMoneda
                        |> List.filter (\( m, _ ) -> m == monedaSeleccionada)
                        |> List.head
                        |> Maybe.map Tuple.second
            in
            div []
                [ if List.length monedasDisponibles > 1 then
                    MonedaSelector.view monedasDisponibles monedaSeleccionada SelectMoneda

                  else
                    text ""
                , case metricasPorMoneda of
                    Nothing ->
                        Bs.alert Bs.AlertSuccess [ class "text-center" ] [ text "No hay datos suficientes todavía." ]

                    Just datos ->
                        viewMetricas grupo datos
                ]


viewMetricas : GrupoLike g -> MetricasPorMoneda -> Html Msg
viewMetricas grupo datos =
    div [ class "row g-3" ]
        [ div [ class "col-12 col-lg-6" ]
            [ Bs.card []
                [ Bs.cardHeader [] [ text "El que más gastó" ]
                , Bs.cardBody []
                    [ if List.isEmpty datos.totalGastadoPorParticipante then
                        p [ class "text-muted mb-0" ] [ text "Todavía no hay gastos." ]

                      else
                        viewNetosBarras grupo datos.totalGastadoPorParticipante
                    ]
                ]
            ]
        , div [ class "col-12 col-lg-6" ]
            [ Bs.card []
                [ Bs.cardHeader [] [ text "El que más puso" ]
                , Bs.cardBody []
                    [ if List.isEmpty datos.totalPagadoPorParticipante then
                        p [ class "text-muted mb-0" ] [ text "Todavía no hay pagos." ]

                      else
                        viewNetosBarras grupo datos.totalPagadoPorParticipante
                    ]
                ]
            ]
        , div [ class "col-12 col-lg-6" ]
            [ Bs.card []
                [ Bs.cardHeader [] [ text "El más generoso" ]
                , Bs.cardBody []
                    [ viewGenerosidad grupo datos.generosidad ]
                ]
            ]
        , div [ class "col-12 col-lg-6" ]
            [ Bs.card []
                [ Bs.cardHeader [] [ text "La bomba" ]
                , Bs.cardBody []
                    [ viewTheBigOne grupo datos.theBigOne ]
                ]
            ]
        , div [ class "col-12 col-lg-6" ]
            [ Bs.card []
                [ Bs.cardHeader [] [ text "Mes con más movimiento" ]
                , Bs.cardBody []
                    [ viewBusiestMes datos.busiestMes ]
                ]
            ]
        , div [ class "col-12 col-lg-6" ]
            [ Bs.card []
                [ Bs.cardHeader [] [ text "Día con más movimiento" ]
                , Bs.cardBody []
                    [ viewBusiestDia datos.busiestDia ]
                ]
            ]
        ]


viewGenerosidad : GrupoLike g -> List ( ParticipanteId, Float ) -> Html Msg
viewGenerosidad grupo generosidad =
    let
        ordenados =
            generosidad
                |> List.sortBy (\( _, ratio ) -> -ratio)
    in
    if List.isEmpty ordenados then
        p [ class "text-muted mb-0" ] [ text "Todavía no hay datos suficientes." ]

    else
        Bs.listGroup [ class "list-group-flush" ]
            (ordenados
                |> List.map
                    (\( participanteId, ratio ) ->
                        Bs.listGroupItem [ class "d-flex justify-content-between align-items-center" ]
                            [ span [] [ text (lookupNombreParticipante grupo participanteId) ]
                            , span [ class "text-muted" ] [ text (formatRatio ratio) ]
                            ]
                    )
            )


formatRatio : Float -> String
formatRatio ratio =
    String.fromInt (round (ratio * 100)) ++ "%"


viewTheBigOne : GrupoLike g -> Maybe BigOne -> Html Msg
viewTheBigOne grupo maybeBigOne =
    case maybeBigOne of
        Nothing ->
            p [ class "text-muted mb-0" ] [ text "Todavía no hay pagos." ]

        Just bigOne ->
            a
                [ Path.href (Path.Grupos_GrupoId__Pagos_PagoId_ { grupoId = grupo.id, pagoId = bigOne.pagoId })
                , class "text-decoration-none"
                ]
                [ h5 [ class "mb-1" ] [ text bigOne.nombre ]
                , p [ class "mb-1 fs-4 fw-bold text-body" ] [ text (Monto.toString bigOne.monto) ]
                , p [ class "text-muted mb-0 small" ] [ text (Day.toString bigOne.fecha) ]
                ]


viewBusiestMes : Maybe BusiestMes -> Html Msg
viewBusiestMes maybeBusiestMes =
    case maybeBusiestMes of
        Nothing ->
            p [ class "text-muted mb-0" ] [ text "Todavía no hay pagos." ]

        Just busiestMes ->
            let
                nombreMes =
                    Date.fromCalendarDate busiestMes.anio (Date.numberToMonth busiestMes.mes) 1
                        |> Day.mesAbreviado
            in
            div []
                [ h5 [ class "mb-1" ] [ text (nombreMes ++ " " ++ String.fromInt busiestMes.anio) ]
                , p [ class "mb-0 fs-4 fw-bold text-body" ] [ text (Monto.toString busiestMes.total) ]
                ]


viewBusiestDia : Maybe BusiestDia -> Html Msg
viewBusiestDia maybeBusiestDia =
    case maybeBusiestDia of
        Nothing ->
            p [ class "text-muted mb-0" ] [ text "Todavía no hay pagos." ]

        Just busiestDia ->
            div []
                [ h5 [ class "mb-1" ] [ text (Day.toString busiestDia.dia) ]
                , p [ class "mb-0 fs-4 fw-bold text-body" ] [ text (Monto.toString busiestDia.total) ]
                ]
