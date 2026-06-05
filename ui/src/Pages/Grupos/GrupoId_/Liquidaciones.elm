module Pages.Grupos.GrupoId_.Liquidaciones exposing (Model, Msg, page)

import Components.Bootstrap as Bs
import Components.MonedaSelector as MonedaSelector exposing (MonedaSeleccionada(..))
import Components.NavBar as NavBar
import Effect exposing (Effect)
import Generated.Api as Api exposing (Moneda, Pago, PorMoneda, ResumenGrupo, ShallowGrupo, Transaccion, ULID)
import Html exposing (Html, div, i, span, text)
import Html.Attributes as Attr exposing (class, style)
import Html.Events exposing (onClick)
import Http
import Layouts
import Models.Grupo exposing (GrupoLike, lookupNombreParticipante)
import Models.Moneda as Moneda
import Models.Monto as Monto
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared
import Utils.Day exposing (Day)
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts
import Utils.Ulid exposing (emptyUlid)
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.grupoId shared.store
        , update = update
        , subscriptions = subscriptions
        , view = view shared.today shared.store
        }
        |> Page.withLayout
            (\_ ->
                Layouts.Default
                    { navBarContent = Just <| NavBar.navBar (NavBar.modelFromShared shared route.params.grupoId) shared.store route.path
                    }
            )


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
        [ Store.ensureResumen grupoId store
        , Store.ensureGrupo grupoId store
        , Effect.getCurrentUser grupoId
        ]
    )


type Msg
    = CrearPago Pago
    | AddedPagoResponse (Result Http.Error Pago)
    | SaldarTransaccion ULID Pago
    | SaldadaTransaccionResponse (Result Http.Error Pago)
    | SelectMoneda Moneda


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        CrearPago pago ->
            ( model
            , Effect.sendCmd <|
                Api.postGrupoByIdPagos
                    model.grupoId
                    pago
                    AddedPagoResponse
            )

        AddedPagoResponse (Ok _) ->
            ( model
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshGrupo model.grupoId
                , Store.refreshPagos model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Se completó el pago"
                ]
            )

        AddedPagoResponse (Err _) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "No se pudo completar el pago"
            )

        SaldarTransaccion transaccionId pago ->
            ( model
            , Effect.sendCmd <|
                Api.postGrupoByIdTransaccionescongeladasByTransaccionIdSaldar
                    model.grupoId
                    transaccionId
                    pago
                    SaldadaTransaccionResponse
            )

        SaldadaTransaccionResponse (Ok _) ->
            ( model
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshGrupo model.grupoId
                , Store.refreshPagos model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Se completó el pago"
                ]
            )

        SaldadaTransaccionResponse (Err _) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "No se pudo completar el pago"
            )

        SelectMoneda moneda ->
            ( { model | monedaSeleccionada = MonedaSeleccionadaPorUsuario moneda }
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Day -> Store -> Model -> View Msg
view hoy store model =
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
                    [ viewContent hoy store model grupo ]
                ]
            }


viewContent : Day -> Store -> Model -> ShallowGrupo -> Html Msg
viewContent hoy store model grupo =
    case store |> Store.getResumen model.grupoId of
        NotAsked ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Loading ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Failure _ ->
            Bs.alert Bs.AlertDanger [] [ text "Error cargando los datos del grupo." ]

        Success resumen ->
            let
                monedasDisponibles : List Moneda
                monedasDisponibles =
                    resumen.netos
                        |> List.map Tuple.first
                        |> List.filter (\m -> m /= grupo.monedaPorDefecto)
                        |> (::) grupo.monedaPorDefecto

                monedaSeleccionada : Moneda
                monedaSeleccionada =
                    MonedaSelector.resolve model.monedaSeleccionada grupo.monedaPorDefecto

                transForActive : PorMoneda (List Transaccion)
                transForActive =
                    resumen.transaccionesParaSaldar
                        |> List.filter (\( m, _ ) -> m == monedaSeleccionada)
            in
            div []
                [ if List.length monedasDisponibles > 1 then
                    MonedaSelector.view monedasDisponibles monedaSeleccionada SelectMoneda

                  else
                    text ""
                , viewTransferencias hoy grupo.monedaPorDefecto grupo { resumen | transaccionesParaSaldar = transForActive }
                ]


viewTransferencias : Day -> Moneda -> GrupoLike g -> ResumenGrupo -> Html Msg
viewTransferencias hoy monedaPorDefecto grupo resumen =
    if List.isEmpty resumen.transaccionesParaSaldar then
        Bs.alert Bs.AlertSuccess
            [ class "text-center" ]
            [ text "¡No hay deudas pendientes! Todos están al día." ]

    else
        div []
            (resumen.transaccionesParaSaldar
                |> Moneda.perEach
                    (\moneda ts ->
                        ts
                            |> List.map
                                (\t ->
                                    div
                                        [ style "display" "grid"
                                        , style "grid-template-columns" "1fr auto 1fr"
                                        , style "align-items" "center"
                                        , style "margin-bottom" "0.5rem"
                                        ]
                                        [ div [ class "text-end" ]
                                            [ div [] [ text <| lookupNombreParticipante grupo t.from ]
                                            , div [ class "text-danger small" ]
                                                [ text <| Moneda.simbolo monedaPorDefecto moneda
                                                , text " "
                                                , text <| Monto.toString t.monto
                                                ]
                                            ]
                                        , Bs.btn Bs.Transparent
                                            [ Attr.title "Crear pago para saldar esta deuda"
                                            , onClick <|
                                                case t.id of
                                                    Just transaccionId ->
                                                        SaldarTransaccion transaccionId (pagoFromTransaccion hoy moneda t)

                                                    Nothing ->
                                                        CrearPago (pagoFromTransaccion hoy moneda t)
                                            , style "margin" "0 0.5rem"
                                            ]
                                            [ i [ class "bi bi-arrow-right" ] [] ]
                                        , span [] [ text <| lookupNombreParticipante grupo t.to ]
                                        ]
                                )
                    )
                |> List.concat
            )


pagoFromTransaccion : Day -> Moneda -> Transaccion -> Pago
pagoFromTransaccion hoy moneda transaction =
    { pagoId = emptyUlid
    , isValid = False
    , fecha = hoy
    , nombre = "Pago saldado"
    , moneda = moneda
    , monto = transaction.monto
    , pagadores =
        { id = emptyUlid
        , tipo =
            Api.TipoDistribucionMontoEquitativo <|
                { id = emptyUlid
                , participantes =
                    [ transaction.from
                    ]
                }
        }
    , deudores =
        { id = emptyUlid
        , tipo =
            Api.TipoDistribucionMontoEquitativo <|
                { id = emptyUlid
                , participantes =
                    [ transaction.to
                    ]
                }
        }
    }
