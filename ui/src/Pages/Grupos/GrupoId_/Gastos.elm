module Pages.Grupos.GrupoId_.Gastos exposing (Model, Msg, page)

import Components.Bootstrap as Bs
import Components.Movimientos as Movimientos
import Components.PagoDetalleModal as PagoDetalleModal
import Components.ResumenGasto as ResumenGasto
import Effect exposing (Effect)
import Generated.Api exposing (Grupo, ShallowPago, ULID)
import Html exposing (Html, a, div, i, span, text)
import Html.Attributes exposing (class, id)
import Layouts
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Route.Path as Path
import Shared
import Time exposing (Zone)
import Utils.Telemetry as Telemetry
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route shared.store
        , update = update
        , subscriptions = subscriptions
        , view = view shared.timezone (Shared.currentParticipante shared route.params.grupoId) shared.store
        }
        |> Page.withLayout (\_ -> Layouts.Default_Grupo {})


type alias Model =
    { grupoId : String
    }


type Msg
    = AbrirPopup PagoDetalleModal.Ruta


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        AbrirPopup ruta ->
            ( model, Effect.replaceRoute ruta )


init : Route { grupoId : String } -> Store -> ( Model, Effect Msg )
init route store =
    let
        grupoId =
            route.params.grupoId
    in
    ( { grupoId = grupoId }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Store.ensureTransferencias grupoId store
        , -- Los gastos los pide Shared cuando resuelve el participante, que
          -- es lo que este mensaje dispara.
          Effect.getCurrentUser grupoId
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Zone -> Maybe ULID -> Store -> Model -> View Msg
view zone participanteId store model =
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
                [ div [ id "gastos", class "container-fluid py-3" ]
                    [ viewMovimientos zone participanteId store model grupo ]
                ]
            }


viewMovimientos : Zone -> Maybe ULID -> Store -> Model -> Grupo -> Html Msg
viewMovimientos zone participanteId store model grupo =
    case
        RemoteData.map2
            (\pagos transferencias ->
                ( pagos, Movimientos.mkMovimientos zone pagos transferencias |> Movimientos.cronologico )
            )
            (store |> Store.getPagos model.grupoId)
            (store |> Store.getTransferencias model.grupoId)
    of
        NotAsked ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Loading ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Failure _ ->
            Bs.alert Bs.AlertDanger [] [ text "Error cargando los gastos." ]

        Success ( pagos, movimientos ) ->
            if List.isEmpty movimientos then
                Bs.alert Bs.AlertInfo
                    []
                    [ text "Todavía no hay gastos registrados. "
                    , a
                        (Telemetry.click "gastos.empty_new_gasto"
                            :: PagoDetalleModal.linkANuevoGasto AbrirPopup (Path.Grupos_GrupoId__Gastos { grupoId = grupo.id })
                        )
                        [ text "¡Agregá el primer gasto para empezar a dividir!" ]
                    ]

            else
                div []
                    [ viewRepartijasActivas participanteId grupo pagos
                    , Bs.card [ id "gastos-lista" ]
                        [ Bs.listGroupKeyed [ class "list-group-flush" ]
                            (movimientos
                                |> List.map
                                    (Movimientos.viewMovimiento
                                        { abrirPopup = AbrirPopup
                                        , desde = Path.Grupos_GrupoId__Gastos { grupoId = grupo.id }
                                        , telemetria = "gastos"
                                        , participanteId = participanteId
                                        , grupo = grupo
                                        }
                                    )
                            )
                        ]
                    ]


viewRepartijasActivas : Maybe ULID -> Grupo -> List ShallowPago -> Html Msg
viewRepartijasActivas participanteId grupo pagos =
    case List.filter ResumenGasto.esRepartijaActiva pagos of
        [] ->
            text ""

        activas ->
            let
                ( meFalta, resto ) =
                    List.partition (ResumenGasto.leFaltaReclamar participanteId) activas

                titulo =
                    (if List.length activas == 1 then
                        "1 repartija activa"

                     else
                        String.fromInt (List.length activas) ++ " repartijas activas"
                    )
                        ++ (if List.isEmpty meFalta then
                                ""

                            else
                                " · te falta reclamar en " ++ String.fromInt (List.length meFalta)
                           )

                fila leFalta pago =
                    a
                        (class "d-flex align-items-center gap-2 text-reset text-decoration-none"
                            :: Telemetry.click "gastos.open_repartija_activa"
                            :: PagoDetalleModal.linkAGasto AbrirPopup (Path.Grupos_GrupoId__Gastos { grupoId = grupo.id }) pago.pagoId
                        )
                        [ ResumenGasto.viewBadgeRepartija pago
                        , span
                            [ class
                                (if leFalta then
                                    "text-truncate fw-semibold"

                                 else
                                    "text-truncate"
                                )
                            ]
                            [ text pago.nombre ]
                        , if leFalta then
                            Bs.badge "text-bg-primary rounded-pill flex-shrink-0 d-inline-flex align-items-center gap-1"
                                []
                                [ i [ class "bi bi-hand-index-thumb" ] [], text "Te falta reclamar" ]

                          else
                            text ""
                        ]
            in
            div [ id "gastos-repartijas-activas", class "mb-3" ]
                [ div [ class "text-uppercase fw-semibold small mb-2" ] [ text titulo ]
                , div [ class "d-flex flex-column gap-1" ]
                    (List.map (fila True) meFalta ++ List.map (fila False) resto)
                ]
