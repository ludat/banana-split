module Pages.Grupos.GrupoId_.Gastos exposing (Model, Msg, page)

import Components.Bootstrap as Bs
import Components.PagoDetalleModal as PagoDetalleModal
import Components.ResumenGasto as ResumenGasto
import Components.TransferenciaModal as TransferenciaModal
import Date exposing (Date)
import Effect exposing (Effect)
import Generated.Api exposing (Grupo, Moneda, ShallowPago, Transferencia, ULID)
import Html exposing (Html, a, div, i, text)
import Html.Attributes exposing (class, id, style)
import Layouts
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Models.Transferencia
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Route.Path as Path
import Shared
import Time exposing (Zone)
import Utils.Day
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


{-| Lo que se lista: los gastos y, entre ellos, la plata que ya se movió. Las
transferencias pendientes no van: todavía no pasaron.
-}
type Movimiento
    = Gasto ShallowPago
    | TransferenciaHecha Date Transferencia


fechaDe : Movimiento -> Date
fechaDe movimiento =
    case movimiento of
        Gasto pago ->
            pago.fecha

        TransferenciaHecha fecha _ ->
            fecha


{-| Un gasto tiene el día que cargó el usuario; una transferencia, el instante
en que se marcó. Para ordenarlas juntas, ese instante pasa al día en la zona
del navegador.
-}
movimientos : Zone -> List ShallowPago -> List Transferencia -> List Movimiento
movimientos zone pagos transferencias =
    List.map Gasto pagos
        ++ List.filterMap
            (\t -> t.saldadaAt |> Maybe.map (\saldadaAt -> TransferenciaHecha (Date.fromPosix zone saldadaAt) t))
            transferencias
        |> List.sortWith (\a b -> Date.compare (fechaDe b) (fechaDe a))


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
    case store |> Store.getPagos model.grupoId of
        NotAsked ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Loading ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Failure _ ->
            Bs.alert Bs.AlertDanger [] [ text "Error cargando los gastos." ]

        Success pagos ->
            let
                -- Los gastos no esperan a las transferencias: si todavía no
                -- llegaron, aparecen cuando lleguen.
                lista =
                    movimientos zone
                        pagos
                        (store |> Store.getTransferencias model.grupoId |> RemoteData.withDefault [])
            in
            if List.isEmpty lista then
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
                Bs.card [ id "gastos-lista" ]
                    [ Bs.listGroupKeyed [ class "list-group-flush" ]
                        (lista
                            |> List.map
                                (\movimiento ->
                                    case movimiento of
                                        Gasto pago ->
                                            ( pago.pagoId, viewPago participanteId grupo.id grupo.monedaPorDefecto pago )

                                        TransferenciaHecha fecha t ->
                                            ( "transferencia-" ++ t.id, viewTransferencia participanteId grupo fecha t )
                                )
                        )
                    ]


viewPago : Maybe ULID -> ULID -> Moneda -> ShallowPago -> Html Msg
viewPago participanteId grupoId monedaPorDefecto pago =
    Bs.listGroupItem
        [ class "list-group-item-action p-0" ]
        [ a
            (class "d-flex align-items-center gap-3 px-3 py-2 text-reset text-decoration-none"
                :: Telemetry.click "gastos.open_gasto"
                :: PagoDetalleModal.linkAGasto AbrirPopup (Path.Grupos_GrupoId__Gastos { grupoId = grupoId }) pago.pagoId
            )
            [ viewFecha pago.fecha
            , ResumenGasto.viewFila participanteId monedaPorDefecto pago
            , ResumenGasto.viewIconoInvalido pago
            , ResumenGasto.viewBadgeRepartija pago
            ]
        ]


{-| Más baja que la de un gasto: es plata que se movió entre dos, no algo que
se repartió, así que alcanza con una línea. La fecha va en el mismo ancho que
el recuadro de los gastos, para que las filas queden alineadas.
-}
viewTransferencia : Maybe ULID -> Grupo -> Date -> Transferencia -> Html Msg
viewTransferencia participanteId grupo fecha t =
    Bs.listGroupItem
        [ class "list-group-item-action p-0" ]
        [ a
            (class "d-flex align-items-center gap-3 px-3 py-1 text-reset text-decoration-none small"
                :: Telemetry.click "gastos.open_transferencia"
                :: TransferenciaModal.linkATransferencia AbrirPopup (Path.Grupos_GrupoId__Gastos { grupoId = grupo.id }) t.id
            )
            [ div
                [ class "text-center text-muted flex-shrink-0 lh-1"
                , style "min-width" "2.5rem"
                , style "font-size" "0.75em"
                ]
                [ text (String.fromInt (Date.day fecha) ++ " " ++ Utils.Day.mesAbreviado fecha) ]
            , div [ class "flex-grow-1 text-truncate text-body-secondary" ]
                (i [ class "bi bi-arrow-left-right me-2", Html.Attributes.attribute "aria-label" "Transferencia" ] []
                    :: Models.Transferencia.conFlechasPara participanteId grupo t
                )
            ]
        ]


viewFecha : Date -> Html msg
viewFecha fecha =
    div
        [ class "text-center border rounded px-2 py-1 flex-shrink-0"
        , style "min-width" "2.5rem"
        ]
        [ div [ class "text-muted text-uppercase lh-1", style "font-size" "0.6em" ]
            [ text (Utils.Day.mesAbreviado fecha) ]
        , div [ class "fw-bold lh-1" ] [ text (String.fromInt (Date.day fecha)) ]
        ]
