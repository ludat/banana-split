module Components.Movimientos exposing (Movimiento, cronologico, mkMovimientos, porId, viewMovimiento)

{-| Los gastos y, entre ellos, la plata que ya se movió: lo que muestran la
lista de gastos y "Ultimos movimientos" en el resumen.
-}

import Components.Bootstrap as Bs
import Components.PagoDetalleModal as PagoDetalleModal
import Components.ResumenGasto as ResumenGasto
import Components.TransferenciaModal as TransferenciaModal
import Date exposing (Date)
import Generated.Api exposing (Grupo, ShallowPago, Transferencia, ULID)
import Html exposing (Html, a, div, i, text)
import Html.Attributes as Attr exposing (class, style)
import Models.Transferencia
import Route.Path as Path
import Time exposing (Zone)
import Utils.Day
import Utils.Telemetry as Telemetry


{-| Las transferencias pendientes no van: todavía no pasaron. De una hecha se
guarda el día en que se marcó, en la zona del navegador, que es lo que se
compara con la fecha de los gastos.
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


idDe : Movimiento -> ULID
idDe movimiento =
    case movimiento of
        Gasto pago ->
            pago.pagoId

        TransferenciaHecha _ t ->
            t.id


{-| Sin ordenar: cada lista elige su orden con `cronologico` o `porId`.
-}
mkMovimientos : Zone -> List ShallowPago -> List Transferencia -> List Movimiento
mkMovimientos zone pagos transferencias =
    List.map Gasto pagos
        ++ List.filterMap
            (\t -> t.saldadaAt |> Maybe.map (\saldadaAt -> TransferenciaHecha (Date.fromPosix zone saldadaAt) t))
            transferencias


{-| Por cuándo pasó, de lo más nuevo a lo más viejo: los gastos por su fecha y
las transferencias por el día en que se marcaron. Un gasto no tiene hora, así
que lo que cae el mismo día se desempata por id, como en `porId`.
-}
cronologico : List Movimiento -> List Movimiento
cronologico =
    List.sortWith
        (\a b ->
            case Date.compare (fechaDe b) (fechaDe a) of
                EQ ->
                    compare (idDe b) (idDe a)

                orden ->
                    orden
        )


{-| Por orden de carga, de lo último a lo primero: los ids son ULIDs, que
ordenan por el momento en que se crearon.
-}
porId : List Movimiento -> List Movimiento
porId =
    List.sortWith (\a b -> compare (idDe b) (idDe a))


{-| Una fila de la lista, con su clave para `Bs.listGroupKeyed`. `desde` es la
página sobre la que se abren los popups y `telemetria`, el prefijo de los
eventos de click (`"gastos"`, `"resumen"`).
-}
viewMovimiento :
    { abrirPopup : PagoDetalleModal.Ruta -> msg
    , desde : Path.Path
    , telemetria : String
    , participanteId : Maybe ULID
    , grupo : Grupo
    }
    -> Movimiento
    -> ( String, Html msg )
viewMovimiento config movimiento =
    case movimiento of
        Gasto pago ->
            ( pago.pagoId, viewPago config pago )

        TransferenciaHecha fecha t ->
            ( "transferencia-" ++ t.id, viewTransferencia config fecha t )


viewPago :
    { r | abrirPopup : PagoDetalleModal.Ruta -> msg, desde : Path.Path, telemetria : String, participanteId : Maybe ULID, grupo : Grupo }
    -> ShallowPago
    -> Html msg
viewPago config pago =
    Bs.listGroupItem
        [ class "list-group-item-action p-0" ]
        [ a
            (class "d-flex align-items-center gap-3 px-3 py-2 text-reset text-decoration-none"
                :: Telemetry.click (config.telemetria ++ ".open_gasto")
                :: PagoDetalleModal.linkAGasto config.abrirPopup config.desde pago.pagoId
            )
            [ viewFecha pago.fecha
            , ResumenGasto.viewFila config.participanteId config.grupo.monedaPorDefecto pago
            , ResumenGasto.viewIconoInvalido pago
            , ResumenGasto.viewBadgeRepartija pago
            ]
        ]


{-| Más baja que la de un gasto: es plata que se movió entre dos, no algo que
se repartió, así que alcanza con una línea. La fecha va en el mismo ancho que
el recuadro de los gastos, para que las filas queden alineadas.
-}
viewTransferencia :
    { r | abrirPopup : PagoDetalleModal.Ruta -> msg, desde : Path.Path, telemetria : String, participanteId : Maybe ULID, grupo : Grupo }
    -> Date
    -> Transferencia
    -> Html msg
viewTransferencia config fecha t =
    Bs.listGroupItem
        [ class "list-group-item-action p-0" ]
        [ a
            (class "d-flex align-items-center gap-3 px-3 py-1 text-reset text-decoration-none small"
                :: Telemetry.click (config.telemetria ++ ".open_transferencia")
                :: TransferenciaModal.linkATransferencia config.abrirPopup config.desde t.id
            )
            [ div
                [ class "text-center text-muted flex-shrink-0 lh-1"
                , style "min-width" "2.5rem"
                , style "font-size" "0.75em"
                ]
                [ text (String.fromInt (Date.day fecha) ++ " " ++ Utils.Day.mesAbreviado fecha) ]
            , div [ class "flex-grow-1 text-truncate text-body-secondary" ]
                (i [ class "bi bi-arrow-left-right me-2", Attr.attribute "aria-label" "Transferencia" ] []
                    :: Models.Transferencia.conFlechasPara config.participanteId config.grupo t
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
