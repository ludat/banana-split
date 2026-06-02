module Pages.Grupos.Id_ exposing (Model, MonedaSeleccionada, Msg, page)

import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.Bootstrap as Bs
import Components.NavBar as NavBar exposing (modelFromShared)
import Date
import Effect exposing (Effect)
import Generated.Api as Api exposing (Moneda, Netos, Pago, PorMoneda, ResumenGrupo, ShallowGrupo, ShallowPago, Transaccion, ULID, jsonDecMoneda)
import Html exposing (Html, a, button, div, i, option, p, select, span, strong, text)
import Html.Attributes as Attr exposing (class, selected, style, type_, value)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode
import Layouts
import Models.Grupo exposing (GrupoLike, lookupNombreParticipante)
import Models.Moneda as Moneda
import Models.Monto as Monto
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Route.Path as Path
import Shared
import Utils.Day exposing (Day)
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts
import Utils.Ulid exposing (emptyUlid)
import View exposing (View)


page : Shared.Model -> Route { id : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.id shared.store
        , update = update shared.store
        , subscriptions = subscriptions
        , view = view shared.today shared.store
        }
        |> Page.withLayout
            (\m ->
                Layouts.Default
                    { navBarContent = Just <| NavBar.navBar (modelFromShared shared route.params.id) shared.store route.path
                    , grupo = Store.getGrupo m.grupoId shared.store
                    }
            )


type alias Model =
    { grupoId : String
    , deletingPagoId : Maybe ULID
    , monedaSeleccionada : MonedaSeleccionada
    }


type MonedaSeleccionada
    = MonedaDefaultDelGrupo
    | MonedaSeleccionadaPorUsuario Moneda


init : ULID -> Store -> ( Model, Effect Msg )
init grupoId store =
    ( { grupoId = grupoId
      , deletingPagoId = Nothing
      , monedaSeleccionada = MonedaDefaultDelGrupo
      }
    , Effect.batch
        [ Store.ensureResumen grupoId store
        , Store.ensureGrupo grupoId store
        , Store.ensurePagos grupoId store
        , Effect.getCurrentUser grupoId
        , Effect.setUnsavedChangesWarning False
        ]
    )


type Msg
    = CrearPago Pago
    | AddedPagoResponse (Result Http.Error Pago)
    | SaldarTransaccion ULID Pago
    | SaldadaTransaccionResponse (Result Http.Error Pago)
    | Navigate Path.Path
    | DeletePago ULID
    | ConfirmDeletePago ULID
    | CancelDeletePago
    | DeletePagoResponse (Result Http.Error ULID)
    | SelectMoneda Moneda


update : Store -> Msg -> Model -> ( Model, Effect Msg )
update store msg model =
    case msg of
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

        CrearPago pago ->
            ( model
            , Effect.sendCmd <|
                Api.postGrupoByIdPagos
                    model.grupoId
                    pago
                    AddedPagoResponse
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

        Navigate path ->
            ( model, Effect.pushRoutePath path )

        DeletePago pagoId ->
            ( { model | deletingPagoId = Just pagoId }
            , Effect.none
            )

        ConfirmDeletePago pagoId ->
            case store |> Store.getGrupo model.grupoId of
                Success grupo ->
                    ( { model | deletingPagoId = Nothing }
                    , Effect.sendCmd <| Api.deleteGrupoByIdPagosByPagoId grupo.id pagoId DeletePagoResponse
                    )

                _ ->
                    ( model, Effect.none )

        CancelDeletePago ->
            ( { model | deletingPagoId = Nothing }
            , Effect.none
            )

        DeletePagoResponse (Ok _) ->
            ( model
            , Effect.batch
                [ Store.refreshGrupo model.grupoId
                , Store.refreshResumen model.grupoId
                , Store.refreshPagos model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Pago borrado"
                ]
            )

        DeletePagoResponse (Err _) ->
            ( model, Toasts.pushToast Toasts.ToastDanger "Falle al borrar el pago" )

        SelectMoneda moneda ->
            ( { model | monedaSeleccionada = MonedaSeleccionadaPorUsuario moneda }
            , Effect.none
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Day -> Store -> Model -> View Msg
view hoy store model =
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
            let
                pagoBeingDeleted =
                    model.deletingPagoId
                        |> Maybe.andThen
                            (\pagoId ->
                                case Store.getPagos model.grupoId store of
                                    Success pagos ->
                                        pagos |> List.filter (\p -> p.pagoId == pagoId) |> List.head

                                    _ ->
                                        Nothing
                            )
            in
            { title = grupo.nombre
            , body =
                [ if List.isEmpty grupo.participantes then
                    div [ class "container-fluid py-3" ]
                        [ p [] [ text "Tu grupo todavía no tiene participantes!" ]
                        , p []
                            [ text "Agregalos "
                            , a [ Path.href <| Path.Grupos_GrupoId__Participantes { grupoId = grupo.id } ]
                                [ text "acá" ]
                            ]
                        ]

                  else
                    div [ class "container-fluid py-3" ]
                        [ div [ class "row g-4" ]
                            [ div [ class "col-lg-8" ]
                                [ viewLeftColumn hoy store model grupo ]
                            , div [ class "col-lg-4" ]
                                [ viewUltimosPagosCard store model grupo ]
                            ]
                        ]
                , Bs.modal
                    { isOpen = model.deletingPagoId /= Nothing
                    , onClose = CancelDeletePago
                    , title = "Confirmar eliminación"
                    , body =
                        [ p []
                            [ text "¿Estás seguro que querés eliminar "
                            , case pagoBeingDeleted of
                                Just pago ->
                                    strong [] [ text ("\"" ++ pago.nombre ++ "\"") ]

                                Nothing ->
                                    text "este pago"
                            , text "?"
                            ]
                        , p [ class "mt-2 text-muted" ] [ text "Esta acción no se puede deshacer." ]
                        ]
                    , footer =
                        [ Bs.btn Bs.Transparent [ onClick CancelDeletePago ] [ text "Cancelar" ]
                        , Bs.btn Bs.Danger
                            [ onClick <|
                                case model.deletingPagoId of
                                    Just pagoId ->
                                        ConfirmDeletePago pagoId

                                    Nothing ->
                                        Navigate (Path.Grupos_Id_ { id = "" })
                            ]
                            [ text "Eliminar" ]
                        ]
                    }
                ]
            }


viewLeftColumn : Day -> Store -> Model -> ShallowGrupo -> Html Msg
viewLeftColumn hoy store model grupo =
    case store |> Store.getResumen model.grupoId of
        NotAsked ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Loading ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Failure _ ->
            Bs.alert Bs.AlertDanger [] [ text "Error cargando los datos del grupo." ]

        Success resumen ->
            if resumen.cantidadPagos == 0 then
                Bs.alert Bs.AlertInfo
                    []
                    [ text "Todavía no hay pagos registrados. "
                    , a [ Path.href <| Path.Grupos_GrupoId__Pagos_New { grupoId = grupo.id } ]
                        [ text "¡Agregá el primer pago para empezar a dividir gastos!" ]
                    ]

            else
                let
                    monedasDisponibles : List Moneda
                    monedasDisponibles =
                        resumen.netos
                            |> List.map Tuple.first
                            |> List.filter (\m -> m /= grupo.monedaPorDefecto)
                            |> (::) grupo.monedaPorDefecto

                    monedaSeleccionada : Moneda
                    monedaSeleccionada =
                        case model.monedaSeleccionada of
                            MonedaDefaultDelGrupo ->
                                grupo.monedaPorDefecto

                            MonedaSeleccionadaPorUsuario moneda ->
                                moneda

                    netosForActive : Maybe (Netos Api.Monto)
                    netosForActive =
                        resumen.netos
                            |> List.filter (\( m, _ ) -> m == monedaSeleccionada)
                            |> List.head
                            |> Maybe.map Tuple.second

                    transForActive : PorMoneda (List Transaccion)
                    transForActive =
                        resumen.transaccionesParaSaldar
                            |> List.filter (\( m, _ ) -> m == monedaSeleccionada)
                in
                div []
                    [ if resumen.cantidadPagosInvalidos > 0 then
                        Bs.alert Bs.AlertDanger
                            [ class "mb-3" ]
                            [ text <|
                                if resumen.cantidadPagosInvalidos == 1 then
                                    "Tenés 1 pago inválido, ese no se cuenta para las deudas."

                                else
                                    "Tenés "
                                        ++ String.fromInt resumen.cantidadPagosInvalidos
                                        ++ " pagos inválidos, esos no se cuentan para las deudas."
                            ]

                      else
                        text ""
                    , if resumen.isFrozen then
                        Bs.alert Bs.AlertWarning
                            [ class "mb-3" ]
                            [ text "Este grupo está congelado. Las deudas están fijas y no se pueden agregar, editar ni eliminar pagos." ]

                      else
                        text ""
                    , div [ class "mb-4" ]
                        [ div [ class "fw-bold mb-3" ] [ text "Netos" ]
                        , div [ class "row g-3" ]
                            [ div [ class "col-4" ] [ div [ class "card bg-secondary-subtle", style "height" "100px" ] [] ]
                            , div [ class "col-4" ] [ div [ class "card bg-secondary-subtle", style "height" "100px" ] [] ]
                            , div [ class "col-4" ] [ div [ class "card bg-secondary-subtle", style "height" "100px" ] [] ]
                            ]
                        ]
                    , Bs.card [ class "mb-4" ]
                        [ Bs.cardHeader [] [ text "Estado del grupo" ]
                        , Bs.cardBody []
                            [ if List.length monedasDisponibles > 1 then
                                viewMonedaSelector monedasDisponibles monedaSeleccionada

                              else
                                text ""
                            , case netosForActive of
                                Just netos ->
                                    viewNetosBarras grupo netos

                                Nothing ->
                                    text ""
                            ]
                        ]
                    , viewTransferencias hoy grupo.monedaPorDefecto grupo { resumen | transaccionesParaSaldar = transForActive }
                    ]


viewUltimosPagosCard : Store -> Model -> ShallowGrupo -> Html Msg
viewUltimosPagosCard store model grupo =
    case store |> Store.getPagos model.grupoId of
        Success pagos ->
            let
                ultimosPagos =
                    pagos
                        |> List.sortWith (\a b -> Date.compare b.fecha a.fecha)
                        |> List.take 5
            in
            Bs.card []
                [ Bs.cardHeader [] [ text "Ultimos pagos" ]
                , Bs.listGroup [ class "list-group-flush" ]
                    (ultimosPagos |> List.map (viewUltimoPago model.grupoId grupo.monedaPorDefecto))
                ]

        _ ->
            text ""


viewUltimoPago : ULID -> Moneda -> ShallowPago -> Html Msg
viewUltimoPago grupoId monedaPorDefecto pago =
    Bs.listGroupItem []
        [ div [ class "d-flex align-items-center gap-3" ]
            [ div
                [ class "text-center border rounded px-2 py-1 flex-shrink-0"
                , style "min-width" "2.5rem"
                ]
                [ div [ class "text-muted text-uppercase lh-1", style "font-size" "0.6em" ]
                    [ text (mesAbreviado pago.fecha) ]
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
            , button
                [ type_ "button"
                , class "btn btn-sm btn-outline-danger flex-shrink-0"
                , onClick (DeletePago pago.pagoId)
                ]
                [ i [ class "bi bi-trash" ] [] ]
            ]
        ]


mesAbreviado : Date.Date -> String
mesAbreviado date =
    case Date.monthNumber date of
        1 ->
            "ENE"

        2 ->
            "FEB"

        3 ->
            "MAR"

        4 ->
            "ABR"

        5 ->
            "MAY"

        6 ->
            "JUN"

        7 ->
            "JUL"

        8 ->
            "AGO"

        9 ->
            "SEP"

        10 ->
            "OCT"

        11 ->
            "NOV"

        12 ->
            "DIC"

        _ ->
            ""


viewMonedaSelector : List Moneda -> Moneda -> Html Msg
viewMonedaSelector monedas monedaSeleccionada =
    select
        [ class "form-select form-select-sm mb-3"
        , on "change"
            (Json.Decode.at [ "target", "value" ] jsonDecMoneda
                |> Json.Decode.map SelectMoneda
            )
        ]
        (monedas
            |> List.map
                (\m ->
                    option
                        [ value (Moneda.toString m)
                        , selected (monedaSeleccionada == m)
                        ]
                        [ text (Moneda.nombre m) ]
                )
        )


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
