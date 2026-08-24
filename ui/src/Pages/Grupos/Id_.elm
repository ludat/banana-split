module Pages.Grupos.Id_ exposing (Model, Msg, page)

import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.Bootstrap as Bs
import Components.Cotizaciones
import Components.MonedaSelector as MonedaSelector exposing (MonedaSeleccionada(..))
import Components.PagoDetalleModal as PagoDetalleModal
import Date
import Dict exposing (Dict)
import Effect exposing (Effect)
import Generated.Api as Api exposing (Moneda, Netos, ResumenConsolidado, ShallowGrupo, ShallowPago, ULID)
import Html exposing (Html, a, button, div, i, input, li, p, span, text, ul)
import Html.Attributes exposing (class, classList, disabled, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Layouts
import Models.Grupo exposing (GrupoLike, lookupNombreParticipante)
import Models.Moneda as Moneda
import Models.Monto as Monto
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Utils.Day
import Utils.Http exposing (viewHttpError)
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts
import View exposing (View)


page : Shared.Model -> Route { id : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route shared.store
        , update = update shared.store (PagoDetalleModal.context shared route)
        , subscriptions = subscriptions
        , view = view shared.store (Shared.currentParticipante shared route.params.id)
        }
        |> Page.withLayout (\_ -> Layouts.Default_Grupo {})
        |> Page.withOnUrlChanged (PagoModalMsg << PagoDetalleModal.onUrlChanged)


type alias Model =
    { grupoId : String
    , monedaSeleccionada : MonedaSeleccionada
    , pagoModal : PagoDetalleModal.Model
    , consolidarAbierto : Bool
    , cotizacionesInput : Dict String String
    , preview : WebData ResumenConsolidado
    , congelando : Bool
    }


init : Route { id : String } -> Store -> ( Model, Effect Msg )
init route store =
    let
        grupoId =
            route.params.id

        ( pagoModal, modalEffect ) =
            PagoDetalleModal.init route
    in
    ( { grupoId = grupoId
      , monedaSeleccionada = MonedaDefaultDelGrupo
      , pagoModal = pagoModal
      , consolidarAbierto = False
      , cotizacionesInput = Dict.empty
      , preview = NotAsked
      , congelando = False
      }
    , Effect.batch
        [ Store.ensureResumen grupoId store
        , Store.ensureGrupo grupoId store
        , Store.ensurePagos grupoId store
        , Effect.getCurrentUser grupoId
        , Effect.setUnsavedChangesWarning False
        , Effect.map PagoModalMsg modalEffect
        ]
    )


type Msg
    = SelectMoneda Moneda
    | OpenPago ULID
    | PagoModalMsg PagoDetalleModal.Msg
    | AbrirConsolidar
    | CotizacionInput Moneda String
    | VerConsolidado (Api.PorMoneda Api.Monto)
    | PreviewResponse (Result Http.Error ResumenConsolidado)
    | CongelarConsolidado (Api.PorMoneda Api.Monto)
    | CongelarConsolidadoResponse (Result Http.Error ShallowGrupo)


update : Store -> PagoDetalleModal.Context -> Msg -> Model -> ( Model, Effect Msg )
update store ctx msg model =
    case msg of
        SelectMoneda moneda ->
            ( { model
                | monedaSeleccionada = MonedaSeleccionadaPorUsuario moneda
                , consolidarAbierto = False
              }
            , Effect.none
            )

        AbrirConsolidar ->
            ( { model | consolidarAbierto = True }
            , Effect.none
            )

        CotizacionInput moneda valor ->
            -- Cambiar una cotización invalida el preview: el botón de congelar
            -- usa las cotizaciones del preview, no las de los inputs.
            ( { model
                | cotizacionesInput = Dict.insert (Moneda.toString moneda) valor model.cotizacionesInput
                , preview = NotAsked
              }
            , Effect.none
            )

        VerConsolidado cotizaciones ->
            ( { model | preview = Loading }
            , Effect.sendCmd <|
                Api.postGrupoByIdConsolidacionPreview
                    model.grupoId
                    { cotizaciones = cotizaciones }
                    PreviewResponse
            )

        PreviewResponse result ->
            ( { model | preview = RemoteData.fromResult result }
            , Effect.none
            )

        CongelarConsolidado cotizaciones ->
            ( { model | congelando = True }
            , Effect.sendCmd <|
                Api.postGrupoByIdFreeze
                    model.grupoId
                    { cotizaciones = cotizaciones }
                    CongelarConsolidadoResponse
            )

        CongelarConsolidadoResponse (Ok _) ->
            ( { model
                | congelando = False
                , consolidarAbierto = False
                , preview = NotAsked
              }
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshGrupo model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Grupo congelado con la liquidación consolidada"
                ]
            )

        CongelarConsolidadoResponse (Err _) ->
            ( { model | congelando = False }
            , Toasts.pushToast Toasts.ToastDanger "No se pudo congelar el grupo"
            )

        OpenPago pagoId ->
            let
                ( pagoModal, eff ) =
                    PagoDetalleModal.open ctx pagoId
            in
            ( { model | pagoModal = pagoModal }, Effect.map PagoModalMsg eff )

        PagoModalMsg subMsg ->
            let
                ( pagoModal, eff ) =
                    PagoDetalleModal.update ctx store subMsg model.pagoModal
            in
            ( { model | pagoModal = pagoModal }, Effect.map PagoModalMsg eff )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Store -> Maybe String -> Model -> View Msg
view store userId model =
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
                                [ viewLeftColumn store userId model grupo ]
                            , div [ class "col-lg-4" ]
                                [ viewUltimosPagosCard store model grupo ]
                            ]
                        ]
                , Html.map PagoModalMsg (PagoDetalleModal.view store grupo model.pagoModal)
                ]
            }


viewLeftColumn : Store -> Maybe String -> Model -> ShallowGrupo -> Html Msg
viewLeftColumn store userId model grupo =
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
                    alerts =
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
                        ]
                in
                case resumen.consolidacion of
                    -- Congelado consolidando monedas: una sola vista en la
                    -- moneda por defecto, sin tabs por moneda.
                    Just consolidacion ->
                        div []
                            (alerts
                                ++ [ div [ class "mb-3" ]
                                        [ Components.Cotizaciones.view consolidacion.moneda consolidacion.cotizaciones ]
                                   , viewNetosSection userId grupo consolidacion.moneda consolidacion.netos
                                   ]
                            )

                    Nothing ->
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

                            -- Consolidar solo tiene sentido con varias monedas
                            -- y con el grupo todavía sin congelar.
                            conConsolidar =
                                not resumen.isFrozen

                            consolidarActivo =
                                conConsolidar && model.consolidarAbierto
                        in
                        div []
                            (alerts
                                ++ [ if List.length monedasDisponibles > 1 then
                                        viewMonedaTabs userId resumen.netos monedasDisponibles grupo.monedaPorDefecto monedaSeleccionada conConsolidar consolidarActivo

                                     else
                                        text ""
                                   , if consolidarActivo then
                                        viewConsolidarPanel userId grupo resumen model

                                     else
                                        case
                                            resumen.netos
                                                |> List.filter (\( m, _ ) -> m == monedaSeleccionada)
                                                |> List.head
                                                |> Maybe.map Tuple.second
                                        of
                                            Just netos ->
                                                viewNetosSection userId grupo monedaSeleccionada netos

                                            Nothing ->
                                                text ""
                                   ]
                            )


{-| Las cards de netos y las barras de estado del grupo para una moneda.
-}
viewNetosSection : Maybe String -> ShallowGrupo -> Moneda -> Netos Api.Monto -> Html Msg
viewNetosSection userId grupo monedaSeleccionada netos =
    div [ class "pt-4 mb-4" ]
        [ div [ class "mb-4" ]
            [ div [ class "fw-bold mb-3" ] [ text "Netos" ]
            , div [ class "row g-3" ]
                [ div [ class "col-12 col-md-4" ]
                    [ viewTuEstadoCard userId netos grupo grupo.monedaPorDefecto monedaSeleccionada ]
                , div [ class "col-6 col-md-4" ]
                    [ viewNetoCard "Mayor pagador"
                        (netos |> List.sortBy (\( _, m ) -> Monto.toFloat m) |> List.reverse |> List.head)
                        grupo
                        grupo.monedaPorDefecto
                        monedaSeleccionada
                        False
                    ]
                , div [ class "col-6 col-md-4" ]
                    [ viewNetoCard "Mayor deudor"
                        (netos |> List.sortBy (\( _, m ) -> Monto.toFloat m) |> List.head)
                        grupo
                        grupo.monedaPorDefecto
                        monedaSeleccionada
                        False
                    ]
                ]
            ]
        , div [ class "fw-bold mb-3" ] [ text "Estado del grupo" ]
        , viewNetosBarras grupo netos
        ]


{-| El panel de la pestaña "Consolidar": inputs de cotizaciones, preview de la
liquidación consolidada y el botón para congelar con esas cotizaciones.
-}
viewConsolidarPanel : Maybe String -> ShallowGrupo -> Api.ResumenGrupo -> Model -> Html Msg
viewConsolidarPanel userId grupo resumen model =
    let
        monedasAConvertir : List Moneda
        monedasAConvertir =
            resumen.netos
                |> List.map Tuple.first
                |> List.filter (\m -> m /= grupo.monedaPorDefecto)

        rawInput moneda =
            model.cotizacionesInput
                |> Dict.get (Moneda.toString moneda)
                |> Maybe.withDefault ""

        parseCotizacion raw =
            Monto.fromString raw
                |> Maybe.andThen
                    (\monto ->
                        if monto.valor > 0 then
                            Just monto

                        else
                            Nothing
                    )

        cotizaciones : Maybe (Api.PorMoneda Api.Monto)
        cotizaciones =
            monedasAConvertir
                |> List.map (\m -> parseCotizacion (rawInput m) |> Maybe.map (Tuple.pair m))
                |> List.foldr (Maybe.map2 (::)) (Just [])

        viewCotizacionInput moneda =
            let
                raw =
                    rawInput moneda

                esInvalida =
                    raw /= "" && parseCotizacion raw == Nothing
            in
            div [ class "input-group mb-2", style "max-width" "22rem" ]
                [ span [ class "input-group-text" ]
                    [ text ("1 " ++ Moneda.simboloUnico moneda ++ " =") ]
                , input
                    [ type_ "text"
                    , class "form-control"
                    , classList [ ( "is-invalid", esInvalida ) ]
                    , value raw
                    , onInput (CotizacionInput moneda)
                    ]
                    []
                , span [ class "input-group-text" ]
                    [ text (Moneda.simbolo grupo.monedaPorDefecto grupo.monedaPorDefecto) ]
                ]
    in
    div [ class "pt-4 mb-4" ]
        [ p [ class "text-muted" ]
            [ text <|
                "Convertí todos los netos a "
                    ++ Moneda.nombre grupo.monedaPorDefecto
                    ++ " usando las cotizaciones que acuerden entre ustedes."
            ]
        , div [ class "mb-3" ] (monedasAConvertir |> List.map viewCotizacionInput)
        , case ( cotizaciones, model.preview ) of
            ( Just cotis, NotAsked ) ->
                Bs.btn Bs.Primary
                    [ onClick (VerConsolidado cotis) ]
                    [ text "Ver consolidado" ]

            ( Nothing, _ ) ->
                Bs.btn Bs.Primary
                    [ disabled True ]
                    [ text "Ver consolidado" ]

            ( Just _, _ ) ->
                text ""
        , case model.preview of
            NotAsked ->
                text ""

            Loading ->
                div [ class "text-muted" ] [ text "Calculando..." ]

            Failure e ->
                viewHttpError e

            Success preview ->
                div []
                    [ viewNetosSection userId grupo preview.moneda preview.netos
                    , div [ class "fw-bold mb-3" ] [ text "Transacciones para saldar" ]
                    , div [ class "mb-4" ]
                        (preview.transaccionesParaSaldar
                            |> List.map (viewTransaccionPreview grupo preview.moneda)
                        )
                    , Bs.btn Bs.Primary
                        [ onClick (CongelarConsolidado preview.cotizaciones)
                        , disabled model.congelando
                        ]
                        [ text "Congelar con estas cotizaciones" ]
                    , div [ class "text-muted small mt-2" ]
                        [ text "Congelar fija esta liquidación para todo el grupo. No se podrán agregar, editar ni eliminar pagos mientras esté congelado." ]
                    ]
        ]


viewTransaccionPreview : GrupoLike g -> Moneda -> Api.Transaccion -> Html Msg
viewTransaccionPreview grupo moneda transaccion =
    div
        [ style "display" "grid"
        , style "grid-template-columns" "1fr auto 1fr"
        , style "align-items" "center"
        , style "margin-bottom" "0.5rem"
        ]
        [ div [ class "text-end" ]
            [ div [] [ text (lookupNombreParticipante grupo transaccion.from) ]
            , div [ class "text-danger small" ]
                [ text (Moneda.simbolo moneda moneda ++ " " ++ Monto.toString transaccion.monto) ]
            ]
        , i [ class "bi bi-arrow-right", style "margin" "0 0.75rem" ] []
        , span [] [ text (lookupNombreParticipante grupo transaccion.to) ]
        ]


{-| Un neto mostrado como delta: el símbolo de la moneda apagado (para que no
compita) seguido del monto con signo y color (verde/rojo).
-}
viewMontoDelta : String -> Api.Monto -> Html Msg
viewMontoDelta simbolo monto =
    div [ class "small" ]
        [ span [ class "text-muted me-1" ] [ text simbolo ]
        , span [ class "fw-semibold" ] [ Monto.asDeltaHtml monto ]
        ]


viewMonedaTabs : Maybe String -> Api.PorMoneda (Netos Api.Monto) -> List Moneda -> Moneda -> Moneda -> Bool -> Bool -> Html Msg
viewMonedaTabs userId netosPorMoneda monedas monedaPorDefecto monedaSeleccionada conConsolidar consolidarActivo =
    let
        tab m =
            let
                active =
                    not consolidarActivo && m == monedaSeleccionada

                netoUsuario : Maybe Api.Monto
                netoUsuario =
                    userId
                        |> Maybe.andThen
                            (\uid ->
                                netosPorMoneda
                                    |> List.filter (\( mm, _ ) -> mm == m)
                                    |> List.head
                                    |> Maybe.map Tuple.second
                                    |> Maybe.andThen
                                        (\netos ->
                                            netos |> List.filter (\( id, _ ) -> id == uid) |> List.head
                                        )
                                    |> Maybe.map Tuple.second
                            )
            in
            li [ class "nav-item" ]
                [ button
                    [ type_ "button"
                    , classList [ ( "nav-link", True ), ( "active", active ) ]
                    , class "text-nowrap"
                    , onClick (SelectMoneda m)
                    ]
                    [ div [] [ text (Moneda.nombre m) ]
                    , case netoUsuario of
                        Just monto ->
                            viewMontoDelta (Moneda.simbolo monedaPorDefecto m) monto

                        Nothing ->
                            text ""
                    ]
                ]
    in
    -- On desktop these are plain nav-tabs. On mobile `.moneda-tabs` (see
    -- styles.css) makes them fill the width and scroll horizontally instead
    -- of wrapping.
    ul
        [ class "nav nav-tabs moneda-tabs" ]
        ((monedas |> List.map tab)
            ++ (if conConsolidar then
                    [ li [ class "nav-item" ]
                        [ button
                            [ type_ "button"
                            , classList [ ( "nav-link", True ), ( "active", consolidarActivo ) ]
                            , class "text-nowrap"
                            , onClick AbrirConsolidar
                            ]
                            [ div [] [ i [ class "bi bi-arrow-left-right me-1" ] [], text "Consolidar" ] ]
                        ]
                    ]

                else
                    []
               )
        )


viewUltimosPagosCard : Store -> Model -> ShallowGrupo -> Html Msg
viewUltimosPagosCard store model grupo =
    case store |> Store.getPagos model.grupoId of
        Success pagos ->
            let
                ultimosPagos =
                    pagos
                        |> List.sortWith (\p1 p2 -> compare p2.pagoId p1.pagoId)
                        |> List.take 5
            in
            Bs.card []
                [ Bs.cardHeader [] [ text "Ultimos pagos" ]
                , Bs.listGroup [ class "list-group-flush" ]
                    (ultimosPagos |> List.map (viewUltimoPago grupo.monedaPorDefecto))
                ]

        _ ->
            text ""


viewUltimoPago : Moneda -> ShallowPago -> Html Msg
viewUltimoPago monedaPorDefecto pago =
    Bs.listGroupItem
        [ class "list-group-item-action"
        , style "cursor" "pointer"
        , Html.Attributes.attribute "role" "button"
        , onClick (OpenPago pago.pagoId)
        ]
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
            ]
        ]


viewNetoCard : String -> Maybe ( String, Api.Monto ) -> GrupoLike g -> Moneda -> Moneda -> Bool -> Html Msg
viewNetoCard label maybeEntry grupo monedaPorDefecto monedaSeleccionada isCurrentUser =
    div
        [ class "card h-100"
        , if isCurrentUser then
            style "border-color" "var(--bs-primary)"

          else
            style "" ""
        ]
        [ div [ class "card-body d-flex flex-column justify-content-between p-3" ]
            [ div [ class "text-muted text-uppercase fw-semibold", style "font-size" "0.65rem", style "letter-spacing" "0.05em" ] [ text label ]
            , case maybeEntry of
                Just ( participanteId, monto ) ->
                    div []
                        [ div [ class "fw-semibold text-truncate" ]
                            [ text (lookupNombreParticipante grupo participanteId) ]
                        , viewMontoDelta (Moneda.simbolo monedaPorDefecto monedaSeleccionada) monto
                        ]

                Nothing ->
                    text ""
            ]
        ]


viewTuEstadoCard : Maybe String -> Api.Netos Api.Monto -> GrupoLike g -> Moneda -> Moneda -> Html Msg
viewTuEstadoCard userId netos grupo monedaPorDefecto monedaSeleccionada =
    let
        maybeEntry =
            userId
                |> Maybe.andThen
                    (\uid ->
                        netos |> List.filter (\( id, _ ) -> id == uid) |> List.head
                    )
    in
    div [ class "card h-100", style "border-color" "var(--bs-primary)" ]
        [ div [ class "card-body d-flex flex-column justify-content-between p-3" ]
            [ div [ class "text-muted text-uppercase fw-semibold", style "font-size" "0.65rem", style "letter-spacing" "0.05em" ] [ text "Tu estado actual" ]
            , case maybeEntry of
                Just ( _, monto ) ->
                    div []
                        [ viewMontoDelta (Moneda.simbolo monedaPorDefecto monedaSeleccionada) monto
                        , if monto.valor < 0 then
                            a
                                [ Path.href <| Path.Grupos_GrupoId__Liquidaciones { grupoId = grupo.id }
                                , class "small text-muted"
                                ]
                                [ text "Saldar deudas" ]

                          else
                            text ""
                        ]

                Nothing ->
                    div [ class "text-muted small" ] [ text "Seleccioná tu usuario" ]
            ]
        ]
