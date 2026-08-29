module Pages.Grupos.Id_ exposing (Model, Msg, Tab, page)

import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.Bootstrap as Bs
import Components.PagoDetalleModal as PagoDetalleModal
import Date
import Effect exposing (Effect)
import Generated.Api as Api exposing (Moneda, Netos, ResumenGrupo, ShallowGrupo, ShallowPago, ULID)
import Html exposing (Html, a, button, div, i, li, p, span, text, ul)
import Html.Attributes exposing (class, classList, style, type_)
import Html.Events exposing (onClick)
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
import Utils.Day
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
    , tabSeleccionado : Maybe Tab
    , pagoModal : PagoDetalleModal.Model
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
      , tabSeleccionado = Nothing
      , pagoModal = pagoModal
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


type Tab
    = TabMoneda Moneda
    | TabTotal


tabActivo : Maybe Tab -> Tab
tabActivo seleccionado =
    seleccionado |> Maybe.withDefault TabTotal


type Msg
    = SelectTab Tab
    | OpenPago ULID
    | PagoModalMsg PagoDetalleModal.Msg


update : Store -> PagoDetalleModal.Context -> Msg -> Model -> ( Model, Effect Msg )
update store ctx msg model =
    case msg of
        SelectTab tab ->
            ( { model | tabSeleccionado = Just tab }
            , Effect.none
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
                    monedasDisponibles : List Moneda
                    monedasDisponibles =
                        resumen.netos
                            |> List.map Tuple.first
                            |> List.filter (\m -> m /= grupo.monedaPorDefecto)
                            |> (::) grupo.monedaPorDefecto

                    tabActual : Tab
                    tabActual =
                        tabActivo model.tabSeleccionado

                    ( monedaMostrada, netosMostrados ) =
                        case tabActual of
                            TabTotal ->
                                ( resumen.consolidado.moneda
                                , Just resumen.consolidado.netos
                                )

                            TabMoneda moneda ->
                                ( moneda
                                , resumen.netos
                                    |> List.filter (\( m, _ ) -> m == moneda)
                                    |> List.head
                                    |> Maybe.map Tuple.second
                                )
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
                    , if List.length monedasDisponibles > 1 then
                        viewTabs userId resumen monedasDisponibles grupo.monedaPorDefecto tabActual

                      else
                        text ""
                    , case tabActual of
                        TabTotal ->
                            viewAvisoMonedasSinTasa grupo resumen

                        TabMoneda _ ->
                            text ""
                    , div [ class "pt-4 mb-4" ]
                        [ div [ class "mb-4" ]
                            [ div [ class "fw-bold mb-3" ] [ text "Netos" ]
                            , case netosMostrados of
                                Just netos ->
                                    div [ class "row g-3" ]
                                        [ div [ class "col-12 col-md-4" ]
                                            [ viewTuEstadoCard userId netos grupo grupo.monedaPorDefecto monedaMostrada ]
                                        , div [ class "col-6 col-md-4" ]
                                            [ viewNetoCard "Mayor pagador"
                                                (netos |> List.sortBy (\( _, m ) -> Monto.toFloat m) |> List.reverse |> List.head)
                                                grupo
                                                grupo.monedaPorDefecto
                                                monedaMostrada
                                                False
                                            ]
                                        , div [ class "col-6 col-md-4" ]
                                            [ viewNetoCard "Mayor deudor"
                                                (netos |> List.sortBy (\( _, m ) -> Monto.toFloat m) |> List.head)
                                                grupo
                                                grupo.monedaPorDefecto
                                                monedaMostrada
                                                False
                                            ]
                                        ]

                                Nothing ->
                                    text ""
                            ]
                        , div [ class "fw-bold mb-3" ] [ text "Estado del grupo" ]
                        , case netosMostrados of
                            Just netos ->
                                viewNetosBarras grupo netos

                            Nothing ->
                                text ""
                        ]
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


viewAvisoMonedasSinTasa : ShallowGrupo -> ResumenGrupo -> Html Msg
viewAvisoMonedasSinTasa grupo resumen =
    case resumen.consolidado.monedasSinTasa of
        [] ->
            text ""

        monedas ->
            Bs.alert Bs.AlertWarning
                [ class "mt-3 mb-0" ]
                [ text <|
                    (if List.length monedas == 1 then
                        "Falta la tasa de cambio de "

                     else
                        "Faltan las tasas de cambio de "
                    )
                        ++ (monedas |> List.map Moneda.nombre |> String.join ", ")
                        ++ ", así que esas deudas no entran en el total. Cargalas "
                , a [ Path.href <| Path.Grupos_GrupoId__Settings { grupoId = grupo.id } ]
                    [ text "en los ajustes del grupo" ]
                , text "."
                ]


viewTabs : Maybe String -> ResumenGrupo -> List Moneda -> Moneda -> Tab -> Html Msg
viewTabs userId resumen monedas monedaPorDefecto tabActual =
    let
        netoDe : Netos Api.Monto -> Maybe Api.Monto
        netoDe netos =
            userId
                |> Maybe.andThen
                    (\uid -> netos |> List.filter (\( id, _ ) -> id == uid) |> List.head)
                |> Maybe.map Tuple.second

        tabDeMoneda : Moneda -> Html Msg
        tabDeMoneda moneda =
            viewTab (TabMoneda moneda)
                tabActual
                (Moneda.nombre moneda)
                (Moneda.simbolo monedaPorDefecto moneda)
                (resumen.netos
                    |> List.filter (\( m, _ ) -> m == moneda)
                    |> List.head
                    |> Maybe.map Tuple.second
                    |> Maybe.andThen netoDe
                )

        tabDelTotal : Html Msg
        tabDelTotal =
            viewTab TabTotal
                tabActual
                "Total"
                (Moneda.simbolo monedaPorDefecto resumen.consolidado.moneda)
                (netoDe resumen.consolidado.netos)
    in
    -- On desktop these are plain nav-tabs. On mobile `.moneda-tabs` (see
    -- styles.css) makes them fill the width and scroll horizontally instead
    -- of wrapping.
    ul
        [ class "nav nav-tabs moneda-tabs" ]
        (tabDelTotal :: (monedas |> List.map tabDeMoneda))


viewTab : Tab -> Tab -> String -> String -> Maybe Api.Monto -> Html Msg
viewTab tab tabActual etiqueta simbolo netoUsuario =
    li [ class "nav-item" ]
        [ button
            [ type_ "button"
            , classList [ ( "nav-link", True ), ( "active", tab == tabActual ) ]
            , class "text-nowrap"
            , onClick (SelectTab tab)
            ]
            [ div [] [ text etiqueta ]
            , case netoUsuario of
                Just monto ->
                    viewMontoDelta simbolo monto

                Nothing ->
                    text ""
            ]
        ]


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
