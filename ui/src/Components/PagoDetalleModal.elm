module Components.PagoDetalleModal exposing (Context, Model, Msg, Overlay, context, init, onUrlChanged, open, update, view)

import Components.BarrasDeNetos exposing (viewNetosBarras, viewNetosBarrasMini)
import Components.Bootstrap as Bs
import Components.GraficoTorta as GraficoTorta
import Dict
import Effect exposing (Effect)
import Generated.Api as Api exposing (ErrorResumen, Moneda, Monto, Pago, Parte(..), Repartija, ResumenPago, ShallowGrupo, TipoDistribucion(..), ULID)
import Html exposing (Html, a, button, div, h4, i, li, p, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, disabled, id, style, tabindex, type_)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as Decode
import Models.Grupo exposing (grupoIdFromPath, lookupNombreParticipante)
import Models.Moneda as Moneda
import Models.Monto as Monto
import Models.ResumenNetos exposing (errorMensaje, getDeudasFromResumen)
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Process
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Set
import Shared.Model
import Task
import Utils.Day as Day
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts



-- MODEL


type alias Model =
    { isOpen : Bool
    , pagoId : ULID
    , resumen : WebData ResumenPago
    , confirmingDelete : Bool
    , deleting : Bool
    , activeOverlay : Maybe Overlay
    , expandedErrors : Set.Set String
    }


type Overlay
    = BalanceGraphOverlay
    | PagadoresGraphOverlay
    | DeudoresGraphOverlay


type alias Context =
    { grupoId : ULID
    , path : Path.Path
    , origin : String
    }


context : Shared.Model.Model -> Route routeParams -> Context
context shared route =
    { grupoId = grupoIdFromPath route.path |> Maybe.withDefault ""
    , path = route.path
    , origin = shared.origin
    }


init : Route routeParams -> ( Model, Effect Msg )
init route =
    case Dict.get "pago" route.query of
        Just pagoId ->
            ( forPago True pagoId
            , loadPago pagoId
            )

        Nothing ->
            ( forPago False ""
            , Effect.none
            )


open : Context -> ULID -> ( Model, Effect Msg )
open ctx pagoId =
    ( forPago True pagoId
    , Effect.batch
        [ loadPago pagoId
        , syncUrl ctx.path (Just pagoId)
        ]
    )


onUrlChanged : { from : Route (), to : Route () } -> Msg
onUrlChanged { from, to } =
    let
        pagoParam route =
            Dict.get "pago" route.query
    in
    if pagoParam from == pagoParam to then
        NoOp

    else
        QueryChanged (pagoParam to)


forPago : Bool -> ULID -> Model
forPago isOpen pagoId =
    { isOpen = isOpen
    , pagoId = pagoId
    , resumen = Loading
    , confirmingDelete = False
    , deleting = False
    , activeOverlay = Nothing
    , expandedErrors = Set.empty
    }


loadPago : ULID -> Effect Msg
loadPago pagoId =
    Effect.batch
        [ Store.refreshPago pagoId
        , waitForPago
        ]


waitForPago : Effect Msg
waitForPago =
    Effect.sendCmd <| Task.perform (\_ -> CheckPagoPresent) (Process.sleep 100)


syncUrl : Path.Path -> Maybe ULID -> Effect Msg
syncUrl path maybePagoId =
    Effect.pushRoute
        { path = path
        , query =
            maybePagoId
                |> Maybe.map (Dict.singleton "pago")
                |> Maybe.withDefault Dict.empty
        , hash = Nothing
        }



-- UPDATE


type Msg
    = NoOp
    | Close
    | QueryChanged (Maybe ULID)
    | CheckPagoPresent
    | ResumenFetched (Result Http.Error ResumenPago)
    | OpenOverlay Overlay
    | CloseOverlay
    | ToggleErrors String
    | Share { title : String, path : Path.Path }
    | AskDelete
    | CancelDelete
    | ConfirmDelete
    | DeleteResponse (Result Http.Error ULID)


update : Context -> Store -> Msg -> Model -> ( Model, Effect Msg )
update ctx store msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        Close ->
            ( { model | isOpen = False }, syncUrl ctx.path Nothing )

        QueryChanged maybePagoId ->
            case maybePagoId of
                Just pagoId ->
                    if model.isOpen && pagoId == model.pagoId then
                        -- Already showing this pago (e.g. we just pushed the URL ourselves)
                        ( model, Effect.none )

                    else
                        ( forPago True pagoId, loadPago pagoId )

                Nothing ->
                    ( { model | isOpen = False }, Effect.none )

        Share { title, path } ->
            ( model
            , Effect.share { title = title, url = ctx.origin ++ Path.toString path }
            )

        CheckPagoPresent ->
            case Store.getPago model.pagoId store of
                Success pago ->
                    ( model
                    , Effect.sendCmd <| Api.postPagosResumen pago ResumenFetched
                    )

                Failure _ ->
                    ( model, Effect.none )

                _ ->
                    ( model, waitForPago )

        ResumenFetched result ->
            ( { model | resumen = RemoteData.fromResult result }
            , Effect.none
            )

        OpenOverlay overlay ->
            ( { model | activeOverlay = Just overlay }, Effect.none )

        CloseOverlay ->
            ( { model | activeOverlay = Nothing }, Effect.none )

        ToggleErrors key ->
            ( { model
                | expandedErrors =
                    if Set.member key model.expandedErrors then
                        Set.remove key model.expandedErrors

                    else
                        Set.insert key model.expandedErrors
              }
            , Effect.none
            )

        AskDelete ->
            ( { model | confirmingDelete = True }, Effect.none )

        CancelDelete ->
            ( { model | confirmingDelete = False }, Effect.none )

        ConfirmDelete ->
            ( { model | deleting = True }
            , Effect.sendCmd <| Api.deleteGrupoByIdPagosByPagoId ctx.grupoId model.pagoId DeleteResponse
            )

        DeleteResponse (Ok _) ->
            ( { model | isOpen = False }
            , Effect.batch
                [ Store.refreshGrupo ctx.grupoId
                , Store.refreshResumen ctx.grupoId
                , Store.refreshPagos ctx.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Pago borrado"
                , syncUrl ctx.path Nothing
                ]
            )

        DeleteResponse (Err _) ->
            ( { model | deleting = False, confirmingDelete = False }
            , Toasts.pushToast Toasts.ToastDanger "Falló al borrar el pago"
            )



-- VIEW


view : Store -> ShallowGrupo -> Model -> Html Msg
view store grupo model =
    if not model.isOpen then
        text ""

    else
        let
            ( header, content, overlays ) =
                case ( Store.getPago model.pagoId store, model.resumen ) of
                    ( Success pago, Success resumen ) ->
                        ( viewHeader grupo pago resumen model
                        , viewContent grupo pago resumen model
                        , viewOverlay grupo pago resumen model
                        )

                    ( Failure _, _ ) ->
                        viewEstado
                            "bi bi-receipt-cutoff"
                            "No encontramos este pago"
                            "Puede que lo hayan eliminado o que el enlace ya no sea válido."

                    ( _, Failure _ ) ->
                        viewEstado
                            "bi bi-exclamation-triangle"
                            "No pudimos cargar los detalles"
                            "Hubo un problema al calcular el reparto. Probá de nuevo en un momento."

                    _ ->
                        ( text ""
                        , div [ class "text-center py-3" ] [ Bs.spinner [] ]
                        , text ""
                        )
        in
        div []
            [ div
                [ class "modal d-block fade show"
                , id modalOverlayId
                , tabindex -1
                , attribute "aria-modal" "true"
                , attribute "role" "dialog"
                , on "click" closeOnOverlayClick
                ]
                [ div [ class "modal-dialog modal-dialog-scrollable" ]
                    [ div [ class "modal-content" ]
                        [ header
                        , div [ class "modal-body" ] [ content ]
                        ]
                    ]
                ]
            , div [ class "modal-backdrop show" ] []
            , overlays
            ]


viewEstado : String -> String -> String -> ( Html Msg, Html Msg, Html Msg )
viewEstado icono titulo mensaje =
    ( div [ class "modal-header border-bottom-0" ]
        [ button
            [ type_ "button"
            , class "btn-close ms-auto"
            , attribute "aria-label" "Cerrar"
            , onClick Close
            ]
            []
        ]
    , div [ class "text-center px-3 pb-4" ]
        [ i [ class (icono ++ " text-secondary"), style "font-size" "3rem" ] []
        , h4 [ class "mt-3 mb-1 fw-bold" ] [ text titulo ]
        , p [ class "text-muted mb-0" ] [ text mensaje ]
        ]
    , text ""
    )


modalOverlayId : String
modalOverlayId =
    "pago-detalle-modal"


closeOnOverlayClick : Decode.Decoder Msg
closeOnOverlayClick =
    Decode.map2
        (\targetId currentId ->
            if targetId == currentId then
                Close

            else
                NoOp
        )
        (Decode.at [ "target", "id" ] Decode.string)
        (Decode.at [ "currentTarget", "id" ] Decode.string)


overlayId : String
overlayId =
    "pago-detalle-overlay-modal"


closeOverlayOnBackdropClick : Decode.Decoder Msg
closeOverlayOnBackdropClick =
    Decode.map2
        (\targetId currentId ->
            if targetId == currentId then
                CloseOverlay

            else
                NoOp
        )
        (Decode.at [ "target", "id" ] Decode.string)
        (Decode.at [ "currentTarget", "id" ] Decode.string)


viewOverlay : ShallowGrupo -> Pago -> ResumenPago -> Model -> Html Msg
viewOverlay grupo pago resumen model =
    case model.activeOverlay of
        Nothing ->
            text ""

        Just overlay ->
            let
                torta netos =
                    GraficoTorta.viewTortaGrande (GraficoTorta.porciones grupo (Just pago.monto) netos)

                ( titulo, cuerpo ) =
                    case overlay of
                        BalanceGraphOverlay ->
                            ( "Balance", viewBalance grupo resumen )

                        PagadoresGraphOverlay ->
                            ( "Pago", torta resumen.resumenPagadores )

                        DeudoresGraphOverlay ->
                            ( "Reparto", torta resumen.resumenDeudores )
            in
            div []
                [ div
                    [ class "modal d-block fade show"
                    , id overlayId
                    , style "z-index" "1070"
                    , tabindex -1
                    , attribute "aria-modal" "true"
                    , attribute "role" "dialog"
                    , on "click" closeOverlayOnBackdropClick
                    ]
                    [ div [ class "modal-dialog modal-dialog-centered modal-dialog-scrollable" ]
                        [ div [ class "modal-content" ]
                            [ div [ class "modal-header" ]
                                [ h4 [ class "modal-title fw-bold" ] [ text titulo ]
                                , button
                                    [ type_ "button"
                                    , class "btn-close"
                                    , attribute "aria-label" "Cerrar"
                                    , onClick CloseOverlay
                                    ]
                                    []
                                ]
                            , div [ class "modal-body" ] [ cuerpo ]
                            ]
                        ]
                    ]
                , div [ class "modal-backdrop show", style "z-index" "1065" ] []
                ]


viewHeader : ShallowGrupo -> Pago -> ResumenPago -> Model -> Html Msg
viewHeader grupo pago resumen model =
    div [ class "modal-header flex-column align-items-stretch border-bottom-0 pb-0" ]
        [ div [ class "d-flex justify-content-between align-items-start" ]
            [ div []
                [ div [ class "text-muted text-uppercase fw-semibold small" ] [ text "Gasto compartido" ]
                , h4 [ class "modal-title mb-0 fw-bold" ]
                    [ text <| pago.nombre
                    , errorToggle "gasto" model.expandedErrors resumen.resumen.errores
                    ]
                , errorList "gasto" model.expandedErrors resumen.resumen.errores
                ]
            , button
                [ type_ "button"
                , class "btn-close"
                , attribute "aria-label" "Cerrar"
                , onClick Close
                ]
                []
            ]
        , viewActions grupo pago model
        ]


viewActions : ShallowGrupo -> Pago -> Model -> Html Msg
viewActions grupo pago model =
    div [ class "d-flex align-items-center gap-2 mt-3" ]
        [ a
            [ Path.href <| Path.Grupos_GrupoId__Pagos_PagoId_ { grupoId = grupo.id, pagoId = pago.pagoId }
            , class "btn btn-secondary rounded-pill px-4"
            ]
            [ text "Editar" ]
        , viewActionsMenu model
        ]


viewActionsMenu : Model -> Html Msg
viewActionsMenu model =
    div [ class "dropdown" ]
        [ button
            [ type_ "button"
            , class "btn btn-light rounded-circle d-flex align-items-center justify-content-center"
            , style "width" "2.5rem"
            , style "height" "2.5rem"
            , attribute "aria-label" "Más acciones"
            , attribute "data-bs-toggle" "dropdown"
            , attribute "data-bs-auto-close" "outside"
            , attribute "aria-expanded" "false"
            ]
            [ i [ class "bi bi-three-dots-vertical" ] [] ]
        , div [ class "dropdown-menu dropdown-menu-end" ]
            (if model.confirmingDelete then
                [ div [ class "px-3 py-2" ]
                    [ div [ class "small text-danger mb-2" ] [ text "¿Eliminar este pago? No se puede deshacer." ]
                    , div [ class "d-flex gap-2" ]
                        [ Bs.btn Bs.Transparent
                            [ onClick CancelDelete, disabled model.deleting ]
                            [ text "Cancelar" ]
                        , Bs.btn Bs.Danger
                            [ onClick ConfirmDelete, disabled model.deleting ]
                            [ text "Eliminar" ]
                        ]
                    ]
                ]

             else
                [ button
                    [ type_ "button"
                    , class "dropdown-item text-danger"
                    , onClick AskDelete
                    ]
                    [ i [ class "bi bi-trash me-2" ] [], text "Eliminar" ]
                ]
            )
        ]


viewContent : ShallowGrupo -> Pago -> ResumenPago -> Model -> Html Msg
viewContent grupo pago resumen model =
    div []
        [ viewInfo grupo pago resumen
        , viewPago grupo pago resumen model
        , viewReparto grupo pago resumen model
        ]


errorToggle : String -> Set.Set String -> List ErrorResumen -> Html Msg
errorToggle key expanded errores =
    if List.isEmpty errores then
        text ""

    else
        button
            [ type_ "button"
            , class "btn btn-link p-0 ms-2 align-baseline text-warning"
            , attribute "aria-label" "Mostrar errores"
            , attribute "aria-expanded"
                (if Set.member key expanded then
                    "true"

                 else
                    "false"
                )
            , onClick (ToggleErrors key)
            ]
            [ i [ class "bi bi-exclamation-triangle-fill" ] [] ]


errorList : String -> Set.Set String -> List ErrorResumen -> Html Msg
errorList key expanded errores =
    if Set.member key expanded && not (List.isEmpty errores) then
        Bs.alert Bs.AlertWarning
            [ class "py-2 mt-2 mb-0 small" ]
            [ ul [ class "mb-0 ps-3" ]
                (errores |> List.map (\error -> li [] [ text (errorMensaje error.tipo) ]))
            ]

    else
        text ""


viewInfo : ShallowGrupo -> Pago -> ResumenPago -> Html Msg
viewInfo grupo pago resumen =
    div [ class "d-flex justify-content-between align-items-start gap-3 flex-wrap mb-2" ]
        [ div []
            [ div [ class "text-muted text-uppercase fw-semibold small" ] [ text "Monto" ]
            , div [ class "fs-4 fw-bold mb-2" ]
                [ text (Moneda.simbolo grupo.monedaPorDefecto pago.moneda ++ " " ++ Monto.toString pago.monto) ]
            , div [ class "text-muted text-uppercase fw-semibold small" ] [ text "Fecha" ]
            , div [ class "fw-bold" ] [ text (Day.toString pago.fecha) ]
            ]
        , button
            [ type_ "button"
            , class "btn p-0 border-0"
            , attribute "aria-label" "Ver balance"
            , onClick (OpenOverlay BalanceGraphOverlay)
            ]
            [ viewNetosBarrasMini resumen.resumen.netos ]
        ]


viewBalance : ShallowGrupo -> ResumenPago -> Html Msg
viewBalance grupo resumen =
    case getDeudasFromResumen resumen.resumen of
        Just netos ->
            viewNetosBarras grupo netos

        Nothing ->
            div [ class "text-muted small" ] [ text "Sin balance para mostrar." ]


viewReparto : ShallowGrupo -> Pago -> ResumenPago -> Model -> Html Msg
viewReparto grupo pago resumen model =
    Bs.card [ class "mt-3" ]
        [ Bs.cardBody []
            [ div [ class "d-flex justify-content-between align-items-start mb-2" ]
                [ div [ class "text-muted text-uppercase fw-semibold small" ]
                    [ text "Reparto", errorToggle "reparto" model.expandedErrors resumen.resumenDeudores.errores ]
                , button
                    [ type_ "button"
                    , class "btn p-0 border-0"
                    , attribute "aria-label" "Ver gráfico del reparto"
                    , onClick (OpenOverlay DeudoresGraphOverlay)
                    ]
                    [ GraficoTorta.viewTortaMini
                        (GraficoTorta.porciones grupo (Just pago.monto) resumen.resumenDeudores)
                    ]
                ]
            , errorList "reparto" model.expandedErrors resumen.resumenDeudores.errores
            , case pago.deudores.tipo of
                TipoDistribucionPartes dp ->
                    viewRepartoClasico grupo pago.moneda dp.partes

                TipoDistribucionRepartija repartija ->
                    viewRepartoRepartija grupo repartija
            ]
        ]


type alias ParteData =
    { participanteId : ULID
    , monto : Monto
    , division : Int
    }


parteData : Parte -> ParteData
parteData parte =
    case parte of
        MontoFijo m p ->
            { participanteId = p, monto = m, division = 0 }

        Ponderado n p ->
            { participanteId = p, monto = Monto.zero, division = n }

        PonderadoYMontoFijo m n p ->
            { participanteId = p, monto = m, division = n }


viewRepartoClasico : ShallowGrupo -> Moneda -> List Parte -> Html Msg
viewRepartoClasico grupo moneda partes =
    div []
        [ div [ class "d-flex align-items-center gap-2 mb-3" ]
            [ i [ class "bi bi-pie-chart-fill fs-5" ] []
            , span [ class "fs-5 fw-semibold" ] [ text "Clásico" ]
            ]
        , viewPartesTabla grupo moneda "Nadie participa de este reparto." partes
        ]


viewPartesTabla : ShallowGrupo -> Moneda -> String -> List Parte -> Html Msg
viewPartesTabla grupo moneda vacioMsg partes =
    let
        parsed =
            List.map parteData partes
    in
    if List.isEmpty parsed then
        div [ class "text-muted small" ] [ text vacioMsg ]

    else
        let
            hayMontos =
                List.any (\p -> p.monto.valor /= 0) parsed

            partesIguales =
                List.all (\p -> p.division == 1) parsed
        in
        if not hayMontos && partesIguales then
            viewPartesIguales grupo parsed

        else
            let
                hayDivisiones =
                    List.any (\p -> p.division /= 0) parsed
            in
            viewClasicoTabla grupo moneda hayMontos hayDivisiones parsed


viewPartesIguales : ShallowGrupo -> List ParteData -> Html Msg
viewPartesIguales grupo parsed =
    table [ class "table mb-0 align-middle" ]
        [ thead []
            [ tr []
                [ th [] [ text "Participante" ]
                , th [ class "text-end" ] [ text "Partes" ]
                ]
            ]
        , tbody []
            (parsed
                |> List.map
                    (\p ->
                        tr []
                            [ td [ class "fw-semibold" ] [ text (lookupNombreParticipante grupo p.participanteId) ]
                            , td [ class "text-end" ] [ text "1 parte" ]
                            ]
                    )
            )
        ]


viewClasicoTabla : ShallowGrupo -> Moneda -> Bool -> Bool -> List ParteData -> Html Msg
viewClasicoTabla grupo moneda hayMontos hayDivisiones parsed =
    let
        cuandoMontos cell =
            if hayMontos then
                [ cell ]

            else
                []

        cuandoDivisiones cell =
            if hayDivisiones then
                [ cell ]

            else
                []
    in
    table [ class "table mb-0 align-middle" ]
        [ thead []
            [ tr []
                (th [] [ text "Participante" ]
                    :: (cuandoMontos (th [ class "text-end" ] [ text "Monto fijo" ])
                            ++ cuandoDivisiones (th [ class "text-end" ] [ text "Partes" ])
                       )
                )
            ]
        , tbody []
            (parsed
                |> List.map
                    (\p ->
                        tr []
                            (td [ class "fw-semibold" ] [ text (lookupNombreParticipante grupo p.participanteId) ]
                                :: (cuandoMontos
                                        (td [ class "text-end" ]
                                            [ text <| Moneda.simbolo grupo.monedaPorDefecto moneda ++ " " ++ Monto.toString p.monto
                                            ]
                                        )
                                        ++ cuandoDivisiones (td [ class "text-end" ] [ text (String.fromInt p.division) ])
                                   )
                            )
                    )
            )
        ]


viewRepartoRepartija : ShallowGrupo -> Repartija -> Html Msg
viewRepartoRepartija grupo repartija =
    let
        claimants =
            repartija.claims
                |> List.map .participante
                |> Set.fromList

        claimedItemIds =
            repartija.claims |> List.map .itemId |> Set.fromList

        itemsSinReclamar =
            repartija.items |> List.filter (\item -> not (Set.member item.id claimedItemIds))
    in
    div []
        [ div [ class "d-flex align-items-center gap-2 mb-2" ]
            [ i [ class "bi bi-people-fill fs-5" ] []
            , span [ class "fs-5 fw-semibold" ] [ text "Repartija" ]
            ]
        , p [ class "text-muted small mb-2" ]
            [ text "La división se realiza para aquellos participantes que hayan reclamado los items que le corresponden pagar." ]
        , let
            repartijaPath =
                Path.Grupos_GrupoId__Repartijas_RepartijaId_ { grupoId = grupo.id, repartijaId = repartija.id }
          in
          div [ class "d-flex flex-wrap gap-2" ]
            [ a
                [ Path.href repartijaPath
                , class "btn btn-secondary rounded-pill"
                ]
                [ i [ class "bi bi-card-checklist me-1" ] [], text "Reclamar items" ]
            , Bs.btn Bs.Primary
                [ class "rounded-pill"
                , onClick (Share { title = repartija.nombre, path = repartijaPath })
                ]
                [ i [ class "bi bi-share me-1" ] [], text "Invitar a colaborar" ]
            ]
        , div [ class "mt-3" ]
            [ if Set.isEmpty claimants then
                div [ class "text-muted small" ] [ text "Nadie reclamó items todavía." ]

              else
                let
                    nombres =
                        claimants
                            |> Set.toList
                            |> List.map (lookupNombreParticipante grupo)
                            |> String.join ", "
                in
                div []
                    [ div []
                        [ Bs.badge "text-bg-secondary me-1" [] [ text (String.fromInt (Set.size claimants)) ]
                        , text "Participantes reclamaron items"
                        ]
                    , div [ class "text-muted small" ] [ text nombres ]
                    ]
            ]
        , if List.isEmpty itemsSinReclamar then
            text ""

          else
            Bs.alert Bs.AlertWarning
                [ class "py-2 mt-3 mb-0 small" ]
                [ i [ class "bi bi-exclamation-triangle-fill me-1" ] []
                , text "Aún hay ítems sin reclamar"
                ]
        ]


viewPago : ShallowGrupo -> Pago -> ResumenPago -> Model -> Html Msg
viewPago grupo pago resumen model =
    Bs.card [ class "mt-3" ]
        [ Bs.cardBody []
            [ div [ class "d-flex justify-content-between align-items-start mb-2" ]
                [ div [ class "text-muted text-uppercase fw-semibold small" ]
                    [ text "Pago", errorToggle "pago" model.expandedErrors resumen.resumenPagadores.errores ]
                , button
                    [ type_ "button"
                    , class "btn p-0 border-0"
                    , attribute "aria-label" "Ver gráfico del reparto"
                    , onClick (OpenOverlay PagadoresGraphOverlay)
                    ]
                    [ GraficoTorta.viewTortaMini
                        (GraficoTorta.porciones grupo (Just pago.monto) resumen.resumenPagadores)
                    ]
                ]
            , errorList "pago" model.expandedErrors resumen.resumenPagadores.errores
            , case pago.pagadores.tipo of
                TipoDistribucionPartes dp ->
                    viewPartesTabla grupo pago.moneda "Nadie figura como pagador." dp.partes

                TipoDistribucionRepartija _ ->
                    div [ class "text-muted small" ] [ text "Repartija colaborativa" ]
            ]
        ]
