module Components.PagoDetalleModal exposing (Model, Msg, State, init, open, update, view)

import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.Bootstrap as Bs
import Dict
import Effect exposing (Effect)
import Generated.Api as Api exposing (Distribucion, Moneda, Pago, Parte(..), ResumenPago, ShallowGrupo, TipoDistribucion(..), ULID)
import Html exposing (Html, a, button, div, i, li, span, strong, text, ul)
import Html.Attributes exposing (class, disabled, type_)
import Html.Events exposing (onClick)
import Http
import Models.Grupo exposing (lookupNombreParticipante)
import Models.Moneda as Moneda
import Models.Monto as Monto
import Models.ResumenNetos exposing (getDeudasFromResumen)
import Models.Store as Store
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Utils.Day as Day
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts



-- MODEL


type alias Model =
    { path : Path.Path
    , grupoId : ULID
    , selected : Maybe State
    }


type alias State =
    { pagoId : ULID
    , pago : WebData Pago
    , resumen : WebData ResumenPago
    , confirmingDelete : Bool
    , deleting : Bool
    }


init : Route routeParams -> ULID -> ( Model, Effect Msg )
init route grupoId =
    case Dict.get "pago" route.query of
        Just pagoId ->
            ( { path = route.path, grupoId = grupoId, selected = Just (newState pagoId) }
            , fetchPago grupoId pagoId
            )

        Nothing ->
            ( { path = route.path, grupoId = grupoId, selected = Nothing }
            , Effect.none
            )


open : ULID -> Model -> ( Model, Effect Msg )
open pagoId model =
    ( { model | selected = Just (newState pagoId) }
    , Effect.batch
        [ fetchPago model.grupoId pagoId
        , syncUrl model.path (Just pagoId)
        ]
    )


newState : ULID -> State
newState pagoId =
    { pagoId = pagoId
    , pago = Loading
    , resumen = Loading
    , confirmingDelete = False
    , deleting = False
    }


fetchPago : ULID -> ULID -> Effect Msg
fetchPago grupoId pagoId =
    Effect.sendCmd <| Api.getGrupoByIdPagosByPagoId grupoId pagoId PagoFetched


syncUrl : Path.Path -> Maybe ULID -> Effect Msg
syncUrl path maybePagoId =
    Effect.replaceRoute
        { path = path
        , query =
            maybePagoId
                |> Maybe.map (Dict.singleton "pago")
                |> Maybe.withDefault Dict.empty
        , hash = Nothing
        }



-- UPDATE


type Msg
    = Close
    | PagoFetched (Result Http.Error Pago)
    | ResumenFetched (Result Http.Error ResumenPago)
    | AskDelete
    | CancelDelete
    | ConfirmDelete
    | DeleteResponse (Result Http.Error ULID)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case model.selected of
        Nothing ->
            ( model, Effect.none )

        Just state ->
            let
                setState newSelected =
                    { model | selected = Just newSelected }
            in
            case msg of
                Close ->
                    ( { model | selected = Nothing }, syncUrl model.path Nothing )

                PagoFetched result ->
                    ( setState { state | pago = RemoteData.fromResult result }
                    , case result of
                        Ok pago ->
                            Effect.sendCmd <| Api.postPagosResumen pago ResumenFetched

                        Err _ ->
                            Effect.none
                    )

                ResumenFetched result ->
                    ( setState { state | resumen = RemoteData.fromResult result }
                    , Effect.none
                    )

                AskDelete ->
                    ( setState { state | confirmingDelete = True }, Effect.none )

                CancelDelete ->
                    ( setState { state | confirmingDelete = False }, Effect.none )

                ConfirmDelete ->
                    ( setState { state | deleting = True }
                    , Effect.sendCmd <| Api.deleteGrupoByIdPagosByPagoId model.grupoId state.pagoId DeleteResponse
                    )

                DeleteResponse (Ok _) ->
                    ( { model | selected = Nothing }
                    , Effect.batch
                        [ Store.refreshGrupo model.grupoId
                        , Store.refreshResumen model.grupoId
                        , Store.refreshPagos model.grupoId
                        , Toasts.pushToast Toasts.ToastSuccess "Pago borrado"
                        , syncUrl model.path Nothing
                        ]
                    )

                DeleteResponse (Err _) ->
                    ( setState { state | deleting = False, confirmingDelete = False }
                    , Toasts.pushToast Toasts.ToastDanger "Falló al borrar el pago"
                    )



-- VIEW


view : ShallowGrupo -> Model -> Html Msg
view grupo model =
    case model.selected of
        Nothing ->
            Bs.modal { isOpen = False, onClose = Close, title = "", body = [], footer = [] }

        Just state ->
            Bs.modal
                { isOpen = True
                , onClose = Close
                , title =
                    state.pago
                        |> RemoteData.toMaybe
                        |> Maybe.map .nombre
                        |> Maybe.withDefault "Detalle del pago"
                , body = [ viewDetalle grupo state ]
                , footer = viewFooter grupo state
                }


viewFooter : ShallowGrupo -> State -> List (Html Msg)
viewFooter grupo state =
    if state.confirmingDelete then
        [ span [ class "me-auto text-danger small" ] [ text "¿Eliminar este pago? No se puede deshacer." ]
        , Bs.btn Bs.Transparent [ onClick CancelDelete ] [ text "Cancelar" ]
        , Bs.btn Bs.Danger
            [ onClick ConfirmDelete, disabled state.deleting ]
            [ text "Eliminar" ]
        ]

    else
        [ Bs.btn Bs.Danger
            [ class "me-auto", onClick AskDelete ]
            [ i [ class "bi bi-trash me-1" ] [], text "Eliminar" ]
        , a
            [ Path.href <| Path.Grupos_GrupoId__Pagos_PagoId_ { grupoId = grupo.id, pagoId = state.pagoId }
            , class "btn btn-primary"
            ]
            [ i [ class "bi bi-pencil me-1" ] [], text "Editar" ]
        , button [ type_ "button", class "btn btn-secondary", onClick Close ] [ text "Cerrar" ]
        ]


viewDetalle : ShallowGrupo -> State -> Html Msg
viewDetalle grupo state =
    case state.pago of
        Success pago ->
            div []
                [ viewDetalleFila "Fecha" (Day.toString pago.fecha)
                , viewDetalleFila "Total"
                    (Moneda.simbolo grupo.monedaPorDefecto pago.moneda ++ " " ++ Monto.toString pago.monto)
                , if not pago.isValid then
                    Bs.alert Bs.AlertWarning
                        [ class "py-2 mt-2" ]
                        [ i [ class "bi bi-exclamation-triangle-fill me-1" ] []
                        , text "Este pago tiene datos inválidos."
                        ]

                  else
                    text ""
                , Html.h6 [ class "mt-3 mb-1" ] [ text "Pagaron" ]
                , viewDistribucion grupo pago.moneda pago.pagadores
                , Html.h6 [ class "mt-3 mb-1" ] [ text "Participan" ]
                , viewDistribucion grupo pago.moneda pago.deudores
                , viewBalance grupo state.resumen
                ]

        Loading ->
            div [ class "text-center py-3" ] [ Bs.spinner [] ]

        NotAsked ->
            div [ class "text-center py-3" ] [ Bs.spinner [] ]

        Failure _ ->
            Bs.alert Bs.AlertDanger [] [ text "No se pudieron cargar los detalles." ]


viewBalance : ShallowGrupo -> WebData ResumenPago -> Html Msg
viewBalance grupo resumen =
    case resumen of
        Success resumenPago ->
            case getDeudasFromResumen resumenPago.resumen of
                Just netos ->
                    div []
                        [ Html.h6 [ class "mt-3 mb-1" ] [ text "Balance" ]
                        , viewNetosBarras grupo netos
                        ]

                Nothing ->
                    text ""

        Loading ->
            div [ class "text-center py-2" ] [ Bs.spinner [] ]

        _ ->
            text ""


viewDetalleFila : String -> String -> Html Msg
viewDetalleFila label valor =
    div [ class "d-flex justify-content-between" ]
        [ span [ class "text-muted" ] [ text label ]
        , strong [] [ text valor ]
        ]


viewDistribucion : ShallowGrupo -> Moneda -> Distribucion -> Html Msg
viewDistribucion grupo moneda distribucion =
    case distribucion.tipo of
        TipoDistribucionPartes dp ->
            if List.isEmpty dp.partes then
                div [ class "text-muted small" ] [ text "Nadie" ]

            else
                ul [ class "list-group list-group-flush mb-0" ]
                    (dp.partes |> List.map (viewParteLinea grupo moneda))

        TipoDistribucionRepartija _ ->
            div [ class "text-muted small" ] [ text "Repartija colaborativa" ]


viewParteLinea : ShallowGrupo -> Moneda -> Parte -> Html Msg
viewParteLinea grupo moneda parte =
    let
        montoStr m =
            Moneda.simbolo grupo.monedaPorDefecto moneda ++ " " ++ Monto.toString m

        partesStr n =
            String.fromInt n
                ++ (if n == 1 then
                        " parte"

                    else
                        " partes"
                   )

        ( participanteId, descripcion ) =
            case parte of
                MontoFijo m p ->
                    ( p, montoStr m )

                Ponderado n p ->
                    ( p, partesStr n )

                PonderadoYMontoFijo m n p ->
                    ( p, montoStr m ++ " + " ++ partesStr n )
    in
    li [ class "list-group-item d-flex justify-content-between px-0 py-1 border-0" ]
        [ span [] [ text (lookupNombreParticipante grupo participanteId) ]
        , span [ class "text-muted" ] [ text descripcion ]
        ]
