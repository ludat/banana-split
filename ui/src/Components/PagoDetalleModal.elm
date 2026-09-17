module Components.PagoDetalleModal exposing (Context, Model, Modo, Msg, Overlay, context, hrefNuevoGasto, hrefPago, init, onUrlChanged, update, view)

import Components.BarrasDeNetos exposing (viewNetosBarras, viewNetosBarrasMini)
import Components.Bootstrap as Bs
import Components.GraficoTorta as GraficoTorta
import Components.PagoEditForm as PagoEditForm
import Date exposing (Date)
import Dict exposing (Dict)
import Effect exposing (Effect)
import Generated.Api as Api exposing (ErrorResumen, Grupo, Moneda, Monto, Pago, Parte(..), Repartija, ResumenPago, TipoDistribucion(..), ULID)
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
import Shared
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

    -- Mientras se edita el gasto, el popup muestra el formulario completo en
    -- vez del detalle.
    , edicion : Maybe PagoEditForm.Model
    , confirmingDiscard : Bool
    , modo : Modo
    }


{-| El popup se abre para ver un gasto que ya existe o para crear uno nuevo. En
`Nuevo` no hay detalle atrás del formulario: cancelar cierra todo, y al crearlo
pasa a `VerDetalle` mostrando el gasto recién hecho.
-}
type Modo
    = VerDetalle
    | Nuevo


type Overlay
    = BalanceGraphOverlay
    | PagadoresGraphOverlay
    | DeudoresGraphOverlay


type alias Context =
    { grupoId : ULID
    , participanteId : Maybe ULID
    , path : Path.Path
    , origin : String
    , today : Date
    }


context : Shared.Model.Model -> Route routeParams -> Context
context shared route =
    let
        grupoId =
            grupoIdFromPath route.path |> Maybe.withDefault ""
    in
    { grupoId = grupoId
    , participanteId = Shared.currentParticipante shared grupoId
    , path = route.path
    , origin = shared.origin
    , today = shared.today
    }


init : Route routeParams -> ( Model, Effect Msg )
init route =
    case Dict.get queryGasto route.query of
        Just param ->
            if param == queryGastoNuevo then
                ( forNuevo, esperarGrupo )

            else
                ( forPago True param
                , loadPago (grupoIdFromPath route.path |> Maybe.withDefault "") param
                )

        Nothing ->
            ( forPago False ""
            , Effect.none
            )


{-| El parámetro de la query que dice qué muestra el popup.
-}
queryGasto : String
queryGasto =
    "gasto"


{-| El valor de `?gasto=` que abre el popup en modo creación. No puede chocar
con un id porque los ULID son mayúsculas y dígitos.
-}
queryGastoNuevo : String
queryGastoNuevo =
    "nuevo"


{-| Link para crear un gasto: abre el popup vacío sobre la página que se le
pase. Al ser un link de verdad, ctrl+click y "abrir en otra pestaña" funcionan.
-}
hrefNuevoGasto : Path.Path -> Html.Attribute msg
hrefNuevoGasto path =
    Route.href (rutaDelPopup path (Just queryGastoNuevo))


onUrlChanged : { from : Route (), to : Route () } -> Msg
onUrlChanged { from, to } =
    let
        gastoParam route =
            Dict.get queryGasto route.query
    in
    if gastoParam from == gastoParam to then
        NoOp

    else
        QueryChanged (gastoParam to)


forPago : Bool -> ULID -> Model
forPago isOpen pagoId =
    { isOpen = isOpen
    , pagoId = pagoId
    , resumen = Loading
    , confirmingDelete = False
    , deleting = False
    , activeOverlay = Nothing
    , expandedErrors = Set.empty
    , edicion = Nothing
    , confirmingDiscard = False
    , modo = VerDetalle
    }


{-| El popup abierto para crear: el formulario se arma recién cuando el grupo
está en el store (hacen falta los participantes), así que hasta entonces
muestra el spinner.
-}
forNuevo : Model
forNuevo =
    let
        modelo =
            forPago True ""
    in
    { modelo | modo = Nuevo }


esperarGrupo : Effect Msg
esperarGrupo =
    Effect.sendCmd <| Task.perform (\_ -> CheckGrupoPresent) (Process.sleep 100)


{-| Salir del formulario sin guardar. Editando un gasto que existe se vuelve a
su detalle; creando uno nuevo no hay nada atrás, así que se cierra el popup.
-}
salirDeLaEdicion : Context -> Model -> Effect Msg -> ( Model, Effect Msg )
salirDeLaEdicion ctx model effect =
    case model.modo of
        Nuevo ->
            ( cerrado model, Effect.batch [ effect, syncUrl ctx.path Nothing ] )

        VerDetalle ->
            ( { model | edicion = Nothing }, effect )


{-| Arma el formulario sobre el gasto dado, o vacío si es uno nuevo.
-}
abrirFormulario : Context -> Grupo -> Maybe Pago -> ( Model, Effect Msg ) -> ( Model, Effect Msg )
abrirFormulario ctx grupo pago ( model, effect ) =
    let
        ( edicion, eff ) =
            PagoEditForm.init
                { grupoId = ctx.grupoId
                , participantes = grupo.participantes
                , participanteId = ctx.participanteId
                , monedaPorDefecto = grupo.monedaPorDefecto
                , today = ctx.today
                , pago = pago
                }
    in
    ( { model | edicion = Just edicion }
    , Effect.batch [ effect, Effect.map EditFormMsg eff ]
    )


hayCambiosSinGuardar : Model -> Bool
hayCambiosSinGuardar model =
    case model.edicion of
        Just edicion ->
            edicion.hasUnsavedChanges

        Nothing ->
            False


{-| El botón atrás del navegador saca el `?gasto=` de la URL y con eso cerraría
la edición sin preguntar. Como la navegación ya pasó, la única forma de
frenarla es volver a poner la URL del gasto y mostrar la misma confirmación que
la X. Cada "atrás" agrega una entrada al historial, así que después de
descartar puede hacer falta más de un "atrás" para salir de la lista.
-}
volverAlGastoEnEdicion : Context -> Model -> ( Model, Effect Msg )
volverAlGastoEnEdicion ctx model =
    ( { model | confirmingDiscard = True }
    , syncUrl ctx.path (Just (paramDeLaUrl model))
    )


{-| El valor de `?gasto=` que corresponde a lo que el popup está mostrando.
-}
paramDeLaUrl : Model -> String
paramDeLaUrl model =
    case model.modo of
        Nuevo ->
            queryGastoNuevo

        VerDetalle ->
            model.pagoId


{-| El popup cerrado no conserva la edición: si se vuelve a abrir, arranca
mostrando el detalle.
-}
cerrado : Model -> Model
cerrado model =
    { model | isOpen = False, edicion = Nothing, confirmingDiscard = False }


loadPago : ULID -> ULID -> Effect Msg
loadPago grupoId pagoId =
    Effect.batch
        [ Store.refreshPago grupoId pagoId
        , waitForPago
        ]


waitForPago : Effect Msg
waitForPago =
    Effect.sendCmd <| Task.perform (\_ -> CheckPagoPresent) (Process.sleep 100)


{-| Link a un gasto: la misma URL que el popup deja en la barra (`?gasto=`), así
ctrl+click, el botón del medio y "abrir en otra pestaña" funcionan solos.

No hace falta manejar el click: el link navega (sin recargar, como cualquier
link interno) y el popup se abre desde `onUrlChanged`. Manejarlo además por
nuestra cuenta dejaba dos entradas en el historial por cada gasto abierto,
porque Elm intercepta el click igual aunque le hagamos `preventDefault`.

-}
hrefPago : Path.Path -> ULID -> Html.Attribute msg
hrefPago path pagoId =
    Route.href (rutaDelPopup path (Just pagoId))


{-| La ruta de una página con el popup abierto en lo que diga el parámetro (un
id de gasto o `nuevo`), o cerrado si no hay ninguno. Es el único lugar donde se
arma el `?gasto=`, así los links y los `pushRoute` no se pueden desincronizar.
-}
rutaDelPopup : Path.Path -> Maybe String -> { path : Path.Path, query : Dict String String, hash : Maybe String }
rutaDelPopup path param =
    { path = path
    , query =
        param
            |> Maybe.map (Dict.singleton queryGasto)
            |> Maybe.withDefault Dict.empty
    , hash = Nothing
    }


syncUrl : Path.Path -> Maybe String -> Effect Msg
syncUrl path param =
    Effect.pushRoute (rutaDelPopup path param)



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
    | StartEdit
    | CheckGrupoPresent
    | EditFormMsg PagoEditForm.Msg
    | AskDiscardEdit
    | CancelDiscardEdit
    | ConfirmDiscardEdit


update : Context -> Store -> Msg -> Model -> ( Model, Effect Msg )
update ctx store msg model =
    updateInterno ctx store msg model
        |> apagarAvisoSiSeCerroLaEdicion model


{-| El aviso del browser ("tenés cambios sin guardar") lo prende el formulario,
así que cuando el popup lo desmonta por su cuenta —el botón atrás, un borrado—
hay que apagarlo acá o queda pegado en el resto de la navegación.
-}
apagarAvisoSiSeCerroLaEdicion : Model -> ( Model, Effect Msg ) -> ( Model, Effect Msg )
apagarAvisoSiSeCerroLaEdicion modelAnterior ( model, effect ) =
    if estaEditando modelAnterior && not (estaEditando model) then
        ( model, Effect.batch [ effect, Effect.setUnsavedChangesWarning False ] )

    else
        ( model, effect )


updateInterno : Context -> Store -> Msg -> Model -> ( Model, Effect Msg )
updateInterno ctx store msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        Close ->
            ( cerrado model, syncUrl ctx.path Nothing )

        QueryChanged maybePagoId ->
            case maybePagoId of
                Just param ->
                    if param == queryGastoNuevo then
                        if model.isOpen && model.modo == Nuevo then
                            ( model, Effect.none )

                        else
                            ( forNuevo, esperarGrupo )

                    else if model.isOpen && param == model.pagoId && model.modo == VerDetalle then
                        -- Already showing this pago (e.g. we just pushed the URL ourselves)
                        ( model, Effect.none )

                    else if hayCambiosSinGuardar model then
                        volverAlGastoEnEdicion ctx model

                    else
                        ( forPago True param, loadPago ctx.grupoId param )

                Nothing ->
                    if hayCambiosSinGuardar model then
                        volverAlGastoEnEdicion ctx model

                    else
                        ( cerrado model, Effect.none )

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
            ( cerrado model
            , Effect.batch
                [ Store.refreshGrupo ctx.grupoId
                , Store.refreshResumen ctx.grupoId
                , Store.refreshPagos ctx.grupoId ctx.participanteId
                , Toasts.pushToast Toasts.ToastSuccess "Gasto borrado"
                , syncUrl ctx.path Nothing
                ]
            )

        DeleteResponse (Err _) ->
            ( { model | deleting = False, confirmingDelete = False }
            , Toasts.pushToast Toasts.ToastDanger "Falló al borrar el gasto"
            )

        StartEdit ->
            case ( Store.getPago model.pagoId store, Store.getGrupo ctx.grupoId store ) of
                ( Success pago, Success grupo ) ->
                    ( { model | confirmingDelete = False }
                    , Effect.none
                    )
                        |> abrirFormulario ctx grupo (Just pago)

                _ ->
                    ( model, Effect.none )

        CheckGrupoPresent ->
            case ( model.modo, model.edicion, Store.getGrupo ctx.grupoId store ) of
                ( Nuevo, Nothing, Success grupo ) ->
                    ( model, Effect.none ) |> abrirFormulario ctx grupo Nothing

                ( Nuevo, Nothing, Failure _ ) ->
                    ( cerrado model, Effect.none )

                ( Nuevo, Nothing, _ ) ->
                    ( model, esperarGrupo )

                _ ->
                    ( model, Effect.none )

        EditFormMsg subMsg ->
            case ( model.edicion, Store.getGrupo ctx.grupoId store ) of
                ( Just edicion, Success grupo ) ->
                    let
                        ( nuevaEdicion, eff, outcome ) =
                            PagoEditForm.update grupo.participantes subMsg edicion
                    in
                    case outcome of
                        PagoEditForm.SigueEditando ->
                            ( { model | edicion = Just nuevaEdicion }, Effect.map EditFormMsg eff )

                        PagoEditForm.Cancelado ->
                            -- Cancelar con cambios sin guardar pregunta igual
                            -- que cerrar el popup.
                            if nuevaEdicion.hasUnsavedChanges then
                                ( { model | edicion = Just nuevaEdicion, confirmingDiscard = True }
                                , Effect.map EditFormMsg eff
                                )

                            else
                                salirDeLaEdicion ctx model (Effect.map EditFormMsg eff)

                        PagoEditForm.Guardado pago ->
                            -- Al gasto recién creado lo empezamos a mostrar como
                            -- cualquier otro: su id pasa a la URL y el detalle
                            -- queda con el resumen recalculado.
                            ( { model
                                | edicion = Nothing
                                , confirmingDiscard = False
                                , resumen = Loading
                                , modo = VerDetalle
                                , pagoId = pago.pagoId
                              }
                            , Effect.batch
                                [ Effect.map EditFormMsg eff
                                , Effect.sendCmd <| Api.postPagosResumen pago ResumenFetched
                                , if model.modo == Nuevo then
                                    syncUrl ctx.path (Just pago.pagoId)

                                  else
                                    Effect.none
                                ]
                            )

                _ ->
                    ( model, Effect.none )

        AskDiscardEdit ->
            case model.edicion of
                Just edicion ->
                    if edicion.hasUnsavedChanges then
                        ( { model | confirmingDiscard = True }, Effect.none )

                    else
                        salirDeLaEdicion ctx model Effect.none

                Nothing ->
                    -- Todavía no se armó el formulario del gasto nuevo: no hay
                    -- nada que descartar, se cierra y listo.
                    ( cerrado model, syncUrl ctx.path Nothing )

        CancelDiscardEdit ->
            ( { model | confirmingDiscard = False }, Effect.none )

        ConfirmDiscardEdit ->
            salirDeLaEdicion ctx { model | confirmingDiscard = False } Effect.none



-- VIEW


view : Store -> Grupo -> Model -> Html Msg
view store grupo model =
    if not model.isOpen then
        text ""

    else
        let
            cargando =
                ( text ""
                , div [ class "text-center py-3" ] [ Bs.spinner [] ]
                , text ""
                )

            ( header, content, overlays ) =
                case ( model.edicion, model.modo ) of
                    ( Just edicion, _ ) ->
                        ( viewHeaderEdicion model
                        , Html.map EditFormMsg (PagoEditForm.view grupo edicion)
                        , text ""
                        )

                    ( Nothing, Nuevo ) ->
                        -- Todavía armando el formulario (falta el grupo).
                        cargando

                    ( Nothing, VerDetalle ) ->
                        case ( Store.getPago model.pagoId store, model.resumen ) of
                            ( Success pago, Success resumen ) ->
                                ( viewHeader grupo pago resumen model
                                , viewContent grupo pago resumen model
                                , viewOverlay grupo pago resumen model
                                )

                            ( Failure _, _ ) ->
                                viewEstado
                                    "bi bi-receipt-cutoff"
                                    "No encontramos este gasto"
                                    "Puede que lo hayan eliminado o que el enlace ya no sea válido."

                            ( _, Failure _ ) ->
                                viewEstado
                                    "bi bi-exclamation-triangle"
                                    "No pudimos cargar los detalles"
                                    "Hubo un problema al calcular el reparto. Probá de nuevo en un momento."

                            _ ->
                                cargando
        in
        div []
            [ div
                [ class "modal d-block fade show"
                , id modalOverlayId
                , tabindex -1
                , attribute "aria-modal" "true"
                , attribute "role" "dialog"
                , on "click" (closeOnOverlayClick (elBackdropCierra model))
                ]
                [ -- `modal-lg` en los dos modos: el formulario necesita el ancho
                  -- para sus tablas y el popup no cambia de tamaño al pasar de
                  -- ver a editar.
                  div [ class "modal-dialog modal-dialog-scrollable modal-lg" ]
                    [ div [ class "modal-content" ]
                        [ header
                        , div [ class "modal-body" ] [ content ]
                        ]
                    ]
                ]
            , div [ class "modal-backdrop show" ] []
            , overlays
            ]


{-| Los `Form` guardan funciones adentro, así que el modelo de la edición no se
puede comparar con `==`: hay que mirar el constructor.
-}
estaEditando : Model -> Bool
estaEditando model =
    case model.edicion of
        Just _ ->
            True

        Nothing ->
            False


{-| Mientras se edita, el header es sólo el título: los botones (cancelar,
siguiente, actualizar) los pone el propio formulario al final. La X no cierra el
popup de una: si hay cambios sin guardar primero pregunta.
-}
viewHeaderEdicion : Model -> Html Msg
viewHeaderEdicion model =
    div [ class "modal-header flex-column align-items-stretch border-bottom-0" ]
        [ div [ class "d-flex justify-content-between align-items-start" ]
            [ h4 [ class "modal-title fw-bold" ]
                [ text <|
                    case model.modo of
                        Nuevo ->
                            "Nuevo gasto"

                        VerDetalle ->
                            "Editar gasto"
                ]
            , button
                [ type_ "button"
                , class "btn-close"
                , attribute "aria-label" "Cerrar"
                , onClick AskDiscardEdit
                ]
                []
            ]
        , if model.confirmingDiscard then
            Bs.alert Bs.AlertWarning
                [ class "d-flex flex-wrap align-items-center gap-2 mt-3 mb-0 py-2" ]
                [ span [ class "flex-grow-1 small" ] [ text "Hay cambios sin guardar. ¿Descartarlos?" ]
                , Bs.btn Bs.Transparent
                    [ class "btn-sm", onClick CancelDiscardEdit ]
                    [ text "Seguir editando" ]
                , Bs.btn Bs.Danger
                    [ class "btn-sm", onClick ConfirmDiscardEdit ]
                    [ text "Descartar" ]
                ]

          else
            text ""
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


{-| Si el click afuera del diálogo cierra el popup: mientras no haya nada
tipeado sin guardar, sí, como cualquier modal. En cuanto el formulario tiene
cambios el backdrop queda inerte y hay que pasar por la X, que pregunta antes
de descartarlos.
-}
elBackdropCierra : Model -> Bool
elBackdropCierra model =
    not (hayCambiosSinGuardar model)


closeOnOverlayClick : Bool -> Decode.Decoder Msg
closeOnOverlayClick cierra =
    Decode.map2
        (\targetId currentId ->
            if targetId == currentId && cierra then
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


viewOverlay : Grupo -> Pago -> ResumenPago -> Model -> Html Msg
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


viewHeader : Grupo -> Pago -> ResumenPago -> Model -> Html Msg
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


viewActions : Grupo -> Pago -> Model -> Html Msg
viewActions grupo pago model =
    div [ class "d-flex align-items-center gap-2 mt-3" ]
        [ Bs.btn Bs.SecondarySolid
            [ class "rounded-pill px-4", onClick StartEdit ]
            [ text "Editar" ]
        , viewActionsMenu grupo pago model
        ]


viewActionsMenu : Grupo -> Pago -> Model -> Html Msg
viewActionsMenu grupo pago model =
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
                    [ div [ class "small text-danger mb-2" ] [ text "¿Eliminar este gasto? No se puede deshacer." ]
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
                [ a
                    [ Path.href <| Path.Grupos_GrupoId__Gastos_GastoId_ { grupoId = grupo.id, gastoId = pago.pagoId }
                    , class "dropdown-item"
                    ]
                    [ i [ class "bi bi-arrows-fullscreen me-2" ] [], text "Editar en pantalla completa" ]
                , button
                    [ type_ "button"
                    , class "dropdown-item text-danger"
                    , onClick AskDelete
                    ]
                    [ i [ class "bi bi-trash me-2" ] [], text "Eliminar" ]
                ]
            )
        ]


viewContent : Grupo -> Pago -> ResumenPago -> Model -> Html Msg
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


viewInfo : Grupo -> Pago -> ResumenPago -> Html Msg
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


viewBalance : Grupo -> ResumenPago -> Html Msg
viewBalance grupo resumen =
    case getDeudasFromResumen resumen.resumen of
        Just netos ->
            viewNetosBarras grupo netos

        Nothing ->
            div [ class "text-muted small" ] [ text "Sin balance para mostrar." ]


viewReparto : Grupo -> Pago -> ResumenPago -> Model -> Html Msg
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


viewRepartoClasico : Grupo -> Moneda -> List Parte -> Html Msg
viewRepartoClasico grupo moneda partes =
    div []
        [ div [ class "d-flex align-items-center gap-2 mb-3" ]
            [ i [ class "bi bi-pie-chart-fill fs-5" ] []
            , span [ class "fs-5 fw-semibold" ] [ text "Clásico" ]
            ]
        , viewPartesTabla grupo moneda "Nadie participa de este reparto." partes
        ]


viewPartesTabla : Grupo -> Moneda -> String -> List Parte -> Html Msg
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


viewPartesIguales : Grupo -> List ParteData -> Html Msg
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


viewClasicoTabla : Grupo -> Moneda -> Bool -> Bool -> List ParteData -> Html Msg
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


viewRepartoRepartija : Grupo -> Repartija -> Html Msg
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


viewPago : Grupo -> Pago -> ResumenPago -> Model -> Html Msg
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
