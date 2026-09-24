module Components.PagoDetalleModal exposing (Context, Edicion, Model, Msg, Overlay, ReceiptReadingState, Ruta, context, hrefNuevoGasto, hrefPago, init, onUrlChanged, rutaGasto, rutaNuevoGasto, update, view)

{-| El popup de un gasto: muestra su detalle y, cuando se edita o se crea uno,
el formulario completo en su lugar.

El formulario vivía en su propio módulo, pero el popup era su único usuario y la
separación costaba más de lo que daba: obligaba a exponer el `Model` del
formulario (y con él sus tipos internos), a un protocolo de `Outcome` para
avisar hacia arriba, y dejaba dos implementaciones distintas del mismo overlay
de gráficos. Acá es todo una sola máquina de estados.

Como el popup ya es un modal de Bootstrap, nada de lo que abre por encima puede
ser otro (no se anidan): los gráficos en grande y el selector de participantes
de mobile son overlays propios.

-}

import Base64.Encode
import Browser.Dom
import Bytes exposing (Bytes)
import Components.BarrasDeNetos exposing (viewNetosBarras, viewNetosBarrasMini)
import Components.Bootstrap as Bs
import Components.GraficoTorta as GraficoTorta
import Date exposing (Date)
import Dict exposing (Dict)
import Effect exposing (Effect)
import File exposing (File)
import Form exposing (Form)
import Form.Field as FormField
import Form.Init as Form
import Generated.Api as Api exposing (Distribucion, DistribucionDeSobras(..), ErrorResumen, Grupo, Moneda, Monto, Pago, Parte(..), Participante, ParticipanteId, Repartija, ResumenNetos, ResumenPago, TipoDistribucion(..), ULID)
import Generated.Moneda exposing (escalaDe)
import Html exposing (Html, a, button, div, h4, i, li, p, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes as Attr exposing (accept, attribute, class, classList, disabled, id, placeholder, style, tabindex, target, type_)
import Html.Events exposing (on, onClick, onSubmit)
import Http
import Json.Decode as Decode
import Models.Grupo exposing (grupoIdFromPath, lookupNombreParticipante)
import Models.LugarAccionable exposing (LugarParaAccionar(..))
import Models.Moneda as Moneda
import Models.Monto as Monto
import Models.PagoForm exposing (ModoPartes, Section(..), distribucionDeSobrasToString, mostrarMontoFijoField, mostrarPartesField, validatePago, validatePagoInSection)
import Models.Parte as Parte
import Models.ResumenNetos exposing (errorAccionableEn, errorMensaje, getDeudasFromResumen)
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
import Utils.Form exposing (CustomFormError, isDataModifyingEvent)
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts
import Utils.Ulid exposing (emptyUlid)



-- MODEL


type alias Model =
    { isOpen : Bool
    , pagoId : ULID
    , resumen : WebData ResumenPago
    , confirmingDelete : Bool
    , deleting : Bool
    , activeOverlay : Maybe Overlay
    , expandedErrors : Set.Set String
    , edicion : Maybe Edicion
    , confirmingDiscard : Bool
    }


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


{-| Una ruta con su query, como la quieren `Route.href` y `Effect.pushRoute`.
-}
type alias Ruta =
    { path : Path.Path, query : Dict String String, hash : Maybe String }


{-| La página dada con el popup abierto en un gasto, o creando uno nuevo.
-}
rutaGasto : Path.Path -> ULID -> Ruta
rutaGasto path pagoId =
    rutaDelPopup path (Just pagoId)


rutaNuevoGasto : Path.Path -> Ruta
rutaNuevoGasto path =
    rutaDelPopup path (Just queryGastoNuevo)


hrefNuevoGasto : Path.Path -> Html.Attribute msg
hrefNuevoGasto desde =
    Route.href (rutaNuevoGasto desde)


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
    }


{-| El popup abierto para crear: el id vacío es lo que lo marca como gasto
nuevo (ver `esGastoNuevo`). El formulario se arma recién cuando el grupo está en
el store —hacen falta los participantes—, así que hasta entonces muestra el
spinner.
-}
forNuevo : Model
forNuevo =
    forPago True ""


esperarGrupo : Effect Msg
esperarGrupo =
    Effect.sendCmd <| Task.perform (\_ -> CheckGrupoPresent) (Process.sleep 100)


{-| Salir del formulario sin guardar cierra el popup entero, lo mismo creando
que editando: cancelar devuelve la pantalla a como estaba antes de abrirlo, no
a un detalle intermedio que nadie pidió ver.
-}
salirDeLaEdicion : Context -> Model -> ( Model, Effect Msg )
salirDeLaEdicion ctx model =
    ( cerrado model, syncUrl ctx.path Nothing )


{-| Arma el formulario sobre el gasto dado, o vacío si es uno nuevo.
-}
abrirFormulario : Context -> Grupo -> Maybe Pago -> Model -> ( Model, Effect Msg )
abrirFormulario ctx grupo pago model =
    let
        ( edicion, eff ) =
            initEdicion ctx grupo pago
    in
    ( { model | edicion = Just edicion }, eff )


{-| El popup abierto para crear todavía no tiene gasto atrás: es lo único que
distingue crear de editar, y por eso el id vacío alcanza como marca.
-}
esGastoNuevo : Model -> Bool
esGastoNuevo model =
    String.isEmpty model.pagoId


{-| Un mensaje que solo toca el formulario. Necesita los participantes para
revalidar, así que sin el grupo en el store no hay nada que hacer.
-}
enElFormulario : Context -> Store -> Msg -> Model -> ( Model, Effect Msg )
enElFormulario ctx store msg model =
    case ( model.edicion, Store.getGrupo ctx.grupoId store ) of
        ( Just edicion, Success grupo ) ->
            let
                ( nuevaEdicion, eff ) =
                    updateEdicion grupo.participantes msg edicion
            in
            ( { model | edicion = Just nuevaEdicion }
            , Effect.batch
                [ eff
                , -- Mientras haya cambios sin guardar, el browser pregunta
                  -- antes de recargar o cerrar la pestaña.
                  Effect.setUnsavedChangesWarning nuevaEdicion.hasUnsavedChanges
                ]
            )

        _ ->
            ( model, Effect.none )


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
    if esGastoNuevo model then
        queryGastoNuevo

    else
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
    Route.href (rutaGasto path pagoId)


{-| La ruta de una página con el popup abierto en lo que diga el parámetro (un
id de gasto o `nuevo`), o cerrado si no hay ninguno. Es el único lugar donde se
arma el `?gasto=`, así los links y los `pushRoute` no se pueden desincronizar.
-}
rutaDelPopup : Path.Path -> Maybe String -> Ruta
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
    | AskDiscardEdit
    | CancelDiscardEdit
    | ConfirmDiscardEdit
      -- De acá para abajo, mensajes del formulario. Van en el mismo `Msg` que
      -- el resto: el formulario ya no es un componente aparte al que haya que
      -- traducirle los mensajes.
    | PagoForm Form.Msg
    | GuardadoPagoResponse (Result Http.Error Pago)
    | SelectSection Section
    | SubmitCurrentSection
    | ResumenDeudoresUpdated (WebData ResumenPago)
    | ResumenPagadoresUpdated (WebData ResumenPago)
    | ReceiptImageSelected File
    | ReceiptImageBytes File Bytes
    | ReceiptParseResponse (Result Http.Error Api.ReceiptImageResponse)
    | ClearReceiptError
    | AbrirSelector { titulo : String, prefix : String }
    | CerrarSelector
    | Cancel


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
                        if model.isOpen && esGastoNuevo model then
                            ( model, Effect.none )

                        else
                            ( forNuevo, esperarGrupo )

                    else if model.isOpen && param == model.pagoId && not (esGastoNuevo model) then
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
                    { model | confirmingDelete = False }
                        |> abrirFormulario ctx grupo (Just pago)

                _ ->
                    ( model, Effect.none )

        CheckGrupoPresent ->
            if not (esGastoNuevo model) || model.edicion /= Nothing then
                ( model, Effect.none )

            else
                case Store.getGrupo ctx.grupoId store of
                    Success grupo ->
                        abrirFormulario ctx grupo Nothing model

                    Failure _ ->
                        ( cerrado model, Effect.none )

                    _ ->
                        ( model, esperarGrupo )

        Cancel ->
            -- Cancelar con cambios sin guardar pregunta igual que cerrar el
            -- popup.
            if hayCambiosSinGuardar model then
                ( { model | confirmingDiscard = True }, Effect.none )

            else
                salirDeLaEdicion ctx model

        GuardadoPagoResponse (Ok pago) ->
            -- Al gasto recién creado lo empezamos a mostrar como cualquier
            -- otro: su id pasa a la URL y el detalle queda con el resumen
            -- recalculado.
            ( { model
                | edicion = Nothing
                , confirmingDiscard = False
                , resumen = Loading
                , pagoId = pago.pagoId
              }
            , Effect.batch
                [ Store.refreshResumen ctx.grupoId
                , Store.refreshPagos ctx.grupoId ctx.participanteId
                , Store.setPago pago.pagoId pago
                , Toasts.pushToast Toasts.ToastSuccess <|
                    if esGastoNuevo model then
                        "Se creó el gasto"

                    else
                        "Se actualizó el gasto"
                , Effect.sendCmd <| Api.postPagosResumen pago ResumenFetched
                , if esGastoNuevo model then
                    syncUrl ctx.path (Just pago.pagoId)

                  else
                    Effect.none
                ]
            )

        GuardadoPagoResponse (Err _) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger <|
                if esGastoNuevo model then
                    "Falló la creación del gasto"

                else
                    "Falló la actualización del gasto"
            )

        PagoForm _ ->
            enElFormulario ctx store msg model

        SelectSection _ ->
            enElFormulario ctx store msg model

        SubmitCurrentSection ->
            enElFormulario ctx store msg model

        ResumenDeudoresUpdated _ ->
            enElFormulario ctx store msg model

        ResumenPagadoresUpdated _ ->
            enElFormulario ctx store msg model

        ReceiptImageSelected _ ->
            enElFormulario ctx store msg model

        ReceiptImageBytes _ _ ->
            enElFormulario ctx store msg model

        ReceiptParseResponse _ ->
            enElFormulario ctx store msg model

        ClearReceiptError ->
            enElFormulario ctx store msg model

        AbrirSelector _ ->
            enElFormulario ctx store msg model

        CerrarSelector ->
            enElFormulario ctx store msg model

        AskDiscardEdit ->
            case model.edicion of
                Just edicion ->
                    if edicion.hasUnsavedChanges then
                        ( { model | confirmingDiscard = True }, Effect.none )

                    else
                        salirDeLaEdicion ctx model

                Nothing ->
                    -- Todavía no se armó el formulario del gasto nuevo: no hay
                    -- nada que descartar, se cierra y listo.
                    ( cerrado model, syncUrl ctx.path Nothing )

        CancelDiscardEdit ->
            ( { model | confirmingDiscard = False }, Effect.none )

        ConfirmDiscardEdit ->
            salirDeLaEdicion ctx { model | confirmingDiscard = False }



-- VIEW


view : Store -> Grupo -> Model -> Html Msg
view store grupo model =
    if not model.isOpen then
        text ""

    else
        let
            ( header, content, overlays ) =
                case model.edicion of
                    Just edicion ->
                        ( viewHeaderEdicion model
                        , viewEdicion grupo model.activeOverlay edicion
                        , text ""
                        )

                    Nothing ->
                        if esGastoNuevo model then
                            -- Todavía armando el formulario (falta el grupo).
                            ( text ""
                            , div [ class "text-center py-3" ] [ Bs.spinner [] ]
                            , text ""
                            )

                        else
                            case ( Store.getPago model.pagoId store, model.resumen ) of
                                ( Success pago, Success resumen ) ->
                                    ( viewHeader pago resumen model
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
                , on "click" (closeOnOverlayClick (elBackdropCierra model))
                ]
                [ -- `modal-lg` en los dos modos: el formulario necesita el ancho
                  -- para sus tablas y el popup no cambia de tamaño al pasar de
                  -- ver a editar. En mobile ocupa la pantalla entera, que es
                  -- donde el formulario más necesita el alto.
                  div [ class "modal-dialog modal-dialog-scrollable modal-lg modal-fullscreen-md-down" ]
                    [ div [ class "modal-content" ]
                        [ header

                        -- Columna flex para que el pie de acciones del
                        -- formulario pueda empujarse al fondo con `mt-auto`.
                        , div [ class "modal-body d-flex flex-column" ] [ content ]
                        ]
                    ]
                ]
            , div [ class "modal-backdrop show" ] []
            , overlays
            , viewDescartarOverlay model
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
    div [ class "modal-header border-bottom-0" ]
        [ h4 [ class "modal-title fw-bold" ]
            [ text <|
                if esGastoNuevo model then
                    "Nuevo gasto"

                else
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


{-| La confirmación de descartar, por encima del formulario. Va en su propio
diálogo y no adentro del header para que corte lo que estabas haciendo: es una
decisión que hay que tomar antes de seguir.

Como en los otros overlays del popup, no es un modal de Bootstrap; el click
afuera equivale a "seguir editando", que es lo que no rompe nada.

-}
viewDescartarOverlay : Model -> Html Msg
viewDescartarOverlay model =
    if not model.confirmingDiscard then
        text ""

    else
        div []
            [ div
                [ class "modal d-block"
                , id descartarOverlayId
                , style "z-index" "1070"
                , tabindex -1
                , attribute "aria-modal" "true"
                , attribute "role" "dialog"
                , on "click" (cerrarAlClickearAfuera descartarOverlayId CancelDiscardEdit)
                ]
                [ div [ class "modal-dialog modal-dialog-centered modal-sm" ]
                    [ div [ class "modal-content" ]
                        [ div [ class "modal-body" ]
                            [ h4 [ class "fs-5 fw-bold mb-2" ] [ text "¿Descartar los cambios?" ]
                            , p [ class "text-body-secondary mb-0" ]
                                [ text "Lo que cargaste en este gasto se va a perder." ]
                            ]
                        , div [ class "modal-footer border-top-0" ]
                            [ Bs.btn Bs.Transparent
                                [ onClick CancelDiscardEdit ]
                                [ text "Seguir editando" ]
                            , Bs.btn Bs.Danger
                                [ onClick ConfirmDiscardEdit ]
                                [ text "Descartar" ]
                            ]
                        ]
                    ]
                ]
            , div [ class "modal-backdrop show", style "z-index" "1065" ] []
            ]


descartarOverlayId : String
descartarOverlayId =
    "pago-descartar-modal"


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
    if cierra then
        cerrarAlClickearAfuera modalOverlayId Close

    else
        Decode.succeed NoOp


{-| El click en un diálogo sólo cuenta como "afuera" cuando pegó en el propio
contenedor del modal y no en algo de adentro.
-}
cerrarAlClickearAfuera : String -> Msg -> Decode.Decoder Msg
cerrarAlClickearAfuera unId alCerrar =
    Decode.map2
        (\targetId currentId ->
            if targetId == currentId && currentId == unId then
                alCerrar

            else
                NoOp
        )
        (Decode.at [ "target", "id" ] Decode.string)
        (Decode.at [ "currentTarget", "id" ] Decode.string)


overlayId : String
overlayId =
    "pago-detalle-overlay-modal"


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
            viewOverlayChrome titulo cuerpo


{-| El marco de lo que se abre por encima del popup. No puede ser un modal de
Bootstrap —no se anidan— así que es un `div` propio, con su backdrop y su
z-index por arriba del popup. Lo comparten el detalle y la edición, que solo se
diferencian en de dónde sacan los datos del gráfico.
-}
viewOverlayChrome : String -> Html Msg -> Html Msg
viewOverlayChrome titulo cuerpo =
    div []
        [ div
            [ class "modal d-block fade show"
            , id overlayId
            , style "z-index" "1070"
            , tabindex -1
            , attribute "aria-modal" "true"
            , attribute "role" "dialog"
            , on "click" (cerrarAlClickearAfuera overlayId CloseOverlay)
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


viewHeader : Pago -> ResumenPago -> Model -> Html Msg
viewHeader pago resumen model =
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
        , viewActions model
        ]


viewActions : Model -> Html Msg
viewActions model =
    div [ class "d-flex align-items-center gap-2 mt-3" ]
        [ Bs.btn Bs.SecondarySolid
            [ class "rounded-pill px-4", onClick StartEdit ]
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
                [ button
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



-- FORMULARIO


type alias Edicion =
    { grupoId : ULID

    -- `Nothing` es un gasto que todavía no existe: se crea con POST en vez de
    -- actualizarse con PUT.
    , pagoId : Maybe ULID
    , currentSection : Section
    , pagoBasicoForm : Form CustomFormError Pago
    , pagadoresForm : Form CustomFormError Pago
    , resumenPagadores : WebData ResumenPago
    , deudoresForm : Form CustomFormError Pago
    , resumenDeudores : WebData ResumenPago

    -- El form completo no tiene resumen propio: lo que se muestra son los dos
    -- de arriba, uno por paso.
    , pagoForm : Form CustomFormError Pago
    , receiptParseState : Maybe ReceiptReadingState
    , storedClaims : Maybe { pagadores : List Api.RepartijaClaim, deudores : List Api.RepartijaClaim }
    , hasUnsavedChanges : Bool

    -- El selector de participantes de mobile, con el prefijo del form que está
    -- eligiendo (pagadores o deudores).
    , selectorAbierto : Maybe { titulo : String, prefix : String }
    }


type ReceiptReadingState
    = ReadingFile
    | ProcessingWithAI
    | ErrorProcessing String


{-| Con `pago = Nothing` el formulario arranca vacío para crear un gasto nuevo:
la fecha de hoy y el creador como único pagador.
-}
initEdicion : Context -> Grupo -> Maybe Pago -> ( Edicion, Effect Msg )
initEdicion ctx grupo pago =
    let
        participantes =
            grupo.participantes

        vacio =
            { grupoId = ctx.grupoId
            , pagoId = pago |> Maybe.map .pagoId
            , currentSection = BasicPagoData
            , pagoBasicoForm = Form.initial [] (validatePagoInSection BasicPagoData participantes)
            , pagadoresForm = Form.initial [] (validatePagoInSection PagadoresSection participantes)
            , resumenPagadores = NotAsked
            , deudoresForm = Form.initial [] (validatePagoInSection DeudoresSection participantes)
            , resumenDeudores = NotAsked
            , pagoForm = Form.initial [] (validatePago participantes)
            , receiptParseState = Nothing
            , storedClaims = Nothing
            , hasUnsavedChanges = False
            , selectorAbierto = Nothing
            }
    in
    -- Los resúmenes se piden comparando contra el modelo con los forms todavía
    -- vacíos, así arrancan calculados sobre el gasto que se está editando.
    ( initializePagoForms participantes ctx.participanteId grupo.monedaPorDefecto ctx.today pago vacio
    , Effect.none
    )
        |> andThenUpdateResumenesFromForms vacio


initializePagoForms : List Participante -> Maybe ParticipanteId -> Moneda -> Date -> Maybe Pago -> Edicion -> Edicion
initializePagoForms participantes creadorId monedaPorDefecto today pago model =
    let
        initialFormValues =
            [ Form.setString "id" (pago |> Maybe.map .pagoId |> Maybe.withDefault "")
            , Form.setString "nombre" (pago |> Maybe.map .nombre |> Maybe.withDefault "")
            , Form.setString "monto" (pago |> Maybe.map (.monto >> Monto.toRawString) |> Maybe.withDefault "")
            , Form.setString "moneda"
                (Moneda.toString (pago |> Maybe.map .moneda |> Maybe.withDefault monedaPorDefecto))
            , Form.setString "fecha"
                (pago |> Maybe.map .fecha |> Maybe.withDefault today |> Date.toIsoString)
            , Form.setGroup "distribucion_pagadores" <|
                pagadoresToForm participantes creadorId (Maybe.map .pagadores pago)
            , Form.setGroup "distribucion_deudores" <|
                deudoresToForm participantes (Maybe.map .deudores pago)
            ]
    in
    { model
        | pagoForm = Form.initial initialFormValues (validatePago participantes)
        , pagadoresForm = Form.initial initialFormValues (validatePagoInSection PagadoresSection participantes)
        , deudoresForm = Form.initial initialFormValues (validatePagoInSection DeudoresSection participantes)
        , pagoBasicoForm = Form.initial initialFormValues (validatePagoInSection BasicPagoData participantes)
        , storedClaims =
            pago
                |> Maybe.map
                    (\p ->
                        { pagadores = extractClaimsFromDistribucion p.pagadores
                        , deudores = extractClaimsFromDistribucion p.deudores
                        }
                    )
        , hasUnsavedChanges = False
    }



-- FORM <-> PAGO


extractClaimsFromDistribucion : Distribucion -> List Api.RepartijaClaim
extractClaimsFromDistribucion distribucion =
    case distribucion.tipo of
        TipoDistribucionRepartija repartija ->
            repartija.claims

        _ ->
            []


mergeClaimsIntoPago : Maybe { pagadores : List Api.RepartijaClaim, deudores : List Api.RepartijaClaim } -> Pago -> Pago
mergeClaimsIntoPago maybeClaims pago =
    case maybeClaims of
        Nothing ->
            pago

        Just claims ->
            { pago
                | pagadores = mergeClaimsIntoDistribucion claims.pagadores pago.pagadores
                , deudores = mergeClaimsIntoDistribucion claims.deudores pago.deudores
            }


mergeClaimsIntoDistribucion : List Api.RepartijaClaim -> Distribucion -> Distribucion
mergeClaimsIntoDistribucion claims distribucion =
    case distribucion.tipo of
        TipoDistribucionRepartija repartija ->
            { distribucion
                | tipo = TipoDistribucionRepartija { repartija | claims = claims }
            }

        _ ->
            distribucion


defaultRepartija : Repartija
defaultRepartija =
    { id = emptyUlid
    , nombre = "GENERATED"
    , items = []
    , claims = []
    , extra = Monto.zero
    , distribucionDeSobras = SobrasNoDistribuir
    }


{-| Los pagadores se editan con la misma distribución por partes que los
deudores; las distribuciones existentes se convierten conservando sus partes.
-}
pagadoresToForm : List Participante -> Maybe ParticipanteId -> Maybe Distribucion -> List ( String, FormField.Field )
pagadoresToForm participantes creadorId distribucion =
    let
        modo =
            case distribucion |> Maybe.map .tipo of
                Just (TipoDistribucionPartes _) ->
                    deriveModoPartes distribucion

                _ ->
                    { mostrarPartes = False, mostrarMontoFijo = False }

        partes =
            case distribucion |> Maybe.map .tipo of
                Just (TipoDistribucionPartes p) ->
                    p

                Nothing ->
                    { id = emptyUlid
                    , partes =
                        creadorId
                            |> Maybe.map (\id -> [ Api.Ponderado 1 id ])
                            |> Maybe.withDefault []
                    }

                _ ->
                    { id = emptyUlid, partes = [] }
    in
    [ Form.setString "id" (distribucion |> Maybe.map .id |> Maybe.withDefault emptyUlid)
    , Form.setBool mostrarPartesField modo.mostrarPartes
    , Form.setBool mostrarMontoFijoField modo.mostrarMontoFijo
    ]
        ++ partesToForm participantes partes


{-| Deduce qué columnas mostrar (partes / monto fijo) a partir de las `Parte`
ya guardadas. Si todas son `Ponderado 1` se asume "en partes iguales" (ambos
flags en `False`).
-}
deriveModoPartes : Maybe Distribucion -> ModoPartes
deriveModoPartes distribucion =
    case distribucion |> Maybe.map .tipo of
        Just (TipoDistribucionPartes partes) ->
            { mostrarPartes =
                partes.partes
                    |> List.any
                        (\parte ->
                            case parte of
                                Api.Ponderado cuota _ ->
                                    cuota /= 1

                                Api.PonderadoYMontoFijo _ _ _ ->
                                    True

                                Api.MontoFijo _ _ ->
                                    False
                        )
            , mostrarMontoFijo =
                partes.partes
                    |> List.any
                        (\parte ->
                            case parte of
                                Api.MontoFijo _ _ ->
                                    True

                                Api.PonderadoYMontoFijo _ _ _ ->
                                    True

                                Api.Ponderado _ _ ->
                                    False
                        )
            }

        _ ->
            { mostrarPartes = False, mostrarMontoFijo = False }


deudoresToForm : List Participante -> Maybe Distribucion -> List ( String, FormField.Field )
deudoresToForm participantes distribucion =
    let
        modo =
            deriveModoPartes distribucion

        partesEquitativas incluidos =
            { id = emptyUlid
            , partes = incluidos |> List.map (Api.Ponderado 1)
            }

        tipoInicial =
            case distribucion |> Maybe.map .tipo of
                Just (TipoDistribucionRepartija _) ->
                    "repartija"

                Just (TipoDistribucionPartes _) ->
                    "partes"

                Nothing ->
                    ""
    in
    [ Form.setString "id" (distribucion |> Maybe.map .id |> Maybe.withDefault emptyUlid)
    , Form.setBool mostrarPartesField modo.mostrarPartes
    , Form.setBool mostrarMontoFijoField modo.mostrarMontoFijo
    ]
        ++ (case distribucion |> Maybe.map .tipo of
                Just (TipoDistribucionRepartija repartija) ->
                    partesToForm participantes (partesEquitativas (participantes |> List.map .id))
                        ++ repartijaToForm repartija

                Just (TipoDistribucionPartes partes) ->
                    repartijaToForm defaultRepartija
                        ++ partesToForm participantes partes

                Nothing ->
                    repartijaToForm defaultRepartija
                        ++ partesToForm participantes (partesEquitativas (participantes |> List.map .id))
           )
        -- `partesToForm`/`repartijaToForm` fijan su propio "tipo"; lo
        -- sobreescribimos al final para que mande `tipoInicial`.
        ++ [ Form.setString "tipo" tipoInicial ]


partesToForm : List Participante -> Api.DistribucionPartes -> List ( String, FormField.Field )
partesToForm participantes distribucion =
    [ Form.setString "partes_id" distribucion.id
    , Form.setString "tipo" "partes"
    , Form.setGroup "partes"
        (participantes
            |> List.map
                (\participante ->
                    let
                        campos =
                            case
                                distribucion.partes
                                    |> List.filter (\parte -> Parte.participanteId parte == participante.id)
                                    |> List.head
                            of
                                Nothing ->
                                    { participa = False, cuota = "1", monto = Monto.toRawString Monto.zero }

                                Just (Api.Ponderado cuota _) ->
                                    { participa = True, cuota = String.fromInt cuota, monto = Monto.toRawString Monto.zero }

                                Just (Api.MontoFijo monto _) ->
                                    { participa = True, cuota = "1", monto = Monto.toRawString monto }

                                Just (Api.PonderadoYMontoFijo monto cuota _) ->
                                    { participa = True, cuota = String.fromInt cuota, monto = Monto.toRawString monto }
                    in
                    Form.setGroup participante.id
                        [ Form.setBool "participa" campos.participa
                        , Form.setString "cuota" campos.cuota
                        , Form.setString "monto" campos.monto
                        ]
                )
        )
    ]


repartijaToForm : Repartija -> List ( String, FormField.Field )
repartijaToForm repartija =
    [ Form.setString "repartija_id" repartija.id
    , Form.setString "tipo" "repartija"
    , Form.setString "extra" (Monto.toRawString repartija.extra)
    , Form.setString "distribucionDeSobras"
        (distribucionDeSobrasToString repartija.distribucionDeSobras)
    , Form.setList "items"
        (repartija.items
            |> List.map
                (\item ->
                    FormField.group
                        [ Form.setString "id" <| item.id
                        , Form.setString "monto" <| Monto.toRawString item.monto
                        , Form.setString "cantidad" <| String.fromInt item.cantidad
                        , Form.setString "nombre" item.nombre
                        ]
                )
        )
    ]


receiptItemsFormMsgs : String -> List Api.RepartijaItem -> Form CustomFormError Pago -> List Form.Msg
receiptItemsFormMsgs prefix items form =
    let
        startingIndex =
            List.length (Form.getListIndexes (prefix ++ ".items") form)
    in
    items
        |> List.indexedMap
            (\idx item ->
                let
                    itemPrefix =
                        prefix ++ ".items." ++ String.fromInt (startingIndex + idx)
                in
                [ Form.Append (prefix ++ ".items")
                , Form.Input (itemPrefix ++ ".id") Form.Text (FormField.String emptyUlid)
                , Form.Input (itemPrefix ++ ".nombre") Form.Text (FormField.String item.nombre)
                , Form.Input (itemPrefix ++ ".monto") Form.Text (FormField.String (Monto.toRawString item.monto))
                , Form.Input (itemPrefix ++ ".cantidad") Form.Text (FormField.String (String.fromInt item.cantidad))
                ]
            )
        |> List.concat



-- UPDATE


viewEdicion : Grupo -> Maybe Overlay -> Edicion -> Html Msg
viewEdicion grupo overlayAbierto model =
    -- Crece hasta el alto del modal y reparte en columna, así el paso de
    -- adentro puede mandar su pie de acciones al fondo.
    div [ class "flex-grow-1 d-flex flex-column" ]
        [ if model.hasUnsavedChanges then
            Bs.alert Bs.AlertWarning [ style "margin-bottom" "1rem" ] [ text "Hay cambios sin guardar" ]

          else
            text ""
        , viewStepTabs model
        , case model.currentSection of
            BasicPagoData ->
                viewBasicSection model

            PagadoresSection ->
                viewPagadoresSection grupo model

            DeudoresSection ->
                viewDeudoresSection grupo model
        , viewSelectorOverlay grupo.participantes model
        , viewTortaOverlay grupo overlayAbierto model
        ]


{-| Los dos pasos del formulario que tienen netos propios —y por lo tanto
errores y gráfico—. El campo del que salen y el accessor que los extrae iban
siempre de a pares, así que el paso alcanza para los dos.

El detalle tiene además el balance, que acá no existe: por eso esto no es el
`Overlay` de tres constructores.

-}
type Paso
    = Pagadores
    | Deudores


netosDelPaso : Paso -> Edicion -> WebData ResumenNetos
netosDelPaso paso model =
    case paso of
        Pagadores ->
            RemoteData.map .resumenPagadores model.resumenPagadores

        Deudores ->
            RemoteData.map .resumenDeudores model.resumenDeudores


overlayDelPaso : Paso -> Overlay
overlayDelPaso paso =
    case paso of
        Pagadores ->
            PagadoresGraphOverlay

        Deudores ->
            DeudoresGraphOverlay


pasoDelOverlay : Overlay -> Maybe Paso
pasoDelOverlay overlay =
    case overlay of
        PagadoresGraphOverlay ->
            Just Pagadores

        DeudoresGraphOverlay ->
            Just Deudores

        BalanceGraphOverlay ->
            Nothing


hasActionableErrors : WebData ResumenNetos -> Bool
hasActionableErrors netos =
    case netos of
        Success resumen ->
            List.any (\e -> List.member Lugar_CreacionPago (errorAccionableEn e.tipo)) resumen.errores

        _ ->
            False


viewErrorFromResumenData : WebData ResumenNetos -> Html msg
viewErrorFromResumenData netos =
    case netos of
        Success resumen ->
            viewErrorFromResumen Lugar_CreacionPago resumen

        _ ->
            text ""


viewErrorFromResumen : LugarParaAccionar -> ResumenNetos -> Html msg
viewErrorFromResumen lugar resumen =
    case resumen.errores of
        [] ->
            text ""

        _ ->
            div []
                (resumen.errores
                    |> List.map
                        (\error ->
                            let
                                esAccionable =
                                    List.member lugar (errorAccionableEn error.tipo)

                                mensaje =
                                    errorMensaje error.tipo
                            in
                            Bs.alert
                                (if esAccionable then
                                    Bs.AlertDanger

                                 else
                                    Bs.AlertInfo
                                )
                                [ style "margin-bottom" "0.5rem" ]
                                [ case error.objeto of
                                    [] ->
                                        text mensaje

                                    _ ->
                                        div [ style "display" "flex", style "gap" "0.5rem", style "align-items" "baseline" ]
                                            [ Html.strong [] [ text (String.join " > " error.objeto ++ ":") ]
                                            , text mensaje
                                            ]
                                ]
                        )
                )


porcionesTorta : Grupo -> Paso -> Edicion -> List GraficoTorta.PorcionTorta
porcionesTorta grupo paso model =
    case netosDelPaso paso model of
        Success netos ->
            GraficoTorta.porciones grupo (totalDelForm model) netos

        _ ->
            []


{-| El monto que se está cargando, contra el que se calculan las porciones.
-}
totalDelForm : Edicion -> Maybe Monto
totalDelForm model =
    Form.getOutput model.pagoBasicoForm |> Maybe.map .monto


{-| La torta del paso, en chiquito y clickeable para verla grande. El grande no
es un modal de Bootstrap (no se pueden anidar) sino `viewTortaOverlay`.
-}
viewTortaFooter : Grupo -> Paso -> Edicion -> Html Msg
viewTortaFooter grupo paso model =
    button
        [ type_ "button"
        , class "btn p-0 border-0 flex-shrink-0 d-flex align-items-center"
        , Attr.attribute "aria-label" "Ver el gráfico en grande"
        , onClick (OpenOverlay (overlayDelPaso paso))
        ]
        [ GraficoTorta.viewTortaMini (porcionesTorta grupo paso model) ]


{-| El gráfico en grande mientras se edita. Las porciones se recalculan al
dibujarlo, así sigue los cambios del formulario mientras está abierto; el
detalle en cambio las saca del resumen ya guardado (ver `viewOverlay`).
-}
viewTortaOverlay : Grupo -> Maybe Overlay -> Edicion -> Html Msg
viewTortaOverlay grupo overlayAbierto model =
    -- El balance no es un paso del formulario, así que `pasoDelOverlay` lo
    -- descarta junto con el caso de que no haya nada abierto.
    case overlayAbierto |> Maybe.andThen pasoDelOverlay of
        Nothing ->
            text ""

        Just paso ->
            viewOverlayChrome (tituloDelPaso paso)
                (GraficoTorta.viewTortaGrande (porcionesTorta grupo paso model))


tituloDelPaso : Paso -> String
tituloDelPaso paso =
    case paso of
        Pagadores ->
            "Pago"

        Deudores ->
            "Reparto"


{-| Wizard de pasos como tabs por defecto de Bootstrap (`nav-tabs`). Marca el
paso actual y permite saltar a cualquier paso con `SelectSection`.
-}
viewStepTabs : Edicion -> Html Msg
viewStepTabs model =
    let
        tab section label =
            let
                active =
                    model.currentSection == section
            in
            li [ class "nav-item" ]
                [ button
                    [ type_ "button"
                    , classList
                        [ ( "nav-link", True )
                        , ( "active", active )

                        -- Paso incompleto (todavía sin completar): se ve grisado
                        -- en vez de como error, así no aparece "en rojo" al inicio.
                        , ( "text-body-tertiary", not active && sectionIncomplete model section )
                        ]
                    , onClick (SelectSection section)
                    ]
                    (text label
                        :: (if not (sectionIncomplete model section) && sectionHasError model section then
                                [ i [ class "bi bi-exclamation-circle-fill text-danger ms-1" ] [] ]

                            else
                                []
                           )
                    )
                ]
    in
    div [ class "d-flex align-items-end mb-4" ]
        [ Html.ul [ class "nav nav-tabs flex-grow-1" ]
            [ tab BasicPagoData "Gasto"
            , tab PagadoresSection "Pago"
            , tab DeudoresSection "Reparto"
            ]
        ]


{-| Un paso está incompleto cuando su form todavía no produce un valor válido.
Se muestra grisado (no como error) para no alarmar al inicio.
-}
sectionIncomplete : Edicion -> Section -> Bool
sectionIncomplete model section =
    let
        faltaTotal =
            Form.getOutput model.pagoBasicoForm == Nothing
    in
    case section of
        BasicPagoData ->
            faltaTotal

        PagadoresSection ->
            let
                sinPagadores =
                    case Form.getOutput model.pagadoresForm of
                        Just pago ->
                            distribucionSinPartes pago.pagadores

                        Nothing ->
                            True
            in
            faltaTotal || sinPagadores

        DeudoresSection ->
            faltaTotal || Form.getOutput model.deudoresForm == Nothing


distribucionSinPartes : Distribucion -> Bool
distribucionSinPartes distribucion =
    case distribucion.tipo of
        Api.TipoDistribucionPartes { partes } ->
            List.isEmpty partes

        Api.TipoDistribucionRepartija _ ->
            False


{-| Un paso tiene error sólo cuando ya tiene datos cargados pero su resumen
marca errores accionables (p. ej. los montos no cuadran). Un paso simplemente
incompleto no cuenta como error: se muestra grisado vía `sectionIncomplete`.
-}
sectionHasError : Edicion -> Section -> Bool
sectionHasError model section =
    case section of
        BasicPagoData ->
            False

        PagadoresSection ->
            hasActionableErrors (netosDelPaso Pagadores model)

        DeudoresSection ->
            hasActionableErrors (netosDelPaso Deudores model)


viewBasicSection : Edicion -> Html Msg
viewBasicSection model =
    let
        form =
            model.pagoBasicoForm

        nombreField =
            Form.getFieldAsString "nombre" form

        montoField =
            Form.getFieldAsString "monto" form

        monedaField =
            Form.getFieldAsString "moneda" form

        fechaField =
            Form.getFieldAsString "fecha" form
    in
    Html.form [ onSubmit SubmitCurrentSection, class "flex-grow-1 d-flex flex-column" ]
        [ Html.map PagoForm <|
            Bs.textFormItem nombreField
                { label = "Título"
                , placeholder = Just "Restaurant El Oso Pardo"
                , required = True
                }
        , div [ class "d-flex gap-3 flex-wrap align-items-start" ]
            [ div [ class "flex-grow-1 mb-3" ]
                [ Html.label [ class "form-label", Attr.for "monto" ]
                    [ text "Monto total", Bs.requiredMarker True ]
                , div [ class "d-flex gap-2" ]
                    [ Html.map PagoForm <|
                        Bs.selectInput
                            (Moneda.todas |> List.map (\m -> ( Moneda.toString m, Moneda.simboloUnico m )))
                            monedaField
                            [ style "max-width" "6.5rem" ]
                    , div [ class "flex-grow-1" ]
                        [ Html.map PagoForm <|
                            Bs.montoInput (decimalesDelForm form) montoField [ placeholder "33.000,00" ]
                        ]
                    ]
                ]
            , Html.map PagoForm <|
                Bs.dateFormItem fechaField
                    { label = "Fecha"
                    , required = True
                    }
            ]
        , div [ class "mt-auto pt-4" ]
            [ viewBotonera
                [ Bs.btn Bs.Primary
                    [ disabled (Form.getOutput form == Nothing)
                    , onClick SubmitCurrentSection
                    , class "flex-grow-1"
                    ]
                    [ text "Siguiente" ]
                ]
            ]
        ]


viewPagadoresSection : Grupo -> Edicion -> Html Msg
viewPagadoresSection grupo model =
    let
        form =
            model.pagadoresForm

        prefix =
            "distribucion_pagadores"

        incluidos =
            grupo.participantes
                |> List.filter
                    (\participante ->
                        (Form.getFieldAsBool (prefix ++ ".partes." ++ participante.id ++ ".participa") form).value == Just True
                    )

        mode =
            { mostrarPartes =
                (Form.getFieldAsBool (prefix ++ "." ++ mostrarPartesField) form).value == Just True
            , mostrarMontoFijo =
                (Form.getFieldAsBool (prefix ++ "." ++ mostrarMontoFijoField) form).value == Just True
            }
    in
    Html.form [ onSubmit SubmitCurrentSection, class "flex-grow-1 d-flex flex-column" ]
        [ div [ class "d-flex flex-wrap align-items-center gap-2 mb-2" ]
            [ Html.h5 [ class "mb-0" ] [ text "Quienes pagaron" ] ]
        , viewSeleccionarParticipantes
            { titulo = "Quienes pagaron", id = "pagadores-seleccionar" }
            grupo.participantes
            prefix
            form
        , Html.hr [ class "my-4" ] []
        , div [ class "d-flex flex-wrap gap-2 mb-3" ]
            [ viewModoChip (prefix ++ "." ++ mostrarMontoFijoField) mode.mostrarMontoFijo "Monto fijo"
            , viewModoChip (prefix ++ "." ++ mostrarPartesField) mode.mostrarPartes "Partes"
            ]
        , viewPartesTable grupo.monedaPorDefecto grupo.participantes prefix mode incluidos (Form.getOutput form |> Maybe.map (\pago -> sumaMontosFijos pago.pagadores)) form
        , div [ class "mt-auto pt-4" ]
            [ viewErrorFromResumenData (netosDelPaso Pagadores model)
            , viewBotonera
                [ viewTortaFooter grupo Pagadores model
                , Bs.btn Bs.Primary
                    [ disabled (Form.getOutput form == Nothing)
                    , onClick SubmitCurrentSection
                    , class "flex-grow-1"
                    ]
                    [ text "Siguiente" ]
                ]
            ]
        ]


viewDeudoresSection : Grupo -> Edicion -> Html Msg
viewDeudoresSection grupo model =
    let
        form =
            model.deudoresForm

        tipoField =
            Form.getFieldAsString "distribucion_deudores.tipo" form
    in
    Html.form [ onSubmit <| PagoForm Form.Submit, class "flex-grow-1 d-flex flex-column" ]
        [ viewReceiptBanner model.receiptParseState
        , viewModalidadSelector tipoField
        , case tipoField.value of
            Just "repartija" ->
                div []
                    [ viewRepartijaForm grupo.monedaPorDefecto "distribucion_deudores" form
                    , viewRepartijaLink model.grupoId form
                    ]

            Just "partes" ->
                viewPartesForm grupo "distribucion_deudores" form

            _ ->
                text ""
        , div [ class "mt-auto pt-4" ]
            [ -- Sólo los errores de esta sección (deudores) y sin el tag de
              -- scope: `.resumenDeudores` ya viene sin el prefijo "deudores"
              -- que `.resumen` agrega al combinar pagadores y deudores.
              viewErrorFromResumenData (netosDelPaso Deudores model)
            , viewBotonera
                [ viewTortaFooter grupo Deudores model
                , Bs.btn Bs.Primary
                    [ -- Se permite enviar aunque el gasto sea inválido; el
                      -- backend lo guarda igual y deja los motivos en su
                      -- resumen. Se bloquea si el form no es construible o
                      -- si no hay cambios para guardar.
                      disabled (Form.getOutput model.pagoForm == Nothing || not model.hasUnsavedChanges)
                    , onClick (PagoForm Form.Submit)
                    , Attr.id "pago-submit-button"
                    , class "flex-grow-1"
                    ]
                    [ text <|
                        case model.pagoId of
                            Just _ ->
                                "Actualizar gasto"

                            Nothing ->
                                "Crear"
                    ]
                ]
            ]
        ]


{-| La fila de botones del pie: lo que venga (la torta, el botón del paso) más
el "Cancelar" que vuelve al detalle del gasto.
-}
viewBotonera : List (Html Msg) -> Html Msg
viewBotonera children =
    div [ class "d-flex gap-2 align-items-stretch" ]
        (Bs.btn Bs.Transparent [ onClick Cancel ] [ text "Cancelar" ] :: children)


viewModalidadSelector : Form.FieldState CustomFormError String -> Html Msg
viewModalidadSelector tipoField =
    let
        card value icono titulo descripcion attrs =
            let
                active =
                    tipoField.value == Just value

                -- Borde resaltado con `--bs-emphasis-color`, que se invierte según
                -- el tema (casi negro en claro, casi blanco en oscuro), así se ve
                -- bien en ambos. Sin esto un borde fijo oscuro quedaría invisible
                -- sobre el fondo oscuro.
                emphasisBorder =
                    if active then
                        [ style "border-color" "var(--bs-emphasis-color)" ]

                    else
                        []
            in
            button
                ([ type_ "button"
                 , classList
                    [ ( "card flex-fill text-center border-2 position-relative", True )
                    , ( "shadow-sm", active )
                    ]
                 , onClick <| PagoForm <| Form.Input tipoField.path Form.Select (FormField.String value)
                 ]
                    ++ emphasisBorder
                    ++ attrs
                )
                [ -- El círculo del ícono se posiciona absoluto sobre el borde
                  -- superior (translate-middle lo centra ahí) y lleva fondo
                  -- `bg-body` para tapar la línea del borde por detrás.
                  span
                    [ class "position-absolute top-0 start-50 translate-middle d-inline-flex align-items-center justify-content-center rounded-circle fs-4 bg-body"
                    , style "width" "3.5rem"
                    , style "height" "3.5rem"
                    , style "border-style" "solid"
                    , style "border-width"
                        (if active then
                            "2px"

                         else
                            "var(--bs-border-width)"
                        )
                    , style "border-color"
                        (if active then
                            "var(--bs-emphasis-color)"

                         else
                            "var(--bs-border-color)"
                        )
                    ]
                    [ i [ class icono ] [] ]
                , div [ class "card-body px-2 pb-2", style "padding-top" "2.25rem" ]
                    [ Html.h6 [ class "mb-1" ] [ text titulo ]
                    , p [ class "text-body-secondary small mb-0" ] [ text descripcion ]
                    ]
                ]
    in
    div [ class "mb-4" ]
        [ Html.h5 [ class "mb-2" ] [ text "Modalidad" ]
        , div [ class "d-flex gap-3 mt-4" ]
            [ card "partes"
                "bi bi-robot"
                "Clásica"
                "Repartí de manera automática el gasto, con varias modalidades"
                [ Attr.id "deudores-modalidad" ]
            , card "repartija"
                "bi bi-people-fill"
                "Repartija"
                "Repartí el gasto como items de un recibo y realizá una carga colaborativa"
                []
            ]
        ]


viewReceiptBanner : Maybe ReceiptReadingState -> Html Msg
viewReceiptBanner receiptParseState =
    div [ class "card mb-4" ]
        [ div [ class "card-body" ]
            [ Html.h6 [ class "mb-1" ] [ i [ class "bi bi-stars me-1" ] [], text "Leer recibo con IA" ]
            , p [ class "text-body-secondary small mb-2" ]
                [ text "Subí una foto del recibo y cargá los items automáticamente" ]
            , Bs.fileInput
                [ accept "image/*"
                , on "change" (Decode.map ReceiptImageSelected fileDecoder)
                ]
            , case receiptParseState of
                Just ReadingFile ->
                    div [ class "d-flex gap-2 align-items-center mt-3" ]
                        [ Bs.spinner [ Attr.attribute "aria-hidden" "true" ]
                        , Bs.alert Bs.AlertInfo
                            [ style "margin-bottom" "0", style "flex" "1" ]
                            [ text "Leyendo la imagen..." ]
                        ]

                Just ProcessingWithAI ->
                    div [ class "mt-3" ]
                        [ div [ class "d-flex gap-2 align-items-center mb-3" ]
                            [ Bs.spinner [ Attr.attribute "aria-hidden" "true" ]
                            , Bs.alert Bs.AlertInfo
                                [ style "margin-bottom" "0", style "flex" "1" ]
                                [ text "Analizando el recibo con inteligencia artificial..." ]
                            ]
                        , Bs.alert Bs.AlertWarning
                            [ style "margin-bottom" "0" ]
                            [ text "Esto podria tomar varios minutos, no cierres esta ventana" ]
                        ]

                Just (ErrorProcessing errorMsg) ->
                    Bs.alert Bs.AlertDanger
                        [ class "mt-3"
                        , style "margin-bottom" "0"
                        , style "display" "flex"
                        , style "align-items" "center"
                        , style "gap" "0.5rem"
                        ]
                        [ span [ style "flex" "1" ] [ text ("Algo salio mal: " ++ errorMsg) ]
                        , button
                            [ type_ "button"
                            , class "btn-close"
                            , Attr.attribute "aria-label" "Cerrar"
                            , onClick ClearReceiptError
                            ]
                            []
                        ]

                Nothing ->
                    text ""
            ]
        ]


viewPartesForm : Grupo -> String -> Form CustomFormError Pago -> Html Msg
viewPartesForm grupo prefix form =
    let
        incluidos =
            grupo.participantes
                |> List.filter
                    (\participante ->
                        (Form.getFieldAsBool (prefix ++ ".partes." ++ participante.id ++ ".participa") form).value == Just True
                    )

        mode =
            { mostrarPartes =
                (Form.getFieldAsBool (prefix ++ "." ++ mostrarPartesField) form).value == Just True
            , mostrarMontoFijo =
                (Form.getFieldAsBool (prefix ++ "." ++ mostrarMontoFijoField) form).value == Just True
            }
    in
    div [ class "mb-4" ]
        [ div [ class "d-flex flex-wrap align-items-center gap-2 mb-2" ]
            [ Html.h5 [ class "mb-0" ] [ text "Quienes participan" ] ]
        , viewSeleccionarParticipantes
            { titulo = "Quienes participan", id = "deudores-seleccionar" }
            grupo.participantes
            prefix
            form
        , Html.hr [ class "my-4" ] []
        , div [ class "d-flex flex-wrap gap-2 mb-3" ]
            [ viewModoChip (prefix ++ "." ++ mostrarPartesField) mode.mostrarPartes "Partes"
            , viewModoChip (prefix ++ "." ++ mostrarMontoFijoField) mode.mostrarMontoFijo "Monto fijo"
            ]
        , viewPartesTable grupo.monedaPorDefecto grupo.participantes prefix mode incluidos (Form.getOutput form |> Maybe.map (\pago -> sumaMontosFijos pago.deudores)) form
        ]


{-| Pill con checkbox interno para activar/desactivar un modo de reparto
("Partes" o "Monto fijo"). El cuadradito refleja el estado igual que un
checkbox; al togglearlo se muestra u oculta la columna correspondiente.
-}
viewModoChip : String -> Bool -> String -> Html Msg
viewModoChip path active label =
    button
        [ type_ "button"
        , class
            ("btn rounded-pill d-inline-flex align-items-center gap-2 "
                ++ (if active then
                        "btn-secondary"

                    else
                        "btn-outline-secondary"
                   )
            )
        , Attr.attribute "aria-pressed"
            (if active then
                "true"

             else
                "false"
            )
        , onClick <| PagoForm <| Form.Input path Form.Checkbox (FormField.Bool (not active))
        ]
        [ i
            [ class
                (if active then
                    "bi bi-check-square-fill"

                 else
                    "bi bi-square"
                )
            ]
            []
        , text label
        ]


{-| Selector de participantes como grupo de pills. En desktop se muestran
directamente; en mobile se esconden detrás de un botón que abre el selector.

A diferencia de la pantalla completa, el selector no es un modal de Bootstrap
(no se pueden anidar) sino el overlay de `viewSelectorOverlay`, que se dibuja
por encima del popup y lo maneja el `Edicion`.

-}
viewSeleccionarParticipantes : { titulo : String, id : String } -> List Participante -> String -> Form CustomFormError Pago -> Html Msg
viewSeleccionarParticipantes selector participantes prefix form =
    div []
        [ div [ class "d-none d-md-block" ] [ viewPills participantes prefix form ]
        , div [ class "d-md-none" ]
            [ button
                [ type_ "button"
                , class "btn btn-light rounded-pill d-inline-flex align-items-center gap-2"
                , Attr.id selector.id
                , onClick (AbrirSelector { titulo = selector.titulo, prefix = prefix })
                ]
                [ i [ class "bi bi-person-fill" ] [], text "Seleccionar" ]
            ]
        ]


viewPills : List Participante -> String -> Form CustomFormError Pago -> Html Msg
viewPills participantes prefix form =
    div [ class "d-flex flex-wrap gap-2" ]
        (participantes |> List.map (\participante -> viewParticipantePill participante prefix form))


{-| El selector de participantes de mobile, encima del popup. Los z-index son
los mismos que usa el popup para los gráficos, así queda por arriba de su
diálogo y de su backdrop.
-}
viewSelectorOverlay : List Participante -> Edicion -> Html Msg
viewSelectorOverlay participantes model =
    case model.selectorAbierto of
        Nothing ->
            text ""

        Just { titulo, prefix } ->
            let
                form =
                    formDeLaSeccion model
            in
            div []
                [ div
                    [ class "modal d-block"
                    , style "z-index" "1070"
                    , Attr.tabindex -1
                    , Attr.attribute "aria-modal" "true"
                    , Attr.attribute "role" "dialog"
                    ]
                    [ div [ class "modal-dialog modal-dialog-scrollable modal-fullscreen-sm-down" ]
                        [ div [ class "modal-content" ]
                            [ div [ class "modal-header" ]
                                [ Html.h5 [ class "modal-title" ] [ text titulo ]
                                , button
                                    [ type_ "button"
                                    , class "btn-close"
                                    , Attr.attribute "aria-label" "Cerrar"
                                    , onClick CerrarSelector
                                    ]
                                    []
                                ]
                            , div [ class "modal-body" ] [ viewPills participantes prefix form ]
                            , div [ class "modal-footer" ]
                                [ Bs.btn Bs.Primary
                                    [ class "w-100", onClick CerrarSelector ]
                                    [ text "Listo" ]
                                ]
                            ]
                        ]
                    ]
                , div [ class "modal-backdrop show", style "z-index" "1065" ] []
                ]


{-| El form del paso que se está mostrando, que es de donde el selector tiene
que leer quién participa.
-}
formDeLaSeccion : Edicion -> Form CustomFormError Pago
formDeLaSeccion model =
    case model.currentSection of
        BasicPagoData ->
            model.pagoBasicoForm

        PagadoresSection ->
            model.pagadoresForm

        DeudoresSection ->
            model.deudoresForm


viewParticipantePill : Participante -> String -> Form CustomFormError Pago -> Html Msg
viewParticipantePill participante prefix form =
    let
        participaField =
            Form.getFieldAsBool (prefix ++ ".partes." ++ participante.id ++ ".participa") form

        participa =
            participaField.value == Just True
    in
    button
        [ type_ "button"
        , class "btn btn-light rounded-pill d-inline-flex align-items-center gap-2"
        , Attr.attribute "aria-pressed"
            (if participa then
                "true"

             else
                "false"
            )
        , onClick <| PagoForm <| Form.Input participaField.path Form.Checkbox (FormField.Bool (not participa))
        ]
        [ i
            [ class
                (if participa then
                    "bi bi-check-square-fill"

                 else
                    "bi bi-square"
                )
            ]
            []
        , text participante.nombre
        ]


{-| Con cuántos decimales dejan tipear los inputs de monto de este form, según
la moneda elegida.
-}
decimalesDelForm : Form CustomFormError Pago -> Int
decimalesDelForm form =
    monedaDelForm form
        |> Maybe.map escalaDe
        |> Maybe.withDefault 2


{-| La moneda elegida en el formulario, que no tiene por qué ser la del grupo.
-}
monedaDelForm : Form CustomFormError Pago -> Maybe Moneda
monedaDelForm form =
    (Form.getFieldAsString "moneda" form).value
        |> Maybe.andThen Moneda.fromString


{-| Un total del formulario con el símbolo de la moneda que el form tiene
elegida. Con un "$" fijo, un gasto en dólares se mostraba como pesos.
-}
totalConSimbolo : Moneda -> Form CustomFormError Pago -> Maybe Monto -> String
totalConSimbolo monedaPorDefecto form monto =
    let
        moneda =
            monedaDelForm form |> Maybe.withDefault monedaPorDefecto
    in
    Moneda.simbolo monedaPorDefecto moneda
        ++ " "
        ++ (monto |> Maybe.map Monto.toString |> Maybe.withDefault "—")


{-| Indica si la columna "División" debe mostrarse. Sólo se oculta cuando el
único modo activo es el de monto fijo (no hay nada que repartir por partes).
-}
divisionVisible : ModoPartes -> Bool
divisionVisible mode =
    mode.mostrarPartes || not mode.mostrarMontoFijo


{-| Suma de los montos fijos de una distribución, usada en la fila "Suma". Toma
las `Parte` ya parseadas del `Pago` del form, así que no reparsea strings.
-}
sumaMontosFijos : Distribucion -> Monto
sumaMontosFijos distribucion =
    case distribucion.tipo of
        Api.TipoDistribucionPartes dp ->
            dp.partes
                |> List.filterMap
                    (\parte ->
                        case parte of
                            Api.MontoFijo monto _ ->
                                Just monto

                            Api.PonderadoYMontoFijo monto _ _ ->
                                Just monto

                            Api.Ponderado _ _ ->
                                Nothing
                    )
                |> List.foldl Monto.add Monto.zero

        Api.TipoDistribucionRepartija _ ->
            Monto.zero


{-| Total del listado de una repartija: la suma de los montos de cada ítem (la
cantidad no multiplica, sólo se usa al reclamar). No incluye la propina.
-}
totalItemsRepartija : Distribucion -> Monto
totalItemsRepartija distribucion =
    case distribucion.tipo of
        Api.TipoDistribucionRepartija repartija ->
            repartija.items
                |> List.map .monto
                |> List.foldl Monto.add Monto.zero

        Api.TipoDistribucionPartes _ ->
            Monto.zero


{-| Tabla de partes con columnas dinámicas: "Monto fijo" cuando ese modo está
activo y "División" (contador de partes o "En partes iguales") salvo cuando el
único modo es el de monto fijo. `suma` es el total de montos fijos parseado del
form, mostrado en el pie cuando se editan montos.
-}
viewPartesTable : Moneda -> List Participante -> String -> ModoPartes -> List Participante -> Maybe Monto -> Form CustomFormError Pago -> Html Msg
viewPartesTable monedaPorDefecto participantesDelGrupo prefix mode incluidos suma form =
    let
        headerCells =
            Html.th [ Attr.scope "col" ] [ text "Participante" ]
                :: (if mode.mostrarMontoFijo then
                        [ Html.th [ Attr.scope "col", class "text-end" ] [ text "Monto fijo" ] ]

                    else
                        []
                   )
                ++ (if divisionVisible mode then
                        [ Html.th [ Attr.scope "col", class "text-end" ] [ text "Partes" ] ]

                    else
                        []
                   )
    in
    div [ class "table-responsive" ]
        [ Html.table [ class "table align-middle" ]
            [ Html.thead [] [ Html.tr [] headerCells ]
            , Html.tbody []
                (incluidos |> List.map (\participante -> viewParteRow participantesDelGrupo prefix mode participante form))
            , if mode.mostrarMontoFijo || mode.mostrarPartes then
                let
                    sumaRow =
                        Html.tr [ class "text-body-secondary" ]
                            (Html.td [] [ text "Total" ]
                                :: (if mode.mostrarMontoFijo then
                                        [ Html.td [ class "text-end" ]
                                            [ text (totalConSimbolo monedaPorDefecto form suma) ]
                                        ]

                                    else
                                        []
                                   )
                                ++ (if divisionVisible mode then
                                        [ Html.td [ class "text-end" ]
                                            [ if mode.mostrarPartes then
                                                let
                                                    totalPartes =
                                                        incluidos
                                                            |> List.map
                                                                (\participante ->
                                                                    (Form.getFieldAsString (prefix ++ ".partes." ++ participante.id ++ ".cuota") form).value
                                                                        |> Maybe.andThen String.toInt
                                                                        |> Maybe.withDefault 0
                                                                )
                                                            |> List.sum
                                                in
                                                text (String.fromInt totalPartes ++ " partes")

                                              else
                                                text ""
                                            ]
                                        ]

                                    else
                                        []
                                   )
                            )
                in
                Html.tfoot [] [ sumaRow ]

              else
                text ""
            ]
        ]


viewParteRow : List Participante -> String -> ModoPartes -> Participante -> Form CustomFormError Pago -> Html Msg
viewParteRow participantesDelGrupo prefix mode participante form =
    let
        basePath =
            prefix ++ ".partes." ++ participante.id
    in
    Html.tr []
        (Html.td [ class "fw-bold" ]
            [ div [ class "d-flex align-items-center gap-2" ]
                [ GraficoTorta.viewDot (GraficoTorta.colorParaParticipante participantesDelGrupo participante.id)
                , text participante.nombre
                ]
            ]
            :: (if mode.mostrarMontoFijo then
                    let
                        montoField =
                            Form.getFieldAsString (basePath ++ ".monto") form
                    in
                    [ Html.td [ class "text-end" ]
                        [ div [ class "ms-auto", style "width" "11rem" ]
                            [ Html.map PagoForm <|
                                Bs.montoInput (decimalesDelForm form) montoField [ placeholder "13.000,00", style "text-align" "right" ]
                            ]
                        ]
                    ]

                else
                    []
               )
            ++ (if divisionVisible mode then
                    [ Html.td [ class "text-end" ]
                        [ if mode.mostrarPartes then
                            viewCuotaCounter (basePath ++ ".cuota") form

                          else
                            span [ class "text-body-secondary" ] [ text "En partes iguales" ]
                        ]
                    ]

                else
                    []
               )
        )


viewCuotaCounter : String -> Form CustomFormError Pago -> Html Msg
viewCuotaCounter path form =
    let
        cuotaField =
            Form.getFieldAsString path form

        cuota =
            cuotaField.value
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 0

        setCuota nuevaCuota =
            PagoForm <| Form.Input cuotaField.path Form.Text (FormField.String (String.fromInt (max 0 nuevaCuota)))
    in
    div [ class "d-flex align-items-center justify-content-end gap-2" ]
        [ button
            [ type_ "button"
            , class "btn btn-dark btn-sm rounded-circle"
            , Attr.attribute "aria-label" "Menos partes"
            , onClick (setCuota (cuota - 1))
            ]
            [ i [ class "bi bi-dash" ] [] ]
        , span [ class "px-1" ] [ text (String.fromInt cuota) ]
        , button
            [ type_ "button"
            , class "btn btn-dark btn-sm rounded-circle"
            , Attr.attribute "aria-label" "Más partes"
            , onClick (setCuota (cuota + 1))
            ]
            [ i [ class "bi bi-plus" ] [] ]
        ]


viewRepartijaLink : ULID -> Form CustomFormError Pago -> Html Msg
viewRepartijaLink grupoId form =
    let
        repartijaId =
            (Form.getFieldAsString "distribucion_deudores.repartija_id" form).value
                |> Maybe.withDefault emptyUlid
    in
    if repartijaId /= emptyUlid && repartijaId /= "" then
        p []
            [ a
                [ class "link-primary"
                , Path.href <| Path.Grupos_GrupoId__Repartijas_RepartijaId_ { grupoId = grupoId, repartijaId = repartijaId }
                , target "_blank"
                ]
                [ text "Ver repartija colaborativa" ]
            ]

    else
        text ""


fileDecoder : Decode.Decoder File
fileDecoder =
    Decode.at [ "target", "files" ] (Decode.list File.decoder)
        |> Decode.andThen
            (\files ->
                case files of
                    [] ->
                        Decode.fail "No files selected"

                    [ file ] ->
                        Decode.succeed file

                    _ ->
                        Decode.fail "Too many files"
            )


viewRepartijaForm : Moneda -> String -> Form CustomFormError Pago -> Html Msg
viewRepartijaForm monedaPorDefecto prefix form =
    let
        montoField =
            Form.getFieldAsString (prefix ++ ".extra") form

        itemsIndexes =
            Form.getListIndexes (prefix ++ ".items") form
    in
    div []
        [ div [ class "mb-4" ]
            [ div [ class "d-flex justify-content-between align-items-center mb-2" ]
                [ Html.h6 [ class "mb-0" ] [ text "Ítems" ]
                , Bs.btn Bs.Secondary
                    [ onClick <| PagoForm <| Form.Append <| prefix ++ ".items"
                    , type_ "button"
                    , class "btn-sm rounded-pill"
                    ]
                    [ i [ class "bi bi-plus me-1" ] [], text "Item" ]
                ]
            , div [ class "table-responsive" ]
                [ Html.table [ class "table align-middle", style "min-width" "40rem" ]
                    [ Html.thead []
                        [ Html.tr []
                            [ Html.th [ Attr.scope "col" ] [ text "Nombre" ]
                            , Html.th [ Attr.scope "col", style "width" "5rem" ] [ text "Cantidad" ]
                            , Html.th [ Attr.scope "col" ] [ text "Monto" ]
                            , Html.th [ Attr.scope "col", style "width" "1%" ] []
                            ]
                        ]
                    , Html.tbody []
                        (List.map (\idx -> viewRepartijaItemForm idx prefix form) itemsIndexes)
                    , Html.tfoot []
                        [ Html.tr [ class "text-body-secondary" ]
                            [ Html.td [] [ text "Total" ]
                            , Html.td [] []
                            , Html.td []
                                [ text
                                    (totalConSimbolo monedaPorDefecto
                                        form
                                        (Form.getOutput form
                                            |> Maybe.map (\pago -> totalItemsRepartija pago.deudores)
                                        )
                                    )
                                ]
                            , Html.td [ style "width" "1%" ] []
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "mb-4" ]
            [ Html.h6 [ class "mb-2" ] [ text "Propina" ]
            , Html.map PagoForm <|
                Bs.montoInput (decimalesDelForm form) montoField [ placeholder "1000" ]
            ]
        , Html.hr [ class "my-3" ] []
        , div [ class "mb-4" ]
            [ Html.h6 [ class "mb-2" ] [ text "Distribución de sobras" ]
            , p [ class "text-body-secondary mb-2", style "font-size" "0.875rem" ]
                [ text "Qué hacer con los items que nadie reclamó. Podés cambiar esta opción en cualquier momento." ]
            , let
                distribucionDeSobrasField =
                    Form.getFieldAsString (prefix ++ ".distribucionDeSobras") form

                selectSobras v =
                    PagoForm <| Form.Input distribucionDeSobrasField.path Form.Select (FormField.String v)
              in
              Bs.segmentedButton []
                [ Bs.segmentedButtonItem
                    { active = distribucionDeSobrasField.value == Just "SobrasNoDistribuir"
                    , onSelect = selectSobras "SobrasNoDistribuir"
                    }
                    []
                    [ text "No distribuir" ]
                , Bs.segmentedButtonItem
                    { active = distribucionDeSobrasField.value == Just "SobrasProporcional"
                    , onSelect = selectSobras "SobrasProporcional"
                    }
                    []
                    [ text "Proporcional" ]
                ]
            ]
        ]


viewRepartijaItemForm : Int -> String -> Form CustomFormError Pago -> Html Msg
viewRepartijaItemForm i prefix form =
    let
        nombreField =
            Form.getFieldAsString (prefix ++ ".items." ++ String.fromInt i ++ ".nombre") form

        montoField =
            Form.getFieldAsString (prefix ++ ".items." ++ String.fromInt i ++ ".monto") form

        cantidadField =
            Form.getFieldAsString (prefix ++ ".items." ++ String.fromInt i ++ ".cantidad") form
    in
    Html.tr []
        [ Html.td []
            [ Html.map PagoForm <|
                Bs.textInput nombreField [ placeholder "Birrita" ]
            ]
        , Html.td [ style "width" "5rem" ]
            [ Html.map PagoForm <|
                Bs.textInput cantidadField [ placeholder "4" ]
            ]
        , Html.td []
            [ Html.map PagoForm <|
                Bs.montoInput (decimalesDelForm form) montoField [ placeholder "20.000", style "text-align" "right" ]
            ]
        , Html.td [ style "width" "1%" ]
            [ Bs.btn Bs.Danger
                [ type_ "button"
                , onClick <| PagoForm <| Form.RemoveItem (prefix ++ ".items") i
                , Attr.attribute "aria-label" "Eliminar item"
                ]
                [ Html.i [ class "bi bi-trash" ] [] ]
            ]
        ]


allowedMimeTypesForReceiptUpload : List String
allowedMimeTypesForReceiptUpload =
    [ "image/png"
    , "image/jpeg"
    , "image/webp"
    , "image/gif"
    ]


updateAllForms : List Participante -> List Form.Msg -> Edicion -> Edicion
updateAllForms participantes formMsgs model =
    List.foldl
        (\formMsg m ->
            { m
                | pagoForm = Form.update (validatePago participantes) formMsg m.pagoForm
                , pagoBasicoForm = Form.update (validatePagoInSection BasicPagoData participantes) formMsg m.pagoBasicoForm
                , pagadoresForm = Form.update (validatePagoInSection PagadoresSection participantes) formMsg m.pagadoresForm
                , deudoresForm = Form.update (validatePagoInSection DeudoresSection participantes) formMsg m.deudoresForm
            }
        )
        model
        formMsgs


andThenFocusFieldIfSectionChanged : Section -> ( Edicion, Effect Msg ) -> ( Edicion, Effect Msg )
andThenFocusFieldIfSectionChanged oldSection ( model, oldEffects ) =
    let
        focusEffect =
            if oldSection == model.currentSection then
                Effect.none

            else
                case model.currentSection of
                    BasicPagoData ->
                        Effect.sendCmd <| Task.attempt (\_ -> NoOp) (Browser.Dom.focus "nombre")

                    PagadoresSection ->
                        Effect.sendCmd <| Task.attempt (\_ -> NoOp) (Browser.Dom.focus "pagadores-seleccionar")

                    DeudoresSection ->
                        Effect.sendCmd <| Task.attempt (\_ -> NoOp) (Browser.Dom.focus "deudores-modalidad")
    in
    ( model
    , Effect.batch [ oldEffects, focusEffect ]
    )


andThenUpdateResumenesFromForms : Edicion -> ( Edicion, Effect Msg ) -> ( Edicion, Effect Msg )
andThenUpdateResumenesFromForms originalModel ( model, oldEffects ) =
    let
        updateResumenFromForm getForm event =
            case Form.getOutput (getForm model) of
                Just pago ->
                    if Form.getOutput (getForm originalModel) == Just pago then
                        Effect.none

                    else
                        let
                            pagoWithClaims =
                                mergeClaimsIntoPago model.storedClaims pago
                        in
                        Effect.batch
                            [ Effect.sendMsg <| event Loading
                            , Effect.sendCmd <| Api.postPagosResumen pagoWithClaims (RemoteData.fromResult >> event)
                            ]

                Nothing ->
                    Effect.sendMsg <| event NotAsked
    in
    ( model
    , Effect.batch
        [ oldEffects
        , updateResumenFromForm .pagadoresForm ResumenPagadoresUpdated
        , updateResumenFromForm .deudoresForm ResumenDeudoresUpdated
        ]
    )



-- VIEW


{-| Los mensajes que solo tocan el formulario. Los que cambian el estado del
popup —cancelar y la respuesta de guardar— los maneja `update`, que es el que
sabe qué hacer con el popup entero.
-}
updateEdicion : List Participante -> Msg -> Edicion -> ( Edicion, Effect Msg )
updateEdicion participantes msg model =
    case msg of
        PagoForm Form.Submit ->
            case Form.getOutput model.pagoForm of
                Just pago ->
                    let
                        pagoConClaims =
                            mergeClaimsIntoPago model.storedClaims pago
                    in
                    ( { model | pagoForm = Form.update (validatePago participantes) Form.Submit model.pagoForm }
                    , Effect.sendCmd <|
                        case model.pagoId of
                            Just pagoId ->
                                Api.putGrupoByIdPagosByPagoId model.grupoId pagoId pagoConClaims GuardadoPagoResponse

                            Nothing ->
                                Api.postGrupoByIdPagos model.grupoId pagoConClaims GuardadoPagoResponse
                    )

                Nothing ->
                    ( { model | pagoForm = Form.update (validatePago participantes) Form.Submit model.pagoForm }
                    , Effect.none
                    )

        PagoForm formMsg ->
            let
                newModel =
                    updateAllForms participantes [ formMsg ] model
            in
            ( { newModel
                | hasUnsavedChanges =
                    if isDataModifyingEvent formMsg then
                        True

                    else
                        model.hasUnsavedChanges
              }
            , Effect.none
            )
                |> andThenUpdateResumenesFromForms model

        ResumenDeudoresUpdated resumen ->
            ( { model | resumenDeudores = resumen }, Effect.none )

        ResumenPagadoresUpdated resumen ->
            ( { model | resumenPagadores = resumen }, Effect.none )

        ClearReceiptError ->
            ( { model | receiptParseState = Nothing }, Effect.none )

        AbrirSelector selector ->
            ( { model | selectorAbierto = Just selector }, Effect.none )

        CerrarSelector ->
            ( { model | selectorAbierto = Nothing }, Effect.none )

        ReceiptImageSelected file ->
            if List.member (File.mime file) allowedMimeTypesForReceiptUpload then
                ( { model | receiptParseState = Just ReadingFile }
                , Effect.sendCmd <| Task.perform (ReceiptImageBytes file) (File.toBytes file)
                )

            else
                ( { model | receiptParseState = Just <| ErrorProcessing <| "Este archivo no es una imagen (" ++ File.mime file ++ ")" }
                , Effect.none
                )

        ReceiptImageBytes file bytes ->
            let
                base64 =
                    Base64.Encode.encode (Base64.Encode.bytes bytes)
            in
            ( { model | receiptParseState = Just ProcessingWithAI }
            , Effect.sendCmd <| Api.postReceiptParseimage { imageBase64 = File.mime file ++ ";base64," ++ base64 } ReceiptParseResponse
            )

        ReceiptParseResponse result ->
            case result of
                Ok (Api.ReceiptImageSuccess { items }) ->
                    let
                        formMsgs =
                            Form.Input "distribucion_deudores.tipo" Form.Select (FormField.String "repartija")
                                :: receiptItemsFormMsgs "distribucion_deudores" items model.pagoForm

                        newModel =
                            updateAllForms participantes formMsgs { model | hasUnsavedChanges = True }
                    in
                    ( { newModel | receiptParseState = Nothing }
                    , Toasts.pushToast Toasts.ToastSuccess "Recibo parseado correctamente"
                    )
                        |> andThenUpdateResumenesFromForms model

                Ok (Api.ReceiptImageError { error }) ->
                    ( { model | receiptParseState = Just (ErrorProcessing error) }, Effect.none )

                Err _ ->
                    ( { model | receiptParseState = Just (ErrorProcessing "Error al enviar la imagen") }, Effect.none )

        SelectSection section ->
            -- Cambiar de paso cierra el selector: es de la sección que se deja.
            ( { model | currentSection = section, selectorAbierto = Nothing }, Effect.none )
                |> andThenFocusFieldIfSectionChanged model.currentSection
                |> andThenUpdateResumenesFromForms model

        SubmitCurrentSection ->
            case model.currentSection of
                BasicPagoData ->
                    ( { model
                        | pagoBasicoForm = Form.update (validatePagoInSection BasicPagoData participantes) Form.Submit model.pagoBasicoForm
                        , currentSection = PagadoresSection
                        , selectorAbierto = Nothing
                      }
                    , Effect.none
                    )
                        |> andThenFocusFieldIfSectionChanged model.currentSection
                        |> andThenUpdateResumenesFromForms model

                PagadoresSection ->
                    ( { model
                        | pagadoresForm = Form.update (validatePagoInSection PagadoresSection participantes) Form.Submit model.pagadoresForm
                        , currentSection = DeudoresSection
                        , selectorAbierto = Nothing
                      }
                    , Effect.none
                    )
                        |> andThenFocusFieldIfSectionChanged model.currentSection
                        |> andThenUpdateResumenesFromForms model

                DeudoresSection ->
                    updateEdicion participantes (PagoForm Form.Submit) model

        _ ->
            ( model, Effect.none )
