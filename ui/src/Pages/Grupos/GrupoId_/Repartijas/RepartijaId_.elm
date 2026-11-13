module Pages.Grupos.GrupoId_.Repartijas.RepartijaId_ exposing (AccionEnCurso, Ayuda, Feedback, Filtros, Model, Msg, OrigenAyuda, TipoDeAccion, page)

import Components.Bootstrap as Bs
import Effect exposing (Effect)
import Form exposing (Form)
import Form.Init as Form
import Form.Validate as V exposing (Validation)
import Generated.Api as Api exposing (AccionDeRepartija(..), AccionDeRepartijaInvalida(..), DetalleDeItem, DetalleRepartija, DiferenciaDeUnidades(..), DistribucionDeSobras(..), EstadoDeItem(..), Moneda, Repartija, RepartijaClaim, RepartijaForFrontend, RepartijaItem, ResolucionDeItem(..), ResultadoAccionDeRepartija(..), ResultadoPreviewDeAccion(..), ULID)
import Generated.Moneda exposing (escalaDe)
import Html exposing (Html, button, div, h4, h5, h6, i, input, p, span, strong, text)
import Html.Attributes as Attr exposing (class, classList, disabled, type_, value)
import Html.Events exposing (onCheck, onClick, onInput, onSubmit)
import Layouts
import List.Extra exposing (find)
import Models.Grupo exposing (GrupoLike, lookupNombreParticipante)
import Models.Monto as Monto
import Models.Opinion as Opinion exposing (Opinion(..))
import Models.Repartija as Repartija
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Set exposing (Set)
import Shared
import Shared.Msg
import Utils.Form exposing (CustomFormError, anyString, isDataModifyingEvent, textInput)
import Utils.Http exposing (viewHttpError)
import Utils.Telemetry as Telemetry
import Utils.Toasts.Types exposing (ToastLevel(..))
import Utils.Ulid exposing (emptyUlid)
import View exposing (View)


page : Shared.Model -> Route { grupoId : String, repartijaId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.grupoId route.params.repartijaId shared.store
        , update = update (Shared.currentParticipante shared route.params.grupoId) shared.store
        , subscriptions = subscriptions
        , view =
            view (Shared.currentParticipante shared route.params.grupoId)
                { introDismissed = shared.repartijaIntroDismissed
                }
                shared.store
        }
        |> Page.withLayout (\_ -> Layouts.Default_Grupo {})



-- INIT


type alias Model =
    { grupoId : ULID
    , repartijaId : ULID
    , pendingItemOperation : Maybe ULID
    , repartirModalItem : Maybe RepartijaItem
    , detalleModalItem : Maybe RepartijaItem
    , filtros : Filtros
    , participantesExpandido : Bool
    , alertaExpandida : Bool
    , detallesAbiertos : Set ULID
    , ayuda : Maybe Ayuda
    , feedback : Feedback
    , accionEnCurso : Maybe AccionEnCurso
    }


{-| Las reglas de qué se puede y qué no viven en el backend
(`BananaSplit.AccionDeRepartija`): acá sólo se muestra lo que contesta.
-}
type alias AccionEnCurso =
    { tipo : TipoDeAccion
    , form : Form CustomFormError AccionDeRepartija
    , preview : WebData ResultadoPreviewDeAccion
    , confirmacion : WebData ResultadoAccionDeRepartija
    }


{-| Las que tocan un item lo traen, tal como estaba al abrir el modal (ver
`conItemActual`).
-}
type TipoDeAccion
    = AgregarUnidadesAlItem RepartijaItem
    | QuitarUnidadesDelItem RepartijaItem
    | SepararUnidadesDelItem RepartijaItem
    | RepartirItemProporcionalmente RepartijaItem
    | UnirItemConOtro RepartijaItem
    | AgregarItemNuevo


{-| La encuesta sobre el rediseño. Recién cuando alguien ya opinó se le ofrece
contar por qué. La respuesta ya mandada no se puede cambiar, pero tampoco se
recuerda: al volver a entrar a la pantalla se pregunta de nuevo.
-}
type Feedback
    = SinResponder
    | Respondido { opinion : Opinion, comentarioAbierto : Bool, comentario : String, comentarioEnviado : Bool }


type alias Filtros =
    { reclamadosPorVos : Bool
    , noResueltos : Bool
    , porParticipante : Maybe ULID
    }


sinFiltros : Filtros
sinFiltros =
    { reclamadosPorVos = False, noResueltos = False, porParticipante = Nothing }


type Ayuda
    = ComoFuncionaLaRepartija
    | ComoDividirUnItem
    | QuePasaConLoNoReclamado


{-| Desde dónde se abrió una ayuda. El mismo texto se ofrece en más de un lugar
y lo que se quiere saber es cuál de esos lugares se usa.
-}
type OrigenAyuda
    = DesdeEncabezado
    | DesdeItem
    | DesdeTotales


init : ULID -> ULID -> Store -> ( Model, Effect Msg )
init grupoId repartijaId store =
    ( { grupoId = grupoId
      , repartijaId = repartijaId
      , pendingItemOperation = Nothing
      , repartirModalItem = Nothing
      , detalleModalItem = Nothing
      , filtros = sinFiltros
      , participantesExpandido = False
      , alertaExpandida = False
      , detallesAbiertos = Set.empty
      , ayuda = Nothing
      , feedback = SinResponder
      , accionEnCurso = Nothing
      }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Store.refreshRepartija repartijaId
        , Effect.getCurrentUser grupoId
        , Effect.setUnsavedChangesWarning False
        ]
    )



-- UPDATE


type Msg
    = NoOp
    | OpenRepartirModal RepartijaItem
    | CloseRepartirModal
    | AbrirDetalleDeItem RepartijaItem
    | CerrarDetalleDeItem
    | ChangeCurrentClaim RepartijaItem Int
    | JoinCurrentClaim RepartijaItem
    | LeaveCurrentClaim RepartijaClaim
    | RepartijaClaimResponded (WebData Api.RepartijaForFrontend)
    | ToggleFiltroReclamadosPorVos
    | ToggleFiltroNoResueltos
    | SetFiltroParticipante (Maybe ULID)
    | LimpiarFiltros
    | ToggleParticipantes
    | ToggleAlerta
    | ToggleDetalleDeItem ULID
    | AbrirAyuda Ayuda OrigenAyuda
    | CerrarAyuda
    | DismissIntro
    | ResponderFeedback Opinion
    | ComentarioFeedbackCambiado String
    | AbrirComentarioFeedback
    | EnviarComentarioFeedback
    | AbrirAccion TipoDeAccion
    | CerrarAccion
    | AccionForm Form.Msg
    | PreviewDeAccionRecibido AccionDeRepartija (WebData ResultadoPreviewDeAccion)
    | AccionRespondida AccionEnCurso (WebData ResultadoAccionDeRepartija)


update : Maybe ULID -> Store -> Msg -> Model -> ( Model, Effect Msg )
update maybeParticipanteId store msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        OpenRepartirModal item ->
            ( { model | repartirModalItem = Just item }
            , Effect.none
            )

        CloseRepartirModal ->
            ( { model | repartirModalItem = Nothing }
            , Effect.none
            )

        AbrirDetalleDeItem item ->
            ( { model | detalleModalItem = Just item }
            , Effect.none
            )

        CerrarDetalleDeItem ->
            ( { model | detalleModalItem = Nothing }
            , Effect.none
            )

        ToggleFiltroReclamadosPorVos ->
            let
                filtros =
                    model.filtros
            in
            ( { model | filtros = { filtros | reclamadosPorVos = not filtros.reclamadosPorVos } }
            , Effect.telemetryEvent
                (Telemetry.repartijaFiltro
                    { filtro = "reclamados_por_vos", activo = not filtros.reclamadosPorVos }
                )
            )

        ToggleFiltroNoResueltos ->
            let
                filtros =
                    model.filtros
            in
            ( { model | filtros = { filtros | noResueltos = not filtros.noResueltos } }
            , Effect.telemetryEvent
                (Telemetry.repartijaFiltro
                    { filtro = "no_resueltos", activo = not filtros.noResueltos }
                )
            )

        SetFiltroParticipante participanteId ->
            let
                filtros =
                    model.filtros
            in
            ( { model | filtros = { filtros | porParticipante = participanteId } }
            , Effect.telemetryEvent
                (Telemetry.repartijaFiltro
                    { filtro = "participante", activo = participanteId /= Nothing }
                )
            )

        LimpiarFiltros ->
            ( { model | filtros = sinFiltros }
            , Effect.none
            )

        ToggleParticipantes ->
            ( { model | participantesExpandido = not model.participantesExpandido }
            , Effect.telemetryEvent
                (Telemetry.repartijaParticipantesExpandidos (not model.participantesExpandido))
            )

        ToggleAlerta ->
            ( { model | alertaExpandida = not model.alertaExpandida }
            , Effect.none
            )

        ToggleDetalleDeItem itemId ->
            let
                abierto =
                    not (Set.member itemId model.detallesAbiertos)
            in
            ( { model
                | detallesAbiertos =
                    if abierto then
                        Set.insert itemId model.detallesAbiertos

                    else
                        Set.remove itemId model.detallesAbiertos
              }
            , Effect.telemetryEvent (Telemetry.repartijaDetalleDeItemAbierto abierto)
            )

        AbrirAyuda ayuda origen ->
            ( { model | ayuda = Just ayuda }
            , Effect.telemetryEvent
                (Telemetry.repartijaAyudaAbierta
                    { ayuda = ayudaToString ayuda, origen = origenAyudaToString origen }
                )
            )

        CerrarAyuda ->
            ( { model | ayuda = Nothing }
            , Effect.none
            )

        DismissIntro ->
            ( model
            , Effect.sendSharedMsg Shared.Msg.DismissRepartijaIntro
            )

        ResponderFeedback opinion ->
            ( { model
                | feedback =
                    Respondido
                        { opinion = opinion
                        , comentarioAbierto = False
                        , comentario = ""
                        , comentarioEnviado = False
                        }
              }
            , Effect.telemetryEvent (Telemetry.repartijaRediseno (Opinion.toString opinion))
            )

        AbrirComentarioFeedback ->
            ( case model.feedback of
                Respondido respuesta ->
                    { model | feedback = Respondido { respuesta | comentarioAbierto = True } }

                SinResponder ->
                    model
            , Effect.none
            )

        ComentarioFeedbackCambiado texto ->
            ( case model.feedback of
                Respondido respuesta ->
                    { model | feedback = Respondido { respuesta | comentario = texto } }

                SinResponder ->
                    model
            , Effect.none
            )

        EnviarComentarioFeedback ->
            case model.feedback of
                Respondido respuesta ->
                    -- El comentario va por el mismo canal que el del menú: no
                    -- hay confirmación de entrega, así que la card lo da por
                    -- mandado y agradece.
                    ( { model | feedback = Respondido { respuesta | comentarioEnviado = True } }
                    , Effect.sendFeedback
                        (Telemetry.repartijaRedisenoComentario (Opinion.toString respuesta.opinion))
                        respuesta.comentario
                    )

                SinResponder ->
                    ( model, Effect.none )

        AbrirAccion tipo ->
            case repartijaCargada store model of
                Just cargada ->
                    let
                        ( accion, effect ) =
                            pedirPreview cargada.repartija
                                { tipo = tipo
                                , form =
                                    Form.initial
                                        [ Form.setString "cantidad" "1"
                                        , Form.setString "nombre" (itemDeAccion tipo |> Maybe.map .nombre |> Maybe.withDefault "")
                                        ]
                                        (validarAccion cargada.pagoMoneda tipo)
                                , preview = NotAsked
                                , confirmacion = NotAsked
                                }
                    in
                    ( { model | accionEnCurso = Just accion }, effect )

                Nothing ->
                    ( model, Effect.none )

        -- Cerrar mientras se confirma no cancela el pedido: cuando conteste,
        -- `AccionRespondida` lo aplica igual y avisa con un toast.
        CerrarAccion ->
            ( { model | accionEnCurso = Nothing }
            , Effect.none
            )

        AccionForm Form.Submit ->
            case ( model.accionEnCurso, Maybe.andThen accionPedida model.accionEnCurso ) of
                ( Just accion, Just pedida ) ->
                    if puedeConfirmar accion then
                        ( { model | accionEnCurso = Just { accion | confirmacion = Loading } }
                        , Effect.sendCmd <|
                            Api.postRepartijasByRepartijaIdAcciones model.repartijaId
                                pedida
                                (RemoteData.fromResult >> AccionRespondida accion)
                        )

                    else
                        ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        AccionForm formMsg ->
            case repartijaCargada store model of
                Just cargada ->
                    editarAccionEnCurso model
                        (\accion ->
                            let
                                conForm =
                                    { accion | form = Form.update (validarAccion cargada.pagoMoneda accion.tipo) formMsg accion.form }
                            in
                            if isDataModifyingEvent formMsg then
                                pedirPreview cargada.repartija conForm

                            else
                                ( conForm, Effect.none )
                        )

                Nothing ->
                    ( model, Effect.none )

        -- Mientras se escribe pueden volver previews de cantidades que ya no
        -- son las del modal: solo vale el de lo que se está pidiendo ahora.
        PreviewDeAccionRecibido pedida resultado ->
            conAccionEnCurso model
                (\accion ->
                    if accionPedida accion == Just pedida then
                        ( { accion | preview = resultado }, Effect.none )

                    else
                        ( accion, Effect.none )
                )

        -- El total del gasto no cambia, pero sí cuánto consumió cada uno, así
        -- que el resumen y los gastos de otras pantallas quedan viejos.
        --
        -- Que el modal esté esperando no garantiza que sea el que mandó esta
        -- respuesta: con el modal cerrado durante el envío, otro abierto y
        -- confirmado antes de que conteste el primero, la respuesta del primero
        -- se tomaría como del segundo. Pide un confirm más lento que un preview
        -- entero más varios clicks; se acepta sin un número de envío.
        AccionRespondida enviada resultado ->
            let
                esDelModalAbierto =
                    Maybe.map estaConfirmando model.accionEnCurso == Just True

                conConfirmacion =
                    if esDelModalAbierto then
                        { model | accionEnCurso = Maybe.map (\accion -> { accion | confirmacion = resultado }) model.accionEnCurso }

                    else
                        model

                telemetria resultadoDeTelemetria =
                    Effect.telemetryEvent
                        (Telemetry.repartijaAccion
                            { accion = tipoDeAccionToString enviada.tipo, resultado = resultadoDeTelemetria }
                        )

                avisarSiNoSeVe contenido =
                    if esDelModalAbierto then
                        Effect.none

                    else
                        Effect.sendToast { level = ToastDanger, content = contenido }
            in
            case resultado of
                Success (AccionDeRepartijaAceptada repartijaResultante) ->
                    ( if esDelModalAbierto then
                        { model | accionEnCurso = Nothing }

                      else
                        model
                    , Effect.batch
                        [ Store.updateRepartijaForFrontend model.repartijaId repartijaResultante
                        , Store.invalidateResumen model.grupoId
                        , Store.invalidatePagos model.grupoId
                        , Effect.sendToast { level = ToastSuccess, content = mensajeDeAccionAplicada enviada }
                        , telemetria "aplicada"
                        ]
                    )

                Success (AccionDeRepartijaRechazada motivo) ->
                    ( conConfirmacion
                    , Effect.batch
                        [ avisarSiNoSeVe (explicacionDeRechazo enviada.tipo motivo)
                        , telemetria (motivoToString motivo)
                        ]
                    )

                Failure _ ->
                    ( conConfirmacion
                    , avisarSiNoSeVe
                        (case itemDeAccion enviada.tipo of
                            Just item ->
                                "No pudimos aplicar el cambio a " ++ item.nombre ++ ". Probá de nuevo."

                            Nothing ->
                                "No pudimos agregar el item. Probá de nuevo."
                        )
                    )

                _ ->
                    ( model, Effect.none )

        -- Crear, cambiar o borrar un claim contesta la repartija como quedó,
        -- con su detalle recalculado: se guarda tal cual, sin tocar nada acá.
        RepartijaClaimResponded webDataRepartija ->
            case webDataRepartija of
                Success repartija ->
                    ( { model | pendingItemOperation = Nothing }
                    , Effect.batch
                        [ Store.updateRepartijaForFrontend model.repartijaId repartija
                        , Store.invalidateResumen model.grupoId
                        , Store.invalidatePagos model.grupoId
                        ]
                    )

                _ ->
                    ( { model | pendingItemOperation = Nothing }
                    , Effect.batch
                        [ Store.refreshRepartija model.repartijaId
                        ]
                    )

        ChangeCurrentClaim item deltaCantidad ->
            case store |> Store.getRepartija model.repartijaId |> RemoteData.toMaybe of
                Just repartijaPage ->
                    case maybeParticipanteId of
                        Just participanteId ->
                            let
                                repartija =
                                    repartijaPage.repartija

                                oldClaimFound =
                                    repartija.claims
                                        |> List.filter
                                            (\claim ->
                                                claim.participante == participanteId && claim.itemId == item.id
                                            )
                                        |> List.head
                            in
                            case oldClaimFound of
                                Just oldClaim ->
                                    let
                                        newCantidad =
                                            oldClaim
                                                |> .cantidad
                                                |> Maybe.withDefault 0
                                                |> (\x -> x + deltaCantidad)
                                                |> Basics.max 0
                                    in
                                    ( { model | pendingItemOperation = Just item.id, repartirModalItem = Nothing }
                                    , if newCantidad > 0 then
                                        Effect.sendCmd <|
                                            Api.putRepartijasByRepartijaIdClaims model.repartijaId
                                                { id = emptyUlid
                                                , participante = participanteId
                                                , itemId = item.id
                                                , cantidad = Just <| newCantidad
                                                }
                                                (RemoteData.fromResult >> RepartijaClaimResponded)

                                      else
                                        Effect.sendCmd <|
                                            Api.deleteRepartijasClaimsByClaimId oldClaim.id
                                                (RemoteData.fromResult >> RepartijaClaimResponded)
                                    )

                                Nothing ->
                                    ( { model | pendingItemOperation = Just item.id, repartirModalItem = Nothing }
                                    , if deltaCantidad > 0 then
                                        Effect.sendCmd <|
                                            Api.putRepartijasByRepartijaIdClaims model.repartijaId
                                                { id = emptyUlid
                                                , participante = participanteId
                                                , itemId = item.id
                                                , cantidad = Just deltaCantidad
                                                }
                                                (RemoteData.fromResult >> RepartijaClaimResponded)

                                      else
                                        Effect.none
                                    )

                        Nothing ->
                            ( model, Effect.none )

                Nothing ->
                    ( model, Effect.none )

        JoinCurrentClaim item ->
            case maybeParticipanteId of
                Just participanteId ->
                    ( { model | pendingItemOperation = Just item.id, repartirModalItem = Nothing }
                    , Effect.sendCmd <|
                        Api.putRepartijasByRepartijaIdClaims model.repartijaId
                            { id = emptyUlid
                            , participante = participanteId
                            , itemId = item.id
                            , cantidad = Nothing
                            }
                            (RemoteData.fromResult >> RepartijaClaimResponded)
                    )

                Nothing ->
                    ( model, Effect.none )

        LeaveCurrentClaim item ->
            case maybeParticipanteId of
                Just _ ->
                    ( { model | pendingItemOperation = Just item.itemId }
                    , Effect.sendCmd <|
                        Api.deleteRepartijasClaimsByClaimId item.id
                            (RemoteData.fromResult >> RepartijaClaimResponded)
                    )

                Nothing ->
                    ( model, Effect.none )


conAccionEnCurso : Model -> (AccionEnCurso -> ( AccionEnCurso, Effect Msg )) -> ( Model, Effect Msg )
conAccionEnCurso model f =
    case model.accionEnCurso of
        Just accion ->
            let
                ( accionNueva, effect ) =
                    f accion
            in
            ( { model | accionEnCurso = Just accionNueva }, effect )

        Nothing ->
            ( model, Effect.none )


editarAccionEnCurso : Model -> (AccionEnCurso -> ( AccionEnCurso, Effect Msg )) -> ( Model, Effect Msg )
editarAccionEnCurso model f =
    conAccionEnCurso model
        (\accion ->
            if estaConfirmando accion then
                ( accion, Effect.none )

            else
                f accion
        )


estaConfirmando : AccionEnCurso -> Bool
estaConfirmando accion =
    RemoteData.isLoading accion.confirmacion


puedeConfirmar : AccionEnCurso -> Bool
puedeConfirmar accion =
    case accion.preview of
        Success (PreviewDeAccionAceptada _) ->
            not (estaConfirmando accion)

        _ ->
            False


accionPedida : AccionEnCurso -> Maybe AccionDeRepartija
accionPedida accion =
    Form.getOutput accion.form


validarAccion : Moneda -> TipoDeAccion -> Validation CustomFormError AccionDeRepartija
validarAccion moneda tipo =
    let
        cantidad =
            V.field "cantidad" V.int

        nombre =
            V.field "nombre" anyString
    in
    case tipo of
        AgregarUnidadesAlItem item ->
            V.map (AgregarUnidades item.id) cantidad

        QuitarUnidadesDelItem item ->
            V.map (QuitarUnidades item.id) cantidad

        SepararUnidadesDelItem item ->
            V.map2 (SepararUnidades item.id) cantidad nombre

        RepartirItemProporcionalmente item ->
            V.succeed (RepartirProporcionalmente item.id)

        UnirItemConOtro item ->
            V.map2 (UnirItems item.id) (V.field "otroItem" V.string) nombre

        AgregarItemNuevo ->
            V.map3 AgregarItem nombre (V.field "monto" (Monto.validateMonto moneda)) cantidad


itemDeAccion : TipoDeAccion -> Maybe RepartijaItem
itemDeAccion tipo =
    case tipo of
        AgregarUnidadesAlItem item ->
            Just item

        QuitarUnidadesDelItem item ->
            Just item

        SepararUnidadesDelItem item ->
            Just item

        RepartirItemProporcionalmente item ->
            Just item

        UnirItemConOtro item ->
            Just item

        AgregarItemNuevo ->
            Nothing


{-| La acción con el item como está ahora en la repartija, por si cambió
mientras el modal estaba abierto.
-}
conItemActual : Repartija -> TipoDeAccion -> TipoDeAccion
conItemActual repartija tipo =
    let
        actual item =
            repartija.items
                |> find (\i -> i.id == item.id)
                |> Maybe.withDefault item
    in
    case tipo of
        AgregarUnidadesAlItem item ->
            AgregarUnidadesAlItem (actual item)

        QuitarUnidadesDelItem item ->
            QuitarUnidadesDelItem (actual item)

        SepararUnidadesDelItem item ->
            SepararUnidadesDelItem (actual item)

        RepartirItemProporcionalmente item ->
            RepartirItemProporcionalmente (actual item)

        UnirItemConOtro item ->
            UnirItemConOtro (actual item)

        AgregarItemNuevo ->
            AgregarItemNuevo


repartijaCargada : Store -> Model -> Maybe RepartijaForFrontend
repartijaCargada store model =
    store
        |> Store.getRepartija model.repartijaId
        |> RemoteData.toMaybe


pedirPreview : Repartija -> AccionEnCurso -> ( AccionEnCurso, Effect Msg )
pedirPreview repartija accion =
    case accionPedida accion of
        Just pedida ->
            ( { accion | preview = Loading, confirmacion = NotAsked }
            , Effect.sendCmd <|
                Api.postRepartijasAccionesPreview { repartija = repartija, accion = pedida }
                    (RemoteData.fromResult >> PreviewDeAccionRecibido pedida)
            )

        Nothing ->
            ( { accion | preview = NotAsked, confirmacion = NotAsked }, Effect.none )


tipoDeAccionToString : TipoDeAccion -> String
tipoDeAccionToString tipo =
    case tipo of
        AgregarUnidadesAlItem _ ->
            "agregar_unidades"

        QuitarUnidadesDelItem _ ->
            "quitar_unidades"

        SepararUnidadesDelItem _ ->
            "separar_unidades"

        RepartirItemProporcionalmente _ ->
            "repartir_proporcionalmente"

        UnirItemConOtro _ ->
            "unir_items"

        AgregarItemNuevo ->
            "agregar_item"


motivoToString : AccionDeRepartijaInvalida -> String
motivoToString motivo =
    case motivo of
        ItemNoEncontrado ->
            "item_no_encontrado"

        ItemSinUnidades ->
            "item_sin_unidades"

        CantidadDeUnidadesInvalida ->
            "cantidad_invalida"

        NoPuedeQuedarSinItems ->
            "ultimo_item"

        UnirConElMismoItem ->
            "mismo_item"

        ItemSinNombre ->
            "item_sin_nombre"


mensajeDeAccionAplicada : AccionEnCurso -> String
mensajeDeAccionAplicada accion =
    case accion.tipo of
        AgregarUnidadesAlItem item ->
            "Agregamos unidades a " ++ item.nombre

        QuitarUnidadesDelItem item ->
            "Quitamos unidades de " ++ item.nombre

        SepararUnidadesDelItem item ->
            "Separamos unidades de " ++ item.nombre ++ " en un item nuevo"

        RepartirItemProporcionalmente item ->
            "Repartimos " ++ item.nombre ++ " entre todos"

        UnirItemConOtro item ->
            "Unimos los items en " ++ item.nombre

        AgregarItemNuevo ->
            case accionPedida accion of
                Just (AgregarItem nombre _ _) ->
                    "Agregamos " ++ String.trim nombre

                _ ->
                    "Agregamos el item"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Maybe ULID -> { introDismissed : Bool } -> Store -> Model -> View Msg
view userId { introDismissed } store model =
    case ( Store.getRepartija model.repartijaId store, Store.getGrupo model.grupoId store ) of
        ( Success repartijaPage, Success grupo ) ->
            let
                repartija =
                    repartijaPage.repartija

                -- Los montos de cada uno vienen calculados del backend; acá
                -- solo se eligen los del participante que estás mirando.
                detalle =
                    repartijaPage.detalle
            in
            { title = grupo.nombre ++ ": " ++ repartija.nombre
            , body =
                [ div [ class "container-fluid py-3" ]
                    [ div [ class "row g-4" ]
                        [ div [ class "col-12 col-lg-8" ]
                            [ if introDismissed then
                                text ""

                              else
                                div [ Attr.id "repartija-intro" ] [ viewIntro ]
                            , div [ Attr.id "repartija-alertas" ]
                                [ viewAlertaNoResueltos model detalle repartija
                                , viewLinksDeAyuda
                                ]
                            , div [ Attr.id "repartija-participantes-mobile", class "d-lg-none mb-3" ] [ viewParticipantes model grupo repartija ]
                            , div [ Attr.id "repartija-items-header" ] [ viewItemsHeader userId grupo model.filtros ]
                            , div [ Attr.id "repartija-items" ] [ viewItems model userId grupo detalle repartija ]
                            , button
                                [ type_ "button"
                                , class "btn btn-outline-secondary d-inline-flex align-items-center gap-2 mb-3"
                                , Telemetry.click "repartija.add_item"
                                , onClick (AbrirAccion AgregarItemNuevo)
                                ]
                                [ i [ class "bi bi-plus-lg" ] [], text "Agregar item" ]
                            ]
                        , div [ Attr.id "repartija-lateral", class "col-12 col-lg-4" ]
                            [ div [ Attr.id "repartija-participantes", class "d-none d-lg-block mb-3" ] [ viewParticipantes model grupo repartija ]
                            , viewTotales userId detalle repartija
                            , viewFeedback model.feedback
                            ]
                        ]
                    , viewRepartirModal model.repartirModalItem
                    , viewDetalleModal grupo detalle repartija model.detalleModalItem
                    , viewAyudaModal model.ayuda
                    , viewAccionModal grupo repartijaPage.pagoMoneda repartija model.accionEnCurso
                    ]
                ]
            }

        ( Failure e1, Failure e2 ) ->
            { title = ""
            , body =
                [ div [ class "container-fluid py-3" ]
                    [ text "falle"
                    , viewHttpError e1
                    , viewHttpError e2
                    ]
                ]
            }

        ( Failure e, _ ) ->
            { title = ""
            , body =
                [ div [ class "container-fluid py-3" ]
                    [ text "falle"
                    , viewHttpError e
                    ]
                ]
            }

        ( _, Failure e ) ->
            { title = ""
            , body =
                [ div [ class "container-fluid py-3" ]
                    [ text "falle"
                    , viewHttpError e
                    ]
                ]
            }

        _ ->
            { title = ""
            , body = [ div [ class "container-fluid py-3" ] [ text "cargando" ] ]
            }


linkDeAyuda : Ayuda -> OrigenAyuda -> String -> Html Msg
linkDeAyuda ayuda origen label =
    button
        [ type_ "button"
        , class "btn btn-link p-0 text-decoration-underline d-inline-flex align-items-center gap-1"
        , Telemetry.click "repartija.help_open"
        , onClick (AbrirAyuda ayuda origen)
        ]
        [ i [ class "bi bi-info-circle" ] []
        , text label
        ]


viewLinksDeAyuda : Html Msg
viewLinksDeAyuda =
    div [ class "d-flex flex-wrap column-gap-3 mb-3" ]
        [ linkDeAyuda ComoFuncionaLaRepartija DesdeEncabezado "¿Cómo funciona la Repartija?"
        , linkDeAyuda ComoDividirUnItem DesdeEncabezado "¿Cómo dividir un item?"
        ]


viewIntro : Html Msg
viewIntro =
    div [ class "alert alert-primary d-flex align-items-start gap-2 mb-3", Attr.attribute "role" "alert" ]
        [ div [ class "flex-grow-1" ]
            [ h6 [ class "alert-heading fw-semibold mb-1" ]
                [ text "¿Es tu primera vez participando en una repartija?" ]
            , p [ class "small mb-0" ]
                [ text "La repartija es una forma práctica y colaborativa de repartir gastos grupales. A continuación encontrarás el listado de "
                , strong [] [ text "items" ]
                , text ", deberás ubicar aquellos en los que hayas "
                , strong [] [ text "participado" ]
                , text " y "
                , strong [] [ text "reclamarlos" ]
                , text "."
                ]
            ]
        , button
            [ type_ "button"
            , class "btn-close"
            , Attr.attribute "aria-label" "Cerrar"
            , Telemetry.click "repartija.intro_dismiss"
            , onClick DismissIntro
            ]
            []
        ]


viewAlertaNoResueltos : Model -> DetalleRepartija -> Repartija -> Html Msg
viewAlertaNoResueltos model detalle repartija =
    let
        noResueltos =
            Repartija.itemsNoResueltos detalle
    in
    if noResueltos == 0 then
        text ""

    else
        div [ class "alert alert-warning p-0 mb-3" ]
            [ button
                [ type_ "button"

                -- `text-reset`: el `.btn` trae su propio color (blanco en modo
                -- oscuro) y tiene que heredar el del alert.
                , class "btn text-reset w-100 d-flex align-items-center gap-2 text-start border-0 py-2"
                , Attr.attribute "aria-expanded"
                    (if model.alertaExpandida then
                        "true"

                     else
                        "false"
                    )
                , Telemetry.click "repartija.toggle_alert"
                , onClick ToggleAlerta
                ]
                [ Bs.badge "text-bg-danger rounded-pill text-nowrap"
                    []
                    [ text (String.fromInt noResueltos ++ " de " ++ String.fromInt (List.length repartija.items)) ]
                , span [ class "flex-grow-1" ] [ text "Items aún no resueltos" ]
                , i
                    [ class
                        (if model.alertaExpandida then
                            "bi bi-chevron-up"

                         else
                            "bi bi-chevron-down"
                        )
                    ]
                    []
                ]
            , if model.alertaExpandida then
                div [ class "px-3 pb-3 d-flex flex-wrap align-items-center gap-3" ]
                    [ p [ class "flex-grow-1 mb-0", Attr.style "min-width" "14rem" ]
                        [ text "Para que esta repartija sea válida y pueda ser tomada en cuenta en las divisiones es necesario resolver todos los items con la etiqueta en rojo. Los que están en amarillo tienen algo raro, pero se pueden repartir así como están. Pueden conocer el problema pasando por encima de las etiquetas en los items correspondientes." ]
                    , div [ class "d-flex align-items-center gap-2 mx-auto" ]
                        [ Bs.badge "text-bg-danger rounded-pill text-nowrap d-inline-flex align-items-center gap-1"
                            []
                            [ i [ class "bi bi-x-circle-fill" ] [], text "Sin reclamar" ]
                        , i [ class "bi bi-arrow-right" ] []
                        , Bs.badge "text-bg-success rounded-pill text-nowrap d-inline-flex align-items-center gap-1"
                            []
                            [ i [ class "bi bi-check-circle-fill" ] [], text "Repartido" ]
                        ]
                    ]

              else
                text ""
            ]


viewParticipantes : Model -> GrupoLike g -> Repartija -> Html Msg
viewParticipantes model grupo repartija =
    let
        ( conClaims, sinClaims ) =
            grupo.participantes
                |> List.partition (\p -> List.any (\c -> c.participante == p.id) repartija.claims)

        cuenta colorClass titulo participantes =
            Bs.badge (colorClass ++ " rounded-pill d-inline-flex align-items-center gap-1")
                [ Attr.title titulo ]
                [ i [ class "bi bi-people-fill" ] []
                , text (String.fromInt (List.length participantes))
                ]

        grupoDeNombres colorClass titulo participantes =
            div [ class "d-flex align-items-center flex-wrap gap-2" ]
                [ cuenta colorClass titulo participantes
                , span []
                    [ text (participantes |> List.map .nombre |> String.join ", ") ]
                ]

        -- Expandida, cada uno de los que reclamaron es un botón que abre, en
        -- un dropdown, qué se llevó esa persona. Bootstrap lo abre, lo cierra
        -- y le pinta el botón mientras está abierto; Popper lo reubica si no
        -- entra en la pantalla.
        chipDeParticipante p =
            div [ class "dropdown" ]
                [ button
                    [ type_ "button"
                    , class "btn btn-sm btn-outline-primary rounded-pill px-3"
                    , Attr.attribute "data-bs-toggle" "dropdown"
                    , Attr.attribute "data-bs-offset" "0,8"
                    , Attr.attribute "aria-expanded" "false"
                    , Attr.title ("Ver los items que reclamó " ++ p.nombre)
                    , Telemetry.click "repartija.items_de_participante"
                    ]
                    [ text p.nombre ]
                , itemsReclamadosPor p
                ]

        itemsReclamadosPor p =
            let
                filas =
                    repartija.claims
                        |> List.filter (\c -> c.participante == p.id)
                        |> List.filterMap
                            (\c ->
                                repartija.items
                                    |> find (\item -> item.id == c.itemId)
                                    |> Maybe.map
                                        (\item ->
                                            Html.tr []
                                                [ Html.td [ class "ps-3 py-1" ] [ text item.nombre ]
                                                , Html.td [ class "text-end text-nowrap pe-3 py-1" ]
                                                    [ text
                                                        (case c.cantidad of
                                                            Just n ->
                                                                String.fromInt n

                                                            Nothing ->
                                                                "Parte igual"
                                                        )
                                                    ]
                                                ]
                                        )
                            )
            in
            div [ class "dropdown-menu shadow p-0 overflow-hidden", Attr.style "width" "16rem" ]
                [ div [ class "fw-semibold bg-body-tertiary border-bottom px-3 py-2" ]
                    [ text ("Items reclamados por " ++ p.nombre) ]
                , Html.table [ class "table table-sm table-borderless small mb-1" ]
                    [ Html.thead []
                        [ Html.tr [ class "border-bottom" ]
                            [ Html.th [ class "fw-normal text-body-secondary ps-3 pt-2" ] [ text "Producto" ]
                            , Html.th [ class "fw-normal text-body-secondary text-end pe-3 pt-2" ] [ text "Unidades" ]
                            ]
                        ]
                    , Html.tbody [] filas
                    ]
                ]
    in
    Bs.card []
        [ Bs.cardBody [ class "py-2" ]
            [ button
                [ type_ "button"
                , class "btn btn-link w-100 p-0 text-reset text-decoration-none d-flex align-items-center gap-2"
                , Attr.attribute "aria-expanded"
                    (if model.participantesExpandido then
                        "true"

                     else
                        "false"
                    )
                , Telemetry.click "repartija.toggle_participantes"
                , onClick ToggleParticipantes
                ]
                [ h5 [ class "mb-0 flex-grow-1 text-start" ] [ text "Quiénes ya reclamaron" ]
                , i
                    [ class
                        (if model.participantesExpandido then
                            "bi bi-chevron-up"

                         else
                            "bi bi-chevron-down"
                        )
                    ]
                    []
                ]
            , if model.participantesExpandido then
                div []
                    [ div [ class "mt-2 d-flex align-items-center flex-wrap gap-2" ]
                        (cuenta "text-bg-primary" "Ya reclamaron items" conClaims
                            :: List.map chipDeParticipante conClaims
                        )
                    , if List.isEmpty sinClaims then
                        text ""

                      else
                        div [ class "mt-3" ]
                            [ h6 [ class "fw-semibold" ] [ text "No reclamaron items" ]
                            , grupoDeNombres "text-bg-secondary" "Aún no reclamaron items" sinClaims
                            ]
                    ]

              else
                div [ class "mt-2 text-primary" ]
                    [ grupoDeNombres "text-bg-primary" "Ya reclamaron items" conClaims ]
            ]
        ]


{-| El título de la lista con el botón de los filtros, que se abren en un
dropdown. Cerrado, los filtros siguen aplicándose, así que el botón se destaca y
cuenta cuántos hay puestos: si no, se esconderían items sin nada en pantalla que
lo explique.

El dropdown no se cierra al tocar lo de adentro (`data-bs-auto-close`), así se
pueden poner varios filtros seguidos.

-}
viewItemsHeader : Maybe ULID -> GrupoLike g -> Filtros -> Html Msg
viewItemsHeader userId grupo filtros =
    let
        activos =
            [ filtros.reclamadosPorVos, filtros.noResueltos, filtros.porParticipante /= Nothing ]
                |> List.filter identity
                |> List.length

        switch id label activo msg =
            div [ class "form-check form-switch mb-0" ]
                [ input
                    [ class "form-check-input"
                    , type_ "checkbox"
                    , Attr.id id
                    , Attr.checked activo
                    , onCheck (always msg)
                    ]
                    []
                , Html.label [ class "form-check-label", Attr.for id ] [ text label ]
                ]
    in
    div [ class "d-flex align-items-center justify-content-between gap-2 mt-4 mb-3" ]
        [ h4 [ class "mb-0" ] [ text "Items a repartir" ]
        , div [ class "dropdown" ]
            [ button
                [ type_ "button"
                , class "btn btn-sm d-inline-flex align-items-center gap-2"
                , classList
                    [ ( "btn-primary", activos > 0 )
                    , ( "btn-outline-secondary", activos == 0 )
                    ]
                , Attr.attribute "data-bs-toggle" "dropdown"
                , Attr.attribute "data-bs-auto-close" "outside"
                , Attr.attribute "aria-expanded" "false"
                , Attr.attribute "aria-label" "Filtros"
                , Attr.title "Filtrar items"
                , Telemetry.click "repartija.open_filtros"
                ]
                [ i [ class "bi bi-funnel" ] []
                , span [] [ text "Filtros" ]
                , if activos > 0 then
                    Bs.badge "text-bg-light rounded-pill" [ Attr.title "Filtros puestos" ] [ text (String.fromInt activos) ]

                  else
                    text ""
                ]
            , div [ class "dropdown-menu dropdown-menu-end shadow p-3", Attr.style "width" "18rem" ]
                [ div [ class "d-flex align-items-center mb-3" ]
                    [ h6 [ class "fw-semibold mb-0 flex-grow-1" ] [ text "Filtros" ]
                    , if activos > 0 then
                        button
                            [ type_ "button"
                            , class "btn btn-link btn-sm p-0"
                            , Telemetry.click "repartija.clear_filtros"
                            , onClick LimpiarFiltros
                            ]
                            [ text "Limpiar" ]

                      else
                        text ""
                    ]
                , div [ class "d-flex flex-column gap-3" ]
                    [ case userId of
                        Just _ ->
                            switch "filtro-reclamados" "Reclamados por vos" filtros.reclamadosPorVos ToggleFiltroReclamadosPorVos

                        Nothing ->
                            text ""
                    , switch "filtro-no-resueltos" "No resueltos" filtros.noResueltos ToggleFiltroNoResueltos
                    , div [ class "input-group" ]
                        [ span [ class "input-group-text bg-body" ] [ i [ class "bi bi-search" ] [] ]
                        , Html.select
                            [ class "form-select"
                            , Attr.attribute "aria-label" "Filtrar por participante"
                            , onInput
                                (\elegido ->
                                    SetFiltroParticipante
                                        (if String.isEmpty elegido then
                                            Nothing

                                         else
                                            Just elegido
                                        )
                                )
                            ]
                            (Html.option
                                [ value "", Attr.selected (filtros.porParticipante == Nothing) ]
                                [ text "Participante" ]
                                :: (grupo.participantes
                                        |> List.map
                                            (\p ->
                                                Html.option
                                                    [ value p.id
                                                    , Attr.selected (filtros.porParticipante == Just p.id)
                                                    ]
                                                    [ text p.nombre ]
                                            )
                                   )
                            )
                        ]
                    ]
                ]
            ]
        ]


cumpleFiltros : Maybe ULID -> Filtros -> Repartija -> DetalleDeItem -> Bool
cumpleFiltros userId filtros repartija deItem =
    let
        reclamadoPor participanteId =
            Repartija.claimsDeItem repartija deItem.item
                |> List.any (\c -> c.participante == participanteId)

        cumple filtroActivo participanteId =
            not filtroActivo
                || (participanteId |> Maybe.map reclamadoPor |> Maybe.withDefault False)
    in
    cumple filtros.reclamadosPorVos userId
        && (not filtros.noResueltos || deItem.resolucion == ItemNoResuelto)
        && cumple (filtros.porParticipante /= Nothing) filtros.porParticipante


viewItems : Model -> Maybe ULID -> GrupoLike g -> DetalleRepartija -> Repartija -> Html Msg
viewItems model userId grupo detalle repartija =
    case List.filter (cumpleFiltros userId model.filtros repartija) detalle.porItem of
        [] ->
            p [ class "text-muted text-center py-4" ]
                [ if List.isEmpty repartija.items then
                    text "Esta repartija no tiene items."

                  else
                    text "Ningún item coincide con los filtros."
                ]

        items ->
            div []
                (items
                    |> List.map (viewItemCard model userId grupo detalle repartija)
                )


unidades : Int -> String
unidades n =
    if n == 1 then
        "1 unidad"

    else
        String.fromInt n ++ " unidades"


viewItemCard : Model -> Maybe ULID -> GrupoLike g -> DetalleRepartija -> Repartija -> DetalleDeItem -> Html Msg
viewItemCard model userId grupo detalle repartija deItem =
    let
        item =
            deItem.item

        claims =
            Repartija.claimsDeItem repartija item

        estado =
            deItem.estado

        userClaim =
            claims |> find (\c -> Just c.participante == userId)

        pending =
            model.pendingItemOperation == Just item.id || userId == Nothing

        boton track variant contenido msg habilitado =
            button
                [ type_ "button"
                , class ("btn " ++ variant)
                , disabled (pending || not habilitado)
                , Telemetry.click track
                , onClick msg
                ]
                contenido

        botonDeCantidad icono delta habilitado =
            boton
                (if delta > 0 then
                    "repartija.item_increase"

                 else
                    "repartija.item_decrease"
                )
                (if delta > 0 then
                    "btn-primary"

                 else
                    "btn-secondary"
                )
                [ i [ class ("bi me-1 " ++ icono) ] []
                , text (String.fromInt (Basics.abs delta))
                ]
                (ChangeCurrentClaim item delta)
                habilitado

        tieneCantidad =
            (userClaim |> Maybe.andThen .cantidad) /= Nothing

        participaEquitativo =
            case userClaim of
                Just claim ->
                    claim.cantidad == Nothing

                Nothing ->
                    False

        salirse =
            case userClaim of
                Just claim ->
                    LeaveCurrentClaim claim

                Nothing ->
                    NoOp

        botonesDeCantidad =
            div [ class "d-flex flex-nowrap gap-2" ]
                [ botonDeCantidad "bi-plus-circle" 1 True
                , botonDeCantidad "bi-dash-circle" -1 tieneCantidad
                ]

        -- Los botones salen del reparto que ya tiene el item: solo se elige
        -- cómo repartirlo cuando todavía no lo tocó nadie.
        botones =
            case estado of
                EstadoSinReclamar ->
                    [ boton "repartija.item_split" "btn-primary" [ text "Repartir" ] (OpenRepartirModal item) True ]

                EstadoRepartidoPorCantidad _ ->
                    [ botonesDeCantidad ]

                EstadoRepartidoEquitativamente _ ->
                    if participaEquitativo then
                        [ boton "repartija.item_leave" "btn-secondary" [ text "Salirse" ] salirse True ]

                    else
                        [ boton "repartija.item_join" "btn-primary" [ text "Participar" ] (JoinCurrentClaim item) True ]

                EstadoRepartoIncorrecto ->
                    [ botonesDeCantidad
                    , if participaEquitativo then
                        boton "repartija.item_leave" "btn-secondary" [ text "Salirse" ] salirse True

                      else
                        boton "repartija.item_join" "btn-primary" [ text "Participar" ] (JoinCurrentClaim item) True
                    ]

        detalleAbierto =
            Set.member item.id model.detallesAbiertos

        columna titulo contenido =
            div [ class "col-12 col-md-4" ]
                (div [ class "fw-semibold mb-1" ] [ text titulo ] :: contenido)

        apagado texto =
            div [ class "text-body-secondary" ] [ text texto ]

        linkConAyuda track msg label =
            button
                [ type_ "button"
                , class "btn btn-link p-0 text-start d-flex align-items-start gap-1"
                , Telemetry.click track
                , onClick msg
                ]
                [ i [ class "bi bi-question-circle-fill mt-1" ] [], span [] [ text label ] ]
    in
    div
        (class "card mb-3"
            :: (case userClaim of
                    -- Un tinte azul apenas visible marca los items que reclamaste.
                    Just _ ->
                        [ Attr.style "background-color" "color-mix(in srgb, var(--bs-primary) 6%, var(--bs-body-bg))" ]

                    Nothing ->
                        []
               )
        )
        [ div [ class "card-body d-flex flex-column flex-md-row align-items-md-center gap-3" ]
            [ div [ class "flex-grow-1" ]
                [ h5 [ class "card-title mb-1" ] [ text item.nombre ]
                , div [ class "mb-1" ] [ badgeDeItem deItem ]
                , div [ class "small" ]
                    ([ strong [] [ text "Cantidad: " ]
                     , text (String.fromInt item.cantidad)
                     ]
                        ++ viewPrecioUnitario item
                        ++ [ span [ class "mx-2 text-body-secondary" ] [ text "•" ]
                           , strong [] [ text "Total: " ]
                           , text ("$ " ++ Monto.toString item.monto)
                           ]
                    )
                ]
            , div [ class "d-flex flex-column align-items-center align-items-md-end gap-2 flex-shrink-0" ] botones
            ]
        , div
            [ class "card-footer d-flex align-items-center gap-2 small"
            , classList [ ( "border-bottom", detalleAbierto ) ]
            ]
            [ case userClaim of
                Just claim ->
                    span [ class "flex-grow-1 fw-semibold d-inline-flex align-items-center gap-1" ]
                        [ i [ class "bi bi-person-check-fill" ] []
                        , text
                            (case claim.cantidad of
                                Just n ->
                                    "Reclamaste " ++ String.fromInt n ++ " de " ++ String.fromInt item.cantidad

                                Nothing ->
                                    -- Solo cuentan los que participan en partes
                                    -- iguales: si el item además tiene reclamos
                                    -- por cantidad, esos no dividen con vos.
                                    case
                                        claims
                                            |> List.filter (\c -> c.cantidad == Nothing && c.id /= claim.id)
                                            |> List.length
                                    of
                                        0 ->
                                            "Participando sólo"

                                        otros ->
                                            "Participando con " ++ String.fromInt otros ++ " más"
                            )
                        ]

                Nothing ->
                    span [ class "flex-grow-1" ] [ text "No reclamaste unidades" ]
            , button
                [ type_ "button"
                , class "btn btn-link btn-sm p-0 text-reset text-decoration-none d-inline-flex align-items-center gap-1"
                , Attr.attribute "aria-expanded"
                    (if detalleAbierto then
                        "true"

                     else
                        "false"
                    )
                , Telemetry.click "repartija.toggle_item_detail"
                , onClick (ToggleDetalleDeItem item.id)
                ]
                [ text "Detalle"
                , i
                    [ class
                        (if detalleAbierto then
                            "bi bi-chevron-up"

                         else
                            "bi bi-chevron-down"
                        )
                    ]
                    []
                ]
            ]
        , if detalleAbierto then
            let
                -- Cómo se reparte el item y cuánto va repartido, cada uno con su link
                -- a la explicación.
                modoDeReparto =
                    columna "Modo de reparto"
                        (case estado of
                            EstadoSinReclamar ->
                                [ apagado "Aún no definido" ]

                            _ ->
                                [ linkConAyuda "repartija.item_split_mode_help" (AbrirAyuda ComoDividirUnItem DesdeItem) (nombreDelReparto estado)
                                , linkConAyuda "repartija.item_detail" (AbrirDetalleDeItem item) (progresoDelReparto item estado)
                                ]
                        )

                tuParte =
                    columna "Tu parte"
                        (case userClaim of
                            Nothing ->
                                [ apagado "No reclamaste items" ]

                            Just claim ->
                                [ div [ Attr.title (explicacionDeConsumo estado item) ]
                                    [ case claim.cantidad of
                                        Just n ->
                                            div []
                                                [ text "Reclamaste "
                                                , span [ class "fw-semibold" ] [ text (unidades n) ]
                                                ]

                                        Nothing ->
                                            div [ class "fw-semibold" ] [ text "Estás participando" ]
                                    , div [ class "text-danger" ]
                                        [ text ("$ " ++ Monto.toString (Repartija.consumoDeItem detalle item.id userId)) ]
                                    ]
                                ]
                        )

                participantes =
                    columna "Participantes"
                        [ if List.isEmpty claims then
                            apagado "Nadie lo reclamó"

                          else
                            let
                                -- Las unidades de cada uno solo dicen algo si
                                -- el item se reparte por cantidad.
                                porCantidad =
                                    case estado of
                                        EstadoRepartidoPorCantidad _ ->
                                            True

                                        _ ->
                                            False

                                nombreDe claim =
                                    case ( porCantidad, claim.cantidad ) of
                                        ( True, Just n ) ->
                                            lookupNombreParticipante grupo claim.participante ++ " (" ++ String.fromInt n ++ ")"

                                        _ ->
                                            lookupNombreParticipante grupo claim.participante
                            in
                            div []
                                [ text
                                    (claims
                                        |> List.map nombreDe
                                        |> String.join ", "
                                    )
                                ]
                        ]

                botonDeAccion track icono label tipo habilitado =
                    button
                        [ type_ "button"
                        , class "btn btn-sm btn-outline-secondary d-inline-flex align-items-center gap-1"
                        , disabled (not habilitado)
                        , Telemetry.click track
                        , onClick (AbrirAccion (tipo item))
                        ]
                        [ i [ class ("bi " ++ icono) ] [], text label ]

                acciones =
                    div [ class "d-flex flex-wrap align-items-center gap-2 mt-3 pt-3 border-top" ]
                        [ span [ class "fw-semibold me-2" ] [ text "Acciones" ]
                        , botonDeAccion "repartija.item_action_add" "bi-plus-lg" "Agregar unidades" AgregarUnidadesAlItem True
                        , botonDeAccion "repartija.item_action_remove" "bi-dash-lg" "Quitar unidades" QuitarUnidadesDelItem (item.cantidad > 1)
                        , botonDeAccion "repartija.item_action_split" "bi-scissors" "Separar en un item nuevo" SepararUnidadesDelItem (item.cantidad > 1)
                        , botonDeAccion "repartija.item_action_merge" "bi-union" "Unir con otro item" UnirItemConOtro hayOtrosItems
                        , botonDeAccion "repartija.item_action_proportional" "bi-pie-chart" "Repartir proporcionalmente" RepartirItemProporcionalmente hayOtrosItems
                        ]

                -- Unir necesita otro item, y la repartija no puede quedar sin
                -- ninguno.
                hayOtrosItems =
                    List.length repartija.items > 1
            in
            div [ class "bg-body-tertiary p-3 rounded-bottom" ]
                [ div [ class "row g-3" ] [ modoDeReparto, tuParte, participantes ]
                , acciones
                ]

          else
            text ""
        ]


viewPrecioUnitario : RepartijaItem -> List (Html msg)
viewPrecioUnitario item =
    case precioUnitarioTexto item of
        Just precio ->
            [ span [ class "mx-2 text-body-secondary" ] [ text "•" ]
            , text precio
            ]

        Nothing ->
            []


{-| Si la división no es exacta se marca con `≈`, para que nadie multiplique y
crea que la cuenta no cierra.
-}
precioUnitarioTexto : RepartijaItem -> Maybe String
precioUnitarioTexto item =
    if item.cantidad > 1 then
        Monto.porUnidad item.cantidad item.monto
            |> Maybe.map
                (\precio ->
                    (if precio.exacto then
                        "$ "

                     else
                        "≈ $ "
                    )
                        ++ Monto.toString precio.monto
                        ++ " c/u"
                )

    else
        Nothing


nombreDelReparto : EstadoDeItem -> String
nombreDelReparto estado =
    case estado of
        EstadoSinReclamar ->
            "Aún no definido"

        EstadoRepartidoPorCantidad _ ->
            "Por cantidad"

        EstadoRepartidoEquitativamente _ ->
            "Equitativo"

        EstadoRepartoIncorrecto ->
            "Por cantidad y equitativo a la vez"


progresoDelReparto : RepartijaItem -> EstadoDeItem -> String
progresoDelReparto item estado =
    let
        repartidos n =
            "Repartidos " ++ String.fromInt n ++ " de " ++ String.fromInt item.cantidad
    in
    case estado of
        EstadoRepartidoPorCantidad UnidadesExactas ->
            repartidos item.cantidad

        EstadoRepartidoPorCantidad (FaltanUnidades n) ->
            repartidos (item.cantidad - n)

        EstadoRepartidoPorCantidad (SobranUnidades n) ->
            repartidos (item.cantidad + n)

        EstadoRepartidoEquitativamente personas ->
            "Dividido entre " ++ String.fromInt personas

        EstadoRepartoIncorrecto ->
            "Ver quién reclamó qué"

        EstadoSinReclamar ->
            "Todavía no lo reclamó nadie"


explicacionDeConsumo : EstadoDeItem -> RepartijaItem -> String
explicacionDeConsumo estado item =
    case estado of
        EstadoRepartidoEquitativamente personas ->
            "El item se divide en partes iguales entre las "
                ++ String.fromInt personas
                ++ " personas que están participando."

        _ ->
            "Tu consumo sale de las unidades que reclamaste sobre las "
                ++ String.fromInt item.cantidad
                ++ " del item. Las unidades que no reclamó nadie no te cuentan acá."


{-| La etiqueta que acompaña al nombre del item. El color dice si hay algo que
hacer: verde si está repartido, amarillo si hay algo raro pero se puede repartir
así, rojo si alguien tiene que hacer algo para que la repartija sea válida. Qué
pasa con el item y de qué color va lo decide el backend
(`BananaSplit.Deudas.detalleRepartija`); acá solo se elige cómo contarlo. El
texto dice qué es lo raro o lo que falta, y el `title` lo explica. Que lo hayas
reclamado o no ya lo dice el cuerpo de la card, así que acá no se repite.
-}
badgeDeItem : DetalleDeItem -> Html Msg
badgeDeItem deItem =
    let
        -- En pantallas chicas la etiqueta se queda con lo imprescindible: el
        -- resto la haría entrar en dos renglones o empujar al resto afuera.
        conDetalle corto largo =
            [ text corto, span [ class "d-none d-sm-inline" ] [ text largo ] ]

        explicacionDeLoNoReclamado =
            case deItem.resolucion of
                ItemNoResuelto ->
                    "Lo que no se reclama no se le cobra a nadie: hay que reclamarlo para que la repartija sea válida."

                _ ->
                    "Lo que no se reclama se reparte en proporción entre los que consumieron."

        ( contenido, explicacion ) =
            case deItem.estado of
                EstadoRepartidoPorCantidad UnidadesExactas ->
                    ( [ text "Repartido" ], "Se reclamaron exactamente las unidades que hay." )

                EstadoRepartidoPorCantidad (FaltanUnidades n) ->
                    ( conDetalle ("Faltan " ++ String.fromInt n) " por repartir"
                    , explicacionDeLoNoReclamado
                    )

                EstadoRepartidoPorCantidad (SobranUnidades n) ->
                    ( conDetalle ("Sobran " ++ String.fromInt n)
                        (if n == 1 then
                            " unidad"

                         else
                            " unidades"
                        )
                    , "Se reclamaron más unidades de las que hay: el total del item se reparte en proporción a lo que reclamó cada uno."
                    )

                EstadoRepartidoEquitativamente 1 ->
                    ( conDetalle "Solo 1 persona" " está participando"
                    , "El item lo paga entero la única persona que está participando."
                    )

                EstadoRepartidoEquitativamente n ->
                    ( [ text "Repartido" ], "El item se divide en partes iguales entre " ++ String.fromInt n ++ " personas." )

                EstadoRepartoIncorrecto ->
                    ( conDetalle "Error" " de reparto"
                    , "Hay reclamos por cantidad y en partes iguales a la vez, y así el item no se le cobra a nadie: tienen que repartirlo de una sola forma."
                    )

                EstadoSinReclamar ->
                    ( [ text "Sin reclamar" ], explicacionDeLoNoReclamado )

        ( colorClass, icono ) =
            case deItem.resolucion of
                ItemResuelto ->
                    ( "text-bg-success", "bi-check-circle-fill" )

                ItemConAdvertencia ->
                    ( "text-bg-warning", "bi-exclamation-triangle-fill" )

                ItemNoResuelto ->
                    ( "text-bg-danger", "bi-x-circle-fill" )
    in
    Bs.badge (colorClass ++ " rounded-pill text-nowrap d-inline-flex align-items-center gap-1")
        [ Attr.title explicacion ]
        (i [ class ("bi " ++ icono) ] [] :: contenido)


{-| Las tres tarjetas del final: la propina, lo que nadie reclamó y el total,
cada una con cuánto de eso te toca a vos.
-}
viewTotales : Maybe ULID -> DetalleRepartija -> Repartija -> Html Msg
viewTotales userId detalle repartija =
    let
        tarjeta titulo filas =
            Bs.card [ class "mb-3" ]
                [ Bs.cardHeader [ class "d-flex align-items-center gap-2" ] titulo
                , Bs.cardBody [ class "py-2" ] filas
                ]

        fila izquierda derecha =
            div [ class "d-flex align-items-center justify-content-end flex-wrap gap-2" ]
                (izquierda ++ derecha)

        tuParte reparto =
            case userId of
                Nothing ->
                    text ""

                Just _ ->
                    fila [ span [] [ text "Tu parte" ] ]
                        [ span [ class "text-danger" ]
                            [ text ("$ " ++ Monto.toString (Repartija.parteDe userId reparto)) ]
                        ]

        sinReclamar =
            detalle.sinReclamar
    in
    div [ Attr.id "repartija-totales" ]
        [ tarjeta
            [ text "Propina" ]
            [ fila [] [ text ("$ " ++ Monto.toString repartija.extra) ]
            , tuParte detalle.propina
            ]
        , if sinReclamar.cantidadDeItems == 0 then
            text ""

          else
            tarjeta
                [ text "Items sin reclamar"
                , Bs.badge "text-bg-secondary rounded-pill" [] [ text (String.fromInt sinReclamar.cantidadDeItems) ]
                ]
                [ fila
                    [ linkDeAyuda QuePasaConLoNoReclamado DesdeTotales (nombreDistribucion repartija.distribucionDeSobras) ]
                    [ text ("$ " ++ Monto.toString sinReclamar.montoTotal) ]
                , tuParte sinReclamar.porParticipante
                ]
        , tarjeta
            [ strong [] [ text "Total" ] ]
            [ fila [] [ text ("$ " ++ Monto.toString (Repartija.total repartija)) ]
            , div [ class "fw-bold" ] [ tuParte detalle.totales ]
            ]
        ]


ayudaToString : Ayuda -> String
ayudaToString ayuda =
    case ayuda of
        ComoFuncionaLaRepartija ->
            "como_funciona"

        ComoDividirUnItem ->
            "como_dividir_un_item"

        QuePasaConLoNoReclamado ->
            "items_sin_reclamar"


origenAyudaToString : OrigenAyuda -> String
origenAyudaToString origen =
    case origen of
        DesdeEncabezado ->
            "encabezado"

        DesdeItem ->
            "item"

        DesdeTotales ->
            "totales"


{-| La encuesta sobre el rediseño, abajo del total. Está siempre: después de
responder queda el agradecimiento.

Con tocar una opción ya se manda, así que eso es lo que la card dice antes y
después; el comentario es un extra que queda escondido atrás de un link.

-}
viewFeedback : Feedback -> Html Msg
viewFeedback feedback =
    let
        opcion opinion =
            Bs.btn Bs.Secondary
                [ class "flex-grow-1 d-flex flex-column align-items-center gap-1"
                , Telemetry.click "repartija.redesign_opinion"
                , onClick (ResponderFeedback opinion)
                ]
                [ i [ class ("bi fs-4 " ++ Opinion.icono opinion) ] []
                , span [] [ text (Opinion.label opinion) ]
                ]

        linkChico track icono label msg =
            button
                [ type_ "button"
                , class "btn btn-link btn-sm p-0 d-inline-flex align-items-center gap-1"
                , Telemetry.click track
                , onClick msg
                ]
                [ i [ class ("bi " ++ icono) ] []
                , text label
                ]
    in
    case feedback of
        SinResponder ->
            Bs.card [ Attr.id "repartija-feedback", class "mb-3" ]
                [ Bs.cardHeader [ class "d-flex align-items-center gap-2" ]
                    [ i [ class "bi bi-chat-left-heart" ] []
                    , span [ class "fw-semibold" ] [ text "¿Qué te pareció esta pantalla?" ]
                    ]
                , Bs.cardBody [ class "py-3" ]
                    [ p [ class "mb-3" ]
                        [ text "Rediseñamos la repartija. ¿Esta versión te resulta mejor o peor que la anterior?" ]
                    , div [ class "d-flex gap-2" ]
                        [ opcion Mejor
                        , opcion Igual
                        , opcion Peor
                        ]
                    , p [ class "small text-body-secondary text-center mb-0 mt-2" ]
                        [ text "Con elegir una opción ya se envía." ]
                    ]
                ]

        Respondido respuesta ->
            Bs.card [ Attr.id "repartija-feedback", class "mb-3" ]
                [ Bs.cardBody [ class "py-4 text-center" ]
                    [ i [ class ("bi fs-1 text-success " ++ Opinion.icono respuesta.opinion) ] []
                    , p [ class "fw-semibold fs-5 mb-1 mt-2" ] [ text "¡Listo, gracias!" ]
                    , p [ class "text-body-secondary mb-0" ]
                        [ text "Tu respuesta "
                        , strong [] [ text (Opinion.label respuesta.opinion) ]
                        , text " ya se envió."
                        ]
                    , if respuesta.comentarioEnviado then
                        p [ class "small text-body-secondary mb-0 mt-3" ]
                            [ i [ class "bi bi-check2 me-1" ] []
                            , text "Tu comentario también se envió."
                            ]

                      else if respuesta.comentarioAbierto then
                        div [ class "mt-3 text-start" ]
                            [ Html.textarea
                                [ class "form-control form-control-sm"
                                , Attr.id "feedback-comentario"
                                , Attr.rows 2
                                , Attr.placeholder "¿Por qué? (opcional)"
                                , Attr.attribute "aria-label" "Comentario sobre el rediseño"
                                , value respuesta.comentario
                                , onInput ComentarioFeedbackCambiado
                                ]
                                []
                            , div [ class "d-flex justify-content-end" ]
                                [ Bs.btn Bs.Secondary
                                    [ class "btn-sm mt-2"
                                    , disabled (String.trim respuesta.comentario == "")
                                    , Telemetry.click "repartija.redesign_send_comment"
                                    , onClick EnviarComentarioFeedback
                                    ]
                                    [ text "Enviar comentario" ]
                                ]
                            ]

                      else
                        text ""
                    , if respuesta.comentarioAbierto || respuesta.comentarioEnviado then
                        text ""

                      else
                        div [ class "d-flex justify-content-center mt-3" ]
                            [ linkChico "repartija.redesign_add_comment" "bi-chat-left-text" "Agregar un comentario" AbrirComentarioFeedback ]
                    ]
                ]


nombreDistribucion : DistribucionDeSobras -> String
nombreDistribucion distribucion =
    case distribucion of
        SobrasProporcional ->
            "Proporcional"

        SobrasNoDistribuir ->
            "No se distribuyen"


viewRepartirModal : Maybe RepartijaItem -> Html Msg
viewRepartirModal maybeItem =
    let
        opcion track icono titulo descripcion msg =
            button
                [ type_ "button"
                , class "btn btn-outline-primary w-100 text-start d-flex align-items-center gap-3 p-3 mb-2"
                , Telemetry.click track
                , onClick msg
                ]
                [ i [ class ("bi fs-1 " ++ icono) ] []
                , div []
                    [ div [ class "fw-semibold fs-5" ] [ text titulo ]
                    , div [] [ text descripcion ]
                    ]
                ]
    in
    Bs.modal
        { id = "repartija-modal-repartir"
        , isOpen = maybeItem /= Nothing
        , onClose = CloseRepartirModal
        , title = "Reclamar item"
        , centered = False
        , body =
            case maybeItem of
                Just item ->
                    [ h6 [ class "fw-semibold" ] [ text "Decisión colaborativa" ]
                    , p [ class "mb-1" ]
                        [ text "Para reclamar este item es necesario que definas "
                        , strong [] [ text "cómo se debe repartir" ]
                        , text ". Esta decisión quedará reflejada en los demás participantes."
                        ]
                    , p []
                        [ text "Podés cambiarla una vez realizada, pero una vez que otros también reclamen este item ya no podrás revertir esta decisión." ]
                    , opcion "repartija.split_by_quantity"
                        "bi-people-fill"
                        "Por cantidad"
                        "Cada participante indica cuántas unidades consumió."
                        (ChangeCurrentClaim item 1)
                    , opcion "repartija.split_equally"
                        "bi-arrows-angle-contract"
                        "División equitativa"
                        "Se divide en partes iguales entre los participantes que reclamen este item."
                        (JoinCurrentClaim item)
                    ]

                Nothing ->
                    []
        , footer =
            [ Bs.btn Bs.Transparent [ Telemetry.click "repartija.split_cancel", onClick CloseRepartirModal ] [ text "Cancelar" ] ]
        }


{-| Quién reclamó qué en un item, que es lo que hay detrás del "Repartidos 3 de
5" de la card.
-}
viewDetalleModal : GrupoLike g -> DetalleRepartija -> Repartija -> Maybe RepartijaItem -> Html Msg
viewDetalleModal grupo detalle repartija maybeItem =
    Bs.modal
        { id = "repartija-modal-detalle-item"
        , isOpen = maybeItem /= Nothing
        , onClose = CerrarDetalleDeItem
        , centered = False
        , title = maybeItem |> Maybe.map .nombre |> Maybe.withDefault ""
        , body =
            case maybeItem of
                Just item ->
                    let
                        claims =
                            Repartija.claimsDeItem repartija item

                        estado =
                            Repartija.detalleDeItem detalle item.id
                                |> Maybe.map .estado
                                |> Maybe.withDefault EstadoSinReclamar
                    in
                    [ p []
                        [ strong [] [ text "Cantidad: " ]
                        , text (String.fromInt item.cantidad)
                        , span [ class "text-body-tertiary mx-2" ] [ text "|" ]
                        , strong [] [ text "Total: " ]
                        , text ("$ " ++ Monto.toString item.monto)
                        ]
                    , p [] [ strong [] [ text (nombreDelReparto estado) ] ]
                    , if List.isEmpty claims then
                        p [ class "mb-0 text-secondary" ] [ text "Todavía no lo reclamó nadie." ]

                      else
                        Bs.listGroup []
                            (claims
                                |> List.map
                                    (\claim ->
                                        Bs.listGroupItem [ class "d-flex justify-content-between gap-3" ]
                                            [ span [] [ text (lookupNombreParticipante grupo claim.participante) ]
                                            , span [ class "text-secondary" ]
                                                [ text
                                                    (case claim.cantidad of
                                                        Just cantidad ->
                                                            unidades cantidad

                                                        Nothing ->
                                                            "Una parte igual"
                                                    )
                                                ]
                                            ]
                                    )
                            )
                    ]

                Nothing ->
                    []
        , footer =
            [ Bs.btn Bs.Transparent [ Telemetry.click "repartija.item_detail_close", onClick CerrarDetalleDeItem ] [ text "Cerrar" ] ]
        }


viewAccionModal : GrupoLike g -> Moneda -> Repartija -> Maybe AccionEnCurso -> Html Msg
viewAccionModal grupo moneda repartija maybeAccion =
    case maybeAccion of
        Nothing ->
            text ""

        Just accion ->
            let
                tipo =
                    conItemActual repartija accion.tipo

                ( titulo, explicacion ) =
                    case tipo of
                        AgregarItemNuevo ->
                            ( "Agregar un item"
                            , "Se agrega un item nuevo, sin reclamos. Lo que vale sale de la propina, así que el total del gasto no cambia."
                            )

                        AgregarUnidadesAlItem _ ->
                            ( "Agregar unidades"
                            , "Se suman unidades al item con el mismo precio por unidad. Lo que el item pasa a valer de más sale de la propina, así que el total del gasto no cambia."
                            )

                        QuitarUnidadesDelItem _ ->
                            ( "Quitar unidades"
                            , "Se sacan unidades del item con el mismo precio por unidad. Lo que el item pasa a valer de menos se suma a la propina, así que el total del gasto no cambia. Lo que ya reclamó cada uno no cambia."
                            )

                        SepararUnidadesDelItem _ ->
                            ( "Separar en un item nuevo"
                            , "Las unidades separadas pasan a un item nuevo, sin reclamos, con el mismo precio por unidad. El total del gasto no cambia."
                            )

                        RepartirItemProporcionalmente _ ->
                            ( "Repartir proporcionalmente"
                            , "Lo que vale el item se reparte entre todos, en proporción a lo que consumió cada uno. El item sale de la lista y sus reclamos se borran. El total del gasto no cambia."
                            )

                        UnirItemConOtro _ ->
                            ( "Unir con otro item"
                            , "Los dos items quedan en uno solo, con las unidades y el total de los dos. Si los reclamos de los dos son de la misma forma (todos por cantidad o todos en partes iguales) se juntan; si no, el item unido queda sin reclamar. El total del gasto no cambia."
                            )

                enviando =
                    estaConfirmando accion
            in
            Bs.modal
                { id = "repartija-modal-accion"
                , isOpen = True
                , onClose = CerrarAccion
                , centered = False
                , title =
                    case itemDeAccion tipo of
                        Just item ->
                            titulo ++ ": " ++ item.nombre

                        Nothing ->
                            titulo
                , body =
                    [ p [] [ text explicacion ]
                    , Html.form [ Attr.id "repartija-form-accion", onSubmit (AccionForm Form.Submit) ]
                        [ viewDatosDeAccion moneda repartija tipo accion ]
                    , viewPreviewDeAccion grupo repartija tipo accion
                    , case accion.confirmacion of
                        Failure e ->
                            div [ class "mt-3" ] [ viewHttpError e ]

                        Success (AccionDeRepartijaRechazada motivo) ->
                            div [ class "alert alert-danger mt-3 mb-0" ] [ text (explicacionDeRechazo tipo motivo) ]

                        _ ->
                            text ""
                    ]
                , footer =
                    [ Bs.btn Bs.Transparent
                        [ Telemetry.click "repartija.action_cancel", onClick CerrarAccion ]
                        [ text
                            (if enviando then
                                "Cerrar"

                             else
                                "Cancelar"
                            )
                        ]
                    , Bs.btn Bs.Primary
                        [ class "d-inline-flex align-items-center gap-2"
                        , type_ "submit"
                        , Attr.attribute "form" "repartija-form-accion"
                        , disabled (not (puedeConfirmar accion))
                        , Telemetry.click "repartija.action_confirm"
                        ]
                        [ if enviando then
                            span [ class "spinner-border spinner-border-sm", Attr.attribute "aria-hidden" "true" ] []

                          else
                            text ""
                        , text "Confirmar"
                        ]
                    ]
                }


viewDatosDeAccion : Moneda -> Repartija -> TipoDeAccion -> AccionEnCurso -> Html Msg
viewDatosDeAccion moneda repartija tipo accion =
    let
        bloqueado =
            estaConfirmando accion

        campo nombreDelCampo =
            Form.getFieldAsString nombreDelCampo accion.form

        campoDeCantidad label maximo ayuda =
            [ Html.label [ class "form-label", Attr.for "accion-item-cantidad" ] [ text label ]
            , Html.map AccionForm <|
                textInput
                    ([ type_ "number"
                     , Attr.id "accion-item-cantidad"
                     , Attr.min "1"
                     , disabled bloqueado
                     ]
                        ++ maximo
                    )
                    (campo "cantidad")
            , div [ class "form-text" ] [ text ayuda ]
            ]

        cantidadDelItem item =
            "El item tiene " ++ unidades item.cantidad ++ "."

        hastaQuedarUna item =
            [ Attr.max (String.fromInt (item.cantidad - 1)) ]

        campoDeNombre label =
            [ Html.label [ class "form-label mt-3", Attr.for "accion-item-nombre" ] [ text label ]
            , Html.map AccionForm <|
                textInput [ Attr.id "accion-item-nombre", disabled bloqueado ] (campo "nombre")
            ]
    in
    case tipo of
        AgregarItemNuevo ->
            div [ class "mb-3" ]
                [ Html.label [ class "form-label", Attr.for "accion-item-nombre" ] [ text "Nombre" ]
                , Html.map AccionForm <|
                    textInput
                        [ Attr.id "accion-item-nombre"
                        , Attr.placeholder "Ej: Postre"
                        , disabled bloqueado
                        ]
                        (campo "nombre")
                , Html.label [ class "form-label mt-3", Attr.for "monto" ] [ text "Monto total" ]
                , Html.map AccionForm <|
                    Bs.montoInput (escalaDe moneda) (campo "monto") [ disabled bloqueado ]
                , div [ class "form-text mb-3" ] [ text "Lo que valen todas las unidades juntas." ]
                , div [] (campoDeCantidad "Unidades" [] "Cuántas unidades son.")
                ]

        AgregarUnidadesAlItem item ->
            div [ class "mb-3" ] (campoDeCantidad "Unidades a agregar" [] (cantidadDelItem item))

        QuitarUnidadesDelItem item ->
            div [ class "mb-3" ] (campoDeCantidad "Unidades a quitar" (hastaQuedarUna item) (cantidadDelItem item))

        SepararUnidadesDelItem item ->
            div [ class "mb-3" ]
                (campoDeCantidad "Unidades a separar" (hastaQuedarUna item) (cantidadDelItem item)
                    ++ campoDeNombre "Nombre del item nuevo"
                )

        RepartirItemProporcionalmente _ ->
            text ""

        UnirItemConOtro item ->
            let
                resumenConPrecio i =
                    [ Just (unidades i.cantidad)
                    , precioUnitarioTexto i
                    , Just ("$ " ++ Monto.toString i.monto)
                    ]
                        |> List.filterMap identity
                        |> String.join " · "

                opciones =
                    ( "", "Elegí un item" )
                        :: (repartija.items
                                |> List.filter (\otro -> otro.id /= item.id)
                                |> List.map (\otro -> ( otro.id, otro.nombre ++ " · " ++ resumenConPrecio otro ))
                           )
            in
            div [ class "mb-3" ]
                (Html.label [ class "form-label", Attr.for "otroItem" ] [ text "Unir con" ]
                    :: Html.map AccionForm (Bs.selectInput opciones (campo "otroItem") [ disabled bloqueado ])
                    :: div [ class "form-text" ] [ text ("Este item: " ++ resumenConPrecio item ++ ".") ]
                    :: campoDeNombre "Nombre del item unido"
                )


viewPreviewDeAccion : GrupoLike g -> Repartija -> TipoDeAccion -> AccionEnCurso -> Html Msg
viewPreviewDeAccion grupo repartija tipo accion =
    case accion.preview of
        NotAsked ->
            div [ class "alert alert-secondary mb-0" ]
                [ text
                    (case tipo of
                        UnirItemConOtro _ ->
                            "Elegí con qué item unirlo para ver cómo queda."

                        AgregarItemNuevo ->
                            "Poné el monto y las unidades para ver cómo queda."

                        _ ->
                            "Poné cuántas unidades para ver cómo queda."
                    )
                ]

        Loading ->
            div [ class "d-flex align-items-center gap-2 text-body-secondary" ]
                [ span [ class "spinner-border spinner-border-sm", Attr.attribute "aria-hidden" "true" ] []
                , text "Calculando cómo queda…"
                ]

        Failure e ->
            viewHttpError e

        Success (PreviewDeAccionRechazada motivo) ->
            div [ class "alert alert-warning mb-0" ] [ text (explicacionDeRechazo tipo motivo) ]

        Success (PreviewDeAccionAceptada despues) ->
            let
                item =
                    itemDeAccion tipo

                idsDeAntes =
                    List.map .id repartija.items

                resumenDeItem i =
                    unidades i.cantidad ++ " · $ " ++ Monto.toString i.monto

                badgeDespues i =
                    Repartija.detalleDeItem despues.detalle i.id
                        |> Maybe.map (\deItem -> div [ class "mt-1" ] [ badgeDeItem deItem ])
                        |> Maybe.withDefault (text "")

                fila titulo contenido =
                    Bs.listGroupItem [] (div [ class "fw-semibold" ] [ text titulo ] :: contenido)

                flecha =
                    i [ class "bi bi-arrow-right mx-2" ] []

                itemDespues =
                    item |> Maybe.andThen (\antes -> despues.repartija.items |> find (\i -> i.id == antes.id))

                antesYDespues antes ahora =
                    div []
                        [ span [ class "text-body-secondary" ] [ text antes ]
                        , flecha
                        , text ahora
                        ]

                filaDelItem =
                    case ( item, itemDespues ) of
                        ( Just antes, Just itemQueQueda ) ->
                            [ fila antes.nombre
                                [ antesYDespues (resumenDeItem antes) (resumenDeItem itemQueQueda)
                                , badgeDespues itemQueQueda
                                ]
                            ]

                        ( Just antes, Nothing ) ->
                            [ fila antes.nombre [ antesYDespues (resumenDeItem antes) "se reparte entre todos" ] ]

                        ( Nothing, _ ) ->
                            []

                otrosQueSeVan =
                    repartija.items
                        |> List.filter
                            (\i -> Just i.id /= Maybe.map .id item && not (List.any (\d -> d.id == i.id) despues.repartija.items))

                nombreDelQueQueda =
                    case ( itemDespues, item ) of
                        ( Just queQueda, _ ) ->
                            queQueda.nombre

                        ( Nothing, Just antes ) ->
                            antes.nombre

                        ( Nothing, Nothing ) ->
                            ""

                filasDeLosQueSeVan =
                    otrosQueSeVan
                        |> List.map
                            (\i ->
                                fila i.nombre
                                    [ antesYDespues (resumenDeItem i) ("se une a " ++ nombreDelQueQueda) ]
                            )

                filaDePropina =
                    if repartija.extra == despues.repartija.extra then
                        []

                    else
                        [ fila "Propina"
                            [ antesYDespues ("$ " ++ Monto.toString repartija.extra)
                                ("$ " ++ Monto.toString despues.repartija.extra)
                            ]
                        ]

                involucrados =
                    List.filterMap identity [ Maybe.map .id item ] ++ List.map .id otrosQueSeVan

                reclamosDeAntes =
                    repartija.claims |> List.filter (\c -> List.member c.itemId involucrados)

                nombres claims =
                    claims
                        |> List.map (\c -> lookupNombreParticipante grupo c.participante)
                        |> List.Extra.unique
                        |> String.join ", "

                reclamoDespues itemId claim =
                    Bs.listGroupItem [ class "d-flex justify-content-between gap-3" ]
                        [ span []
                            [ text (lookupNombreParticipante grupo claim.participante)
                            , span [ class "text-body-secondary ms-2" ]
                                [ text
                                    (case claim.cantidad of
                                        Just n ->
                                            unidades n

                                        Nothing ->
                                            "Una parte igual"
                                    )
                                ]
                            ]
                        , span [] [ text ("$ " ++ Monto.toString (Repartija.consumoDeItem despues.detalle itemId (Just claim.participante))) ]
                        ]

                reclamos =
                    case tipo of
                        RepartirItemProporcionalmente _ ->
                            if List.isEmpty reclamosDeAntes then
                                text ""

                            else
                                div [ class "alert alert-warning mt-3 mb-0" ]
                                    [ text ("Se borran los reclamos de " ++ nombres reclamosDeAntes ++ ".") ]

                        UnirItemConOtro unido ->
                            let
                                reclamosDeDespues =
                                    despues.repartija.claims |> List.filter (\c -> c.itemId == unido.id)
                            in
                            if List.isEmpty reclamosDeDespues then
                                if List.isEmpty reclamosDeAntes then
                                    text ""

                                else
                                    div [ class "alert alert-warning mt-3 mb-0" ]
                                        [ text
                                            ("Unos reclamos son por cantidad y otros en partes iguales, así que no se pueden juntar: el item unido queda sin reclamar y se borran los reclamos de "
                                                ++ nombres reclamosDeAntes
                                                ++ "."
                                            )
                                        ]

                            else
                                div [ class "mt-3" ]
                                    [ h6 [ class "fw-semibold" ] [ text "Quién lo reclama después" ]
                                    , Bs.listGroup [] (List.map (reclamoDespues unido.id) reclamosDeDespues)
                                    ]

                        _ ->
                            text ""

                filasNuevas =
                    despues.repartija.items
                        |> List.filter (\i -> not (List.member i.id idsDeAntes))
                        |> List.map
                            (\nuevo ->
                                fila ("Item nuevo: " ++ nuevo.nombre)
                                    [ div [] [ text (resumenDeItem nuevo) ]
                                    , badgeDespues nuevo
                                    ]
                            )
            in
            div []
                [ h6 [ class "fw-semibold" ] [ text "Así va a quedar" ]
                , Bs.listGroup [] (filaDelItem ++ filasDeLosQueSeVan ++ filasNuevas ++ filaDePropina)
                , reclamos
                ]


explicacionDeRechazo : TipoDeAccion -> AccionDeRepartijaInvalida -> String
explicacionDeRechazo tipo motivo =
    let
        hastaQuedarUna item =
            "Tiene que ser entre 1 y "
                ++ String.fromInt (item.cantidad - 1)
                ++ ": al item le tiene que quedar al menos una unidad."
    in
    case motivo of
        ItemNoEncontrado ->
            "Este item ya no está en la repartija. Recargá la página para ver cómo quedó."

        ItemSinUnidades ->
            "El item no tiene unidades, así que no se sabe cuánto vale cada una."

        CantidadDeUnidadesInvalida ->
            case tipo of
                QuitarUnidadesDelItem item ->
                    hastaQuedarUna item

                SepararUnidadesDelItem item ->
                    hastaQuedarUna item

                _ ->
                    "Tiene que ser al menos 1 unidad."

        ItemSinNombre ->
            "Ponele un nombre al item."

        NoPuedeQuedarSinItems ->
            "Es el único item de la repartija, y la repartija no puede quedar sin items."

        UnirConElMismoItem ->
            "Elegí otro item para unir."


viewAyudaModal : Maybe Ayuda -> Html Msg
viewAyudaModal ayuda =
    Bs.modal
        { id = "repartija-modal-ayuda"
        , isOpen = ayuda /= Nothing
        , onClose = CerrarAyuda
        , centered = False
        , title =
            case ayuda of
                Just ComoDividirUnItem ->
                    "¿Cómo dividir un item?"

                Just QuePasaConLoNoReclamado ->
                    "Items sin reclamar"

                _ ->
                    "¿Cómo funciona la Repartija?"
        , body =
            case ayuda of
                Just ComoFuncionaLaRepartija ->
                    [ p [] [ text "La repartija divide un gasto según lo que consumió cada uno. Cada participante busca en la lista los items en los que participó y los reclama." ]
                    , p [] [ text "El primero en reclamar un item decide cómo se reparte: por cantidad o en partes iguales. Los demás se suman a esa forma de reparto." ]
                    , p [] [ text "La propina se reparte en proporción a lo que consumió cada uno. A la derecha vas a ver cuánto te toca a vos." ]
                    , p [ class "mb-0" ] [ text "La etiqueta de cada item dice cómo quedó: en verde está repartido, en amarillo tiene algo raro pero se puede repartir así, y en rojo todavía no está bien repartido: revisalos hasta que no quede ninguno en rojo." ]
                    ]

                Just ComoDividirUnItem ->
                    [ h6 [ class "fw-semibold" ] [ text "Por cantidad" ]
                    , p [] [ text "Cada participante indica cuántas unidades consumió con los botones + y −. El item queda resuelto cuando entre todos se reclaman exactamente las unidades que hay." ]
                    , h6 [ class "fw-semibold" ] [ text "División equitativa" ]
                    , p [] [ text "El total del item se divide en partes iguales entre quienes tocaron \"Participar\". Hacen falta al menos dos personas para que quede resuelto." ]
                    , h6 [ class "fw-semibold" ] [ text "Repartido incorrectamente" ]
                    , p [ class "mb-0" ] [ text "Si un item tiene reclamos por cantidad y equitativos a la vez no se puede repartir: pónganse de acuerdo en una sola forma." ]
                    ]

                Just QuePasaConLoNoReclamado ->
                    [ p [] [ text "Acá entra todo lo que quedó sin dueño: los items que nadie reclamó y las unidades que faltan reclamar en los que se reparten por cantidad." ]
                    , h6 [ class "fw-semibold" ] [ text "Proporcional" ]
                    , p [] [ text "Esa plata se reparte entre los que sí reclamaron items, en proporción a lo que consumió cada uno." ]
                    , h6 [ class "fw-semibold" ] [ text "No se distribuyen" ]
                    , p [ class "mb-0" ] [ text "Esa plata no se reparte sola: los que consumieron tienen que reclamar lo que falta para que la repartija sea válida." ]
                    ]

                Nothing ->
                    []
        , footer =
            [ Bs.btn Bs.Transparent [ Telemetry.click "repartija.help_close", onClick CerrarAyuda ] [ text "Cerrar" ] ]
        }
