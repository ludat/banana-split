module Pages.Grupos.GrupoId_.Repartijas.RepartijaId_ exposing (Ayuda, Feedback, Filtros, Model, Msg, Opinion, OrigenAyuda, page)

import Components.Bootstrap as Bs
import Effect exposing (Effect)
import Generated.Api as Api exposing (DetalleDeItem, DetalleRepartija, DiferenciaDeUnidades(..), DistribucionDeSobras(..), EstadoDeItem(..), Repartija, RepartijaClaim, RepartijaItem, ResolucionDeItem(..), ULID)
import Html exposing (Html, button, div, h5, h6, i, input, li, p, span, strong, text, ul)
import Html.Attributes as Attr exposing (class, classList, disabled, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Layouts
import List.Extra exposing (find)
import Models.Grupo exposing (GrupoLike, lookupNombreParticipante)
import Models.Monto as Monto
import Models.Repartija as Repartija
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import Shared.Msg
import Utils.Http exposing (viewHttpError)
import Utils.Telemetry as Telemetry
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
    , modoAvanzado : Bool
    , ayuda : Maybe Ayuda
    , feedback : Feedback
    }


{-| La encuesta sobre el rediseño. Recién cuando alguien ya opinó se le ofrece
contar por qué; no se recuerda la respuesta, así que se puede opinar de nuevo
cuantas veces se quiera.
-}
type Feedback
    = SinResponder
    | Respondido { opinion : Opinion, comentarioAbierto : Bool, comentario : String, comentarioEnviado : Bool }


type Opinion
    = Mejor
    | Igual
    | Peor


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
      , modoAvanzado = False
      , ayuda = Nothing
      , feedback = SinResponder
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
    | CreateRepartijaClaimResponded (WebData Api.RepartijaForFrontend)
    | DeleteRepartijaClaimResponded ULID (WebData String)
    | ToggleFiltroReclamadosPorVos
    | ToggleFiltroNoResueltos
    | SetFiltroParticipante (Maybe ULID)
    | VerItemsDe ULID
    | ToggleParticipantes
    | ToggleAlerta
    | SetModoAvanzado Bool
    | AbrirAyuda Ayuda OrigenAyuda
    | CerrarAyuda
    | DismissIntro
    | ResponderFeedback Opinion
    | ComentarioFeedbackCambiado String
    | AbrirComentarioFeedback
    | EnviarComentarioFeedback
    | VolverAOpinar


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

        VerItemsDe participanteId ->
            -- El filtro vive en el modo avanzado, así que mirar lo de alguien
            -- desde la lista de participantes también lo prende: si no, se
            -- esconderían items sin nada en pantalla que lo explique.
            let
                filtros =
                    model.filtros

                yaEstaba =
                    filtros.porParticipante == Just participanteId
            in
            ( { model
                | modoAvanzado = not yaEstaba || model.modoAvanzado
                , filtros =
                    { filtros
                        | porParticipante =
                            if yaEstaba then
                                Nothing

                            else
                                Just participanteId
                    }
              }
            , Effect.telemetryEvent
                (Telemetry.repartijaFiltro
                    { filtro = "participante_desde_la_lista", activo = not yaEstaba }
                )
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

        SetModoAvanzado avanzado ->
            -- Los filtros viven en el modo avanzado, así que al salir se
            -- limpian: dejarlos puestos escondería items sin nada en pantalla
            -- que lo explique.
            ( { model
                | modoAvanzado = avanzado
                , filtros =
                    if avanzado then
                        model.filtros

                    else
                        sinFiltros
              }
            , Effect.telemetryEvent (Telemetry.repartijaModoDeVista avanzado)
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
            , Effect.telemetryEvent (Telemetry.repartijaRediseno (opinionToString opinion))
            )

        AbrirComentarioFeedback ->
            ( case model.feedback of
                Respondido respuesta ->
                    { model | feedback = Respondido { respuesta | comentarioAbierto = True } }

                SinResponder ->
                    model
            , Effect.none
            )

        VolverAOpinar ->
            ( { model | feedback = SinResponder }
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
                        (Telemetry.repartijaRedisenoComentario (opinionToString respuesta.opinion))
                        respuesta.comentario
                    )

                SinResponder ->
                    ( model, Effect.none )

        CreateRepartijaClaimResponded webDataRepartija ->
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

        DeleteRepartijaClaimResponded claimId webDataResponse ->
            case ( webDataResponse, Store.getRepartija model.repartijaId store ) of
                ( Success _, Success repartijaPage ) ->
                    let
                        repartija =
                            repartijaPage.repartija

                        updatedRepartija =
                            { repartija
                                | claims = repartija.claims |> List.filter (\c -> c.id /= claimId)
                            }
                    in
                    ( { model | pendingItemOperation = Nothing }
                    , Effect.batch
                        [ Store.updateRepartijaForFrontend model.repartijaId { repartijaPage | repartija = updatedRepartija }
                        , Store.invalidateResumen model.grupoId
                        , Store.invalidatePagos model.grupoId
                        , Store.refreshRepartija model.repartijaId
                        ]
                    )

                _ ->
                    ( { model | pendingItemOperation = Nothing }
                    , Effect.batch [ Store.refreshRepartija model.repartijaId ]
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
                                            Api.putRepartijasByRepartijaId model.repartijaId
                                                { id = emptyUlid
                                                , participante = participanteId
                                                , itemId = item.id
                                                , cantidad = Just <| newCantidad
                                                }
                                                (RemoteData.fromResult >> CreateRepartijaClaimResponded)

                                      else
                                        Effect.sendCmd <|
                                            Api.deleteRepartijasClaimsByClaimId oldClaim.id
                                                (RemoteData.fromResult >> DeleteRepartijaClaimResponded oldClaim.id)
                                    )

                                Nothing ->
                                    ( { model | pendingItemOperation = Just item.id, repartirModalItem = Nothing }
                                    , if deltaCantidad > 0 then
                                        Effect.sendCmd <|
                                            Api.putRepartijasByRepartijaId model.repartijaId
                                                { id = emptyUlid
                                                , participante = participanteId
                                                , itemId = item.id
                                                , cantidad = Just deltaCantidad
                                                }
                                                (RemoteData.fromResult >> CreateRepartijaClaimResponded)

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
                        Api.putRepartijasByRepartijaId model.repartijaId
                            { id = emptyUlid
                            , participante = participanteId
                            , itemId = item.id
                            , cantidad = Nothing
                            }
                            (RemoteData.fromResult >> CreateRepartijaClaimResponded)
                    )

                Nothing ->
                    ( model, Effect.none )

        LeaveCurrentClaim item ->
            case maybeParticipanteId of
                Just _ ->
                    ( { model | pendingItemOperation = Just item.itemId }
                    , Effect.sendCmd <|
                        Api.deleteRepartijasClaimsByClaimId item.id
                            (RemoteData.fromResult >> DeleteRepartijaClaimResponded item.id)
                    )

                Nothing ->
                    ( model, Effect.none )



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
                        -- En mobile los filtros van arriba del encabezado de la
                        -- lista y en desktop abajo, así que el orden de esta
                        -- columna no es el del código.
                        [ div [ class "col-12 col-lg-8 d-flex flex-column" ]
                            [ if introDismissed then
                                text ""

                              else
                                div [ class "order-0" ] [ viewIntro ]
                            , div [ class "order-1" ]
                                [ viewAlertaNoResueltos model detalle repartija
                                , viewLinksDeAyuda
                                ]
                            , div [ class "order-2 d-lg-none mb-3" ] [ viewParticipantes model grupo repartija ]
                            , div [ class "order-3 order-lg-4" ]
                                [ if model.modoAvanzado then
                                    viewFiltros userId grupo model.filtros

                                  else
                                    text ""
                                ]
                            , div [ class "order-4 order-lg-3" ] [ viewItemsHeader model ]
                            , div [ class "order-5" ] [ viewItems model userId detalle repartija ]
                            ]
                        , div [ class "col-12 col-lg-4" ]
                            [ div [ class "d-none d-lg-block mb-3" ] [ viewParticipantes model grupo repartija ]
                            , viewTotales userId detalle repartija
                            , viewFeedback model.feedback
                            ]
                        ]
                    , viewRepartirModal model.repartirModalItem
                    , viewDetalleModal grupo detalle repartija model.detalleModalItem
                    , viewAyudaModal model.ayuda
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
            Bs.badge (colorClass ++ " d-inline-flex align-items-center gap-1")
                [ Attr.title titulo ]
                [ text (String.fromInt (List.length participantes))
                , i [ class "bi bi-people-fill" ] []
                ]

        grupoDeNombres colorClass titulo participantes =
            div [ class "d-flex align-items-center flex-wrap gap-2" ]
                [ cuenta colorClass titulo participantes
                , span [ class "fw-semibold" ]
                    [ text (participantes |> List.map .nombre |> String.join ", ") ]
                ]

        -- Expandida, cada uno de los que reclamaron es un botón que filtra la
        -- lista por esa persona, para ver qué se llevó.
        chipDeParticipante p =
            button
                [ type_ "button"
                , class "btn btn-sm d-inline-flex align-items-center gap-1"
                , classList
                    [ ( "btn-primary", model.filtros.porParticipante == Just p.id )
                    , ( "btn-outline-primary", model.filtros.porParticipante /= Just p.id )
                    ]
                , Attr.title ("Ver los items que reclamó " ++ p.nombre)
                , onClick (VerItemsDe p.id)
                ]
                [ i [ class "bi bi-eye" ] [], text p.nombre ]
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
                , onClick ToggleParticipantes
                ]
                [ h6 [ class "mb-0 flex-grow-1 text-start fw-semibold" ] [ text "Quiénes ya reclamaron" ]
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
                    , div [ class "mt-3" ]
                        [ h6 [ class "fw-semibold" ] [ text "No reclamaron items" ]
                        , grupoDeNombres "text-bg-primary" "Aún no reclamaron items" sinClaims
                        ]
                    ]

              else
                div [ class "mt-2 text-primary" ]
                    [ grupoDeNombres "text-bg-primary" "Ya reclamaron items" conClaims ]
            ]
        ]


viewItemsHeader : Model -> Html Msg
viewItemsHeader model =
    let
        opcion avanzado label =
            li []
                [ button
                    [ type_ "button"
                    , class "dropdown-item d-flex align-items-center gap-2"
                    , classList [ ( "active", model.modoAvanzado == avanzado ) ]
                    , onClick (SetModoAvanzado avanzado)
                    ]
                    [ i
                        [ class "bi bi-check-lg"
                        , classList [ ( "invisible", model.modoAvanzado /= avanzado ) ]
                        ]
                        []
                    , text label
                    ]
                ]
    in
    div [ class "d-flex align-items-center justify-content-between gap-2 mb-3" ]
        [ h5 [ class "mb-0" ] [ text "Items a repartir" ]
        , div [ class "dropdown" ]
            [ button
                [ type_ "button"
                , class "btn dropdown-toggle d-inline-flex align-items-center gap-2"

                -- El modo avanzado se destaca porque cambia lo que se ve en
                -- toda la lista: tiene que notarse que está puesto.
                , classList
                    [ ( "btn-outline-info", model.modoAvanzado )
                    , ( "btn-outline-secondary", not model.modoAvanzado )
                    ]
                , Attr.attribute "data-bs-toggle" "dropdown"
                , Attr.attribute "aria-expanded" "false"
                , Attr.title "Cuánto detalle mostrar de cada item"
                ]
                [ i [ class "bi bi-eye" ] []
                , span [ class "d-none d-sm-inline" ]
                    [ text
                        (if model.modoAvanzado then
                            "Modo avanzado"

                         else
                            "Modo simple"
                        )
                    ]
                ]
            , ul [ class "dropdown-menu dropdown-menu-end shadow" ]
                [ opcion False "Modo simple"
                , opcion True "Modo avanzado"
                ]
            ]
        ]


viewFiltros : Maybe ULID -> GrupoLike g -> Filtros -> Html Msg
viewFiltros userId grupo filtros =
    let
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
    Bs.card [ class "mb-3 bg-body-tertiary" ]
        [ Bs.cardBody [ class "py-3" ]
            [ h6 [ class "fw-semibold d-flex align-items-center gap-2" ]
                [ i [ class "bi bi-funnel" ] [], text "Filtros" ]
            , div [ class "d-flex flex-wrap align-items-center gap-3" ]
                [ case userId of
                    Just _ ->
                        switch "filtro-reclamados" "Reclamados por vos" filtros.reclamadosPorVos ToggleFiltroReclamadosPorVos

                    Nothing ->
                        text ""
                , switch "filtro-no-resueltos" "No resueltos" filtros.noResueltos ToggleFiltroNoResueltos
                , div [ class "input-group flex-grow-1", Attr.style "min-width" "14rem" ]
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


cumpleFiltros : Maybe ULID -> Filtros -> Repartija -> ( RepartijaItem, DetalleDeItem ) -> Bool
cumpleFiltros userId filtros repartija ( item, deItem ) =
    let
        reclamadoPor participanteId =
            Repartija.claimsDeItem repartija item
                |> List.any (\c -> c.participante == participanteId)

        cumple filtroActivo participanteId =
            not filtroActivo
                || (participanteId |> Maybe.map reclamadoPor |> Maybe.withDefault False)
    in
    cumple filtros.reclamadosPorVos userId
        && (not filtros.noResueltos || deItem.resolucion == ItemNoResuelto)
        && cumple (filtros.porParticipante /= Nothing) filtros.porParticipante


viewItems : Model -> Maybe ULID -> DetalleRepartija -> Repartija -> Html Msg
viewItems model userId detalle repartija =
    let
        -- Cada item con lo que calculó el backend de él. El detalle trae todos
        -- los items de la repartija, así que acá no se pierde ninguno.
        conDetalle =
            repartija.items
                |> List.filterMap
                    (\item ->
                        Repartija.detalleDeItem detalle item.id
                            |> Maybe.map (\deItem -> ( item, deItem ))
                    )
    in
    case List.filter (cumpleFiltros userId model.filtros repartija) conDetalle of
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
                    |> List.map (\( item, deItem ) -> viewItemCard model userId detalle repartija item deItem)
                )


unidades : Int -> String
unidades n =
    if n == 1 then
        "1 unidad"

    else
        String.fromInt n ++ " unidades"


viewItemCard : Model -> Maybe ULID -> DetalleRepartija -> Repartija -> RepartijaItem -> DetalleDeItem -> Html Msg
viewItemCard model userId detalle repartija item deItem =
    let
        claims =
            Repartija.claimsDeItem repartija item

        estado =
            deItem.estado

        userClaim =
            claims |> find (\c -> Just c.participante == userId)

        pending =
            model.pendingItemOperation == Just item.id || userId == Nothing

        boton variant contenido msg habilitado =
            button
                [ type_ "button"
                , class ("btn " ++ variant)
                , disabled (pending || not habilitado)
                , onClick msg
                ]
                contenido

        botonDeCantidad icono delta habilitado =
            boton
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
                    [ boton "btn-primary" [ text "Repartir" ] (OpenRepartirModal item) True ]

                EstadoRepartidoPorCantidad _ ->
                    [ botonesDeCantidad ]

                EstadoRepartidoEquitativamente _ ->
                    if participaEquitativo then
                        [ boton "btn-secondary" [ text "Salirse" ] salirse True ]

                    else
                        [ boton "btn-primary" [ text "Participar" ] (JoinCurrentClaim item) True ]

                EstadoRepartoIncorrecto ->
                    [ botonesDeCantidad
                    , if participaEquitativo then
                        boton "btn-secondary" [ text "Salirse" ] salirse True

                      else
                        boton "btn-primary" [ text "Participar" ] (JoinCurrentClaim item) True
                    ]

        tuParticipacion =
            case userClaim of
                Nothing ->
                    div []
                        [ div [ class "fw-semibold" ] [ text "Tu participación" ]
                        , div [ class "text-secondary" ] [ text "No reclamaste items" ]
                        ]

                Just claim ->
                    let
                        miConsumo =
                            Repartija.consumoDeItem detalle item.id userId
                    in
                    div [ Attr.title (explicacionDeConsumo estado item) ]
                        [ div [ class "fw-semibold" ] [ text "Tu participación" ]
                        , case claim.cantidad of
                            Just n ->
                                div []
                                    [ text "Reclamaste "
                                    , span [ class "text-primary" ] [ text (unidades n) ]
                                    ]

                            Nothing ->
                                div [ class "text-primary" ] [ text "Estás participando" ]
                        , div [ class "text-danger" ] [ text ("$ " ++ Monto.toString miConsumo) ]
                        ]
    in
    div [ class "card mb-3" ]
        [ div [ class "card-header" ]
            [ div [ class "d-flex flex-wrap align-items-center gap-2" ]
                [ h6 [ class "fs-5 mb-0" ] [ text item.nombre ], badgeDeItem deItem ]
            , div [ class "small text-body-secondary" ]
                [ text (unidades item.cantidad)
                , span [ class "mx-2" ] [ text "·" ]
                , text ("$ " ++ Monto.toString item.monto)
                ]
            ]
        , div [ class "card-body" ]
            [ div [ class "d-flex flex-wrap align-items-start gap-3" ]
                (List.concat
                    [ [ div [ class "flex-grow-1", Attr.style "min-width" "9rem" ] [ tuParticipacion ] ]
                    , if model.modoAvanzado then
                        let
                            -- La columna del medio, solo en modo avanzado: cómo se reparte el item
                            -- y cuánto va repartido, cada uno con su link a la explicación.
                            modoDeReparto =
                                let
                                    linkConAyuda ayuda label =
                                        button
                                            [ type_ "button"
                                            , class "btn btn-link p-0 text-start d-flex align-items-start gap-1"
                                            , onClick ayuda
                                            ]
                                            [ i [ class "bi bi-question-circle-fill mt-1" ] [], span [] [ text label ] ]
                                in
                                div [ class "flex-grow-1", Attr.style "min-width" "11rem" ]
                                    [ div [ class "fw-semibold" ] [ text "Modo de reparto" ]
                                    , case estado of
                                        EstadoSinReclamar ->
                                            div [ class "text-secondary" ] [ text "Aún no definido" ]

                                        _ ->
                                            div []
                                                [ linkConAyuda (AbrirAyuda ComoDividirUnItem DesdeItem) (nombreDelReparto estado)
                                                , linkConAyuda (AbrirDetalleDeItem item) (progresoDelReparto estado)
                                                ]
                                    ]
                        in
                        [ modoDeReparto ]

                      else
                        []
                    , [ div
                            [ class "ms-auto d-flex flex-column flex-sm-row flex-wrap align-items-end justify-content-end gap-2" ]
                            botones
                      ]
                    ]
                )
            , div [ class "mt-3" ]
                [ viewAcciones userId claims userClaim item ]
            ]
        ]


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


progresoDelReparto : EstadoDeItem -> String
progresoDelReparto estado =
    case estado of
        EstadoRepartidoPorCantidad UnidadesExactas ->
            "Todas las unidades repartidas"

        EstadoRepartidoPorCantidad (FaltanUnidades n) ->
            "Faltan repartir " ++ unidades n

        EstadoRepartidoPorCantidad (SobranUnidades n) ->
            "Sobran " ++ unidades n

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


viewAcciones : Maybe ULID -> List RepartijaClaim -> Maybe RepartijaClaim -> RepartijaItem -> Html Msg
viewAcciones userId claims userClaim item =
    let
        -- La forma de reparto se puede cambiar mientras nadie más haya
        -- reclamado el item: después ya es una decisión compartida.
        soloReclamoYo =
            not (List.isEmpty claims) && List.all (\c -> Just c.participante == userId) claims

        accion icono label msg =
            li []
                [ button [ type_ "button", class "dropdown-item", onClick msg ]
                    [ i [ class ("bi me-2 " ++ icono) ] [], text label ]
                ]

        acciones =
            List.filterMap identity
                [ if soloReclamoYo then
                    Just (accion "bi-arrow-left-right" "Cambiar forma de reparto" (OpenRepartirModal item))

                  else
                    Nothing
                , userClaim |> Maybe.map (\claim -> accion "bi-x-circle" "Quitar mi reclamo" (LeaveCurrentClaim claim))
                ]
    in
    div [ class "dropdown" ]
        [ button
            [ type_ "button"
            , class "btn btn-sm btn-outline-secondary dropdown-toggle"
            , Attr.attribute "data-bs-toggle" "dropdown"
            , Attr.attribute "aria-expanded" "false"
            , Attr.title "Acciones"
            ]
            [ i [ class "bi bi-lightning-charge-fill" ] [] ]
        , ul [ class "dropdown-menu shadow" ]
            (if List.isEmpty acciones then
                [ li [] [ span [ class "dropdown-item-text text-muted small" ] [ text "No hay acciones disponibles" ] ] ]

             else
                acciones
            )
        ]


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
    div []
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


opinionToString : Opinion -> String
opinionToString opinion =
    case opinion of
        Mejor ->
            "mejor"

        Igual ->
            "igual"

        Peor ->
            "peor"


opinionIcono : Opinion -> String
opinionIcono opinion =
    case opinion of
        Mejor ->
            "bi-emoji-smile"

        Igual ->
            "bi-emoji-neutral"

        Peor ->
            "bi-emoji-frown"


opinionLabel : Opinion -> String
opinionLabel opinion =
    case opinion of
        Mejor ->
            "Mejor"

        Igual ->
            "Igual"

        Peor ->
            "Peor"


{-| La encuesta sobre el rediseño, abajo del total. Está siempre: después de
responder se puede volver a opinar.

Con tocar una opción ya se manda, así que eso es lo que la card dice antes y
después; el comentario es un extra que queda escondido atrás de un link.

-}
viewFeedback : Feedback -> Html Msg
viewFeedback feedback =
    let
        opcion opinion =
            Bs.btn Bs.Secondary
                [ class "flex-grow-1 d-flex flex-column align-items-center gap-1"
                , onClick (ResponderFeedback opinion)
                ]
                [ i [ class ("bi fs-4 " ++ opinionIcono opinion) ] []
                , span [] [ text (opinionLabel opinion) ]
                ]

        linkChico icono label msg =
            button
                [ type_ "button"
                , class "btn btn-link btn-sm p-0 d-inline-flex align-items-center gap-1"
                , onClick msg
                ]
                [ i [ class ("bi " ++ icono) ] []
                , text label
                ]
    in
    case feedback of
        SinResponder ->
            Bs.card [ class "mb-3" ]
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
            Bs.card [ class "mb-3" ]
                [ Bs.cardBody [ class "py-4 text-center" ]
                    [ i [ class ("bi fs-1 text-success " ++ opinionIcono respuesta.opinion) ] []
                    , p [ class "fw-semibold fs-5 mb-1 mt-2" ] [ text "¡Listo, gracias!" ]
                    , p [ class "text-body-secondary mb-0" ]
                        [ text "Tu respuesta "
                        , strong [] [ text (opinionLabel respuesta.opinion) ]
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
                                    , onClick EnviarComentarioFeedback
                                    ]
                                    [ text "Enviar comentario" ]
                                ]
                            ]

                      else
                        text ""
                    , div [ class "d-flex justify-content-center gap-3 mt-3" ]
                        [ if respuesta.comentarioAbierto || respuesta.comentarioEnviado then
                            text ""

                          else
                            linkChico "bi-chat-left-text" "Agregar un comentario" AbrirComentarioFeedback
                        , linkChico "bi-arrow-counterclockwise" "Cambiar respuesta" VolverAOpinar
                        ]
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
        opcion icono titulo descripcion msg =
            button
                [ type_ "button"
                , class "btn btn-outline-primary w-100 text-start d-flex align-items-center gap-3 p-3 mb-2"
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
        { isOpen = maybeItem /= Nothing
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
                    , opcion "bi-people-fill"
                        "Por cantidad"
                        "Cada participante indica cuántas unidades consumió."
                        (ChangeCurrentClaim item 1)
                    , opcion "bi-arrows-angle-contract"
                        "División equitativa"
                        "Se divide en partes iguales entre los participantes que reclamen este item."
                        (JoinCurrentClaim item)
                    ]

                Nothing ->
                    []
        , footer =
            [ Bs.btn Bs.Transparent [ onClick CloseRepartirModal ] [ text "Cancelar" ] ]
        }


{-| Quién reclamó qué en un item, que es lo que hay detrás del "Repartidos 3 de
5" de la card.
-}
viewDetalleModal : GrupoLike g -> DetalleRepartija -> Repartija -> Maybe RepartijaItem -> Html Msg
viewDetalleModal grupo detalle repartija maybeItem =
    Bs.modal
        { isOpen = maybeItem /= Nothing
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
            [ Bs.btn Bs.Transparent [ onClick CerrarDetalleDeItem ] [ text "Cerrar" ] ]
        }


viewAyudaModal : Maybe Ayuda -> Html Msg
viewAyudaModal ayuda =
    Bs.modal
        { isOpen = ayuda /= Nothing
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
            [ Bs.btn Bs.Transparent [ onClick CerrarAyuda ] [ text "Cerrar" ] ]
        }
