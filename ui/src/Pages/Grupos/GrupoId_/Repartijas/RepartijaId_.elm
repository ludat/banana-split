module Pages.Grupos.GrupoId_.Repartijas.RepartijaId_ exposing (Ayuda, Filtros, Model, Msg, page)

import Components.Bootstrap as Bs
import Css
import Effect exposing (Effect)
import Generated.Api as Api exposing (DistribucionDeSobras(..), Repartija, RepartijaClaim, RepartijaItem, ULID)
import Html exposing (Html, button, div, h5, h6, i, li, p, span, strong, text, ul)
import Html.Attributes as Attr exposing (class, classList, disabled, style, type_)
import Html.Events exposing (onClick)
import Layouts
import List.Extra exposing (find)
import Models.Grupo exposing (GrupoLike, lookupNombreParticipante)
import Models.Monto as Monto
import Models.Repartija as Repartija exposing (Delta(..), ItemRepartidoState(..), ParteDeParticipante)
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import Shared.Msg
import Utils.Http exposing (viewHttpError)
import Utils.Ulid exposing (emptyUlid)
import View exposing (View)


page : Shared.Model -> Route { grupoId : String, repartijaId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.grupoId route.params.repartijaId shared.store
        , update = update (Shared.currentParticipante shared route.params.grupoId) shared.store
        , subscriptions = subscriptions
        , view = view (Shared.currentParticipante shared route.params.grupoId) shared.repartijaIntroDismissed shared.store
        }
        |> Page.withLayout (\_ -> Layouts.Default_Grupo {})



-- INIT


type alias Model =
    { grupoId : ULID
    , repartijaId : ULID
    , pendingItemOperation : Maybe ULID
    , repartirModalItem : Maybe RepartijaItem
    , filtros : Filtros
    , participantesExpandido : Bool
    , alertaExpandida : Bool
    , ayuda : Maybe Ayuda
    }


type alias Filtros =
    { reclamadosPorVos : Bool
    , noResueltos : Bool
    , porParticipante : Maybe ULID
    }


type Ayuda
    = ComoFuncionaLaRepartija
    | ComoDividirUnItem


init : ULID -> ULID -> Store -> ( Model, Effect Msg )
init grupoId repartijaId store =
    ( { grupoId = grupoId
      , repartijaId = repartijaId
      , pendingItemOperation = Nothing
      , repartirModalItem = Nothing
      , filtros = { reclamadosPorVos = False, noResueltos = False, porParticipante = Nothing }
      , participantesExpandido = False
      , alertaExpandida = False
      , ayuda = Nothing
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
    | ChangeCurrentClaim RepartijaItem Int
    | JoinCurrentClaim RepartijaItem
    | LeaveCurrentClaim RepartijaClaim
    | CreateRepartijaClaimResponded (WebData Api.RepartijaForFrontend)
    | DeleteRepartijaClaimResponded ULID (WebData String)
    | ToggleFiltroReclamadosPorVos
    | ToggleFiltroNoResueltos
    | SetFiltroParticipante (Maybe ULID)
    | ToggleParticipantes
    | ToggleAlerta
    | AbrirAyuda Ayuda
    | CerrarAyuda
    | DismissIntro


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

        ToggleFiltroReclamadosPorVos ->
            let
                filtros =
                    model.filtros
            in
            ( { model | filtros = { filtros | reclamadosPorVos = not filtros.reclamadosPorVos } }
            , Effect.none
            )

        ToggleFiltroNoResueltos ->
            let
                filtros =
                    model.filtros
            in
            ( { model | filtros = { filtros | noResueltos = not filtros.noResueltos } }
            , Effect.none
            )

        SetFiltroParticipante participanteId ->
            let
                filtros =
                    model.filtros
            in
            ( { model | filtros = { filtros | porParticipante = participanteId } }
            , Effect.none
            )

        ToggleParticipantes ->
            ( { model | participantesExpandido = not model.participantesExpandido }
            , Effect.none
            )

        ToggleAlerta ->
            ( { model | alertaExpandida = not model.alertaExpandida }
            , Effect.none
            )

        AbrirAyuda ayuda ->
            ( { model | ayuda = Just ayuda }
            , Effect.none
            )

        CerrarAyuda ->
            ( { model | ayuda = Nothing }
            , Effect.none
            )

        DismissIntro ->
            ( model
            , Effect.sendSharedMsg Shared.Msg.DismissRepartijaIntro
            )

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


view : Maybe ULID -> Bool -> Store -> Model -> View Msg
view userId introDismissed store model =
    case ( Store.getRepartija model.repartijaId store, Store.getGrupo model.grupoId store ) of
        ( Success repartijaPage, Success grupo ) ->
            let
                repartija =
                    repartijaPage.repartija

                miParte =
                    userId |> Maybe.map (Repartija.parteDeParticipante repartija)

                -- Partes de la página que en mobile van en otro lugar que en
                -- desktop: se pintan dos veces y cada una se esconde en el
                -- tamaño que no le corresponde.
                participantes =
                    div []
                        [ linkDeAyuda ComoFuncionaLaRepartija "¿Cómo funciona la Repartija?"
                        , viewParticipantes model grupo repartija
                        ]

                alerta =
                    viewAlertaNoResueltos model repartija
            in
            { title = grupo.nombre ++ ": " ++ repartija.nombre
            , body =
                [ div [ class "row g-4" ]
                    [ div [ class "col-12 col-lg-8" ]
                        [ if introDismissed then
                            text ""

                          else
                            viewIntro
                        , div [ class "d-lg-none mb-4" ] [ participantes ]
                        , viewItemsHeader repartija
                        , div [ class "d-lg-none" ] [ alerta ]
                        , viewFiltros userId grupo model.filtros
                        , div [ class "d-none d-lg-block" ] [ alerta ]
                        , viewItems model userId miParte grupo repartija
                        ]
                    , div [ class "col-12 col-lg-4" ]
                        [ div [ class "d-none d-lg-block mb-3" ] [ participantes ]
                        , viewTotales miParte repartija
                        , viewDistribucionDeSobras repartija
                        ]
                    ]
                , viewRepartirModal model.repartirModalItem
                , viewAyudaModal model.ayuda
                ]
            }

        ( Failure e1, Failure e2 ) ->
            { title = ""
            , body =
                [ text "falle"
                , viewHttpError e1
                , viewHttpError e2
                ]
            }

        ( Failure e, _ ) ->
            { title = ""
            , body =
                [ text "falle"
                , viewHttpError e
                ]
            }

        ( _, Failure e ) ->
            { title = ""
            , body =
                [ text "falle"
                , viewHttpError e
                ]
            }

        _ ->
            { title = ""
            , body =
                [ text "cargando" ]
            }


linkDeAyuda : Ayuda -> String -> Html Msg
linkDeAyuda ayuda label =
    button
        [ type_ "button"
        , class "btn btn-link p-0 mb-2 text-decoration-none small d-inline-flex align-items-center gap-1"
        , onClick (AbrirAyuda ayuda)
        ]
        [ i [ class "bi bi-info-circle" ] []
        , text label
        ]


viewIntro : Html Msg
viewIntro =
    div [ class "alert alert-primary d-flex align-items-start gap-2 mb-4", Attr.attribute "role" "alert" ]
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


viewParticipantes : Model -> GrupoLike g -> Repartija -> Html Msg
viewParticipantes model grupo repartija =
    let
        ( conClaims, sinClaims ) =
            grupo.participantes
                |> List.partition (\p -> List.any (\c -> c.participante == p.id) repartija.claims)

        contador colorClass n label =
            div [ class "d-flex align-items-center gap-2 mb-2" ]
                [ Bs.badge colorClass [] [ text (String.fromInt n) ]
                , text label
                ]

        nombres participantes =
            participantes |> List.map .nombre |> String.join ", "
    in
    Bs.card []
        [ Bs.cardBody []
            [ div [ Css.repartija_label, class "mb-2" ] [ text "Participantes" ]
            , contador "bg-success-subtle text-success-emphasis" (List.length conClaims) "Ya reclamaron items"
            , if List.isEmpty conClaims then
                text ""

              else if model.participantesExpandido then
                div [ class "d-flex flex-wrap gap-2 mb-3" ]
                    (conClaims
                        |> List.map
                            (\p ->
                                button
                                    [ type_ "button"
                                    , class "btn btn-sm fw-semibold d-inline-flex align-items-center gap-2"
                                    , classList
                                        [ ( "btn-primary", model.filtros.porParticipante == Just p.id )
                                        , ( "btn-outline-primary", model.filtros.porParticipante /= Just p.id )
                                        ]
                                    , Attr.title ("Ver los items de " ++ p.nombre)
                                    , onClick (SetFiltroParticipante (Just p.id))
                                    ]
                                    [ text p.nombre
                                    , i [ class "bi bi-eye-fill" ] []
                                    ]
                            )
                    )

              else
                div [ class "text-primary fw-semibold mb-2" ] [ text (nombres conClaims) ]
            , if model.participantesExpandido then
                div []
                    [ contador "bg-secondary-subtle text-secondary-emphasis" (List.length sinClaims) "Aún no han reclamado items"
                    , if List.isEmpty sinClaims then
                        text ""

                      else
                        div [ class "fw-semibold mb-2" ] [ text (nombres sinClaims) ]
                    ]

              else
                text ""
            , div [ class "text-center" ]
                [ button
                    [ type_ "button"
                    , class "btn btn-link btn-sm text-decoration-none"
                    , onClick ToggleParticipantes
                    ]
                    (if model.participantesExpandido then
                        [ text "Ver menos ", i [ class "bi bi-caret-up-fill" ] [] ]

                     else
                        [ text "Ver más info ", i [ class "bi bi-caret-down-fill" ] [] ]
                    )
                ]
            ]
        ]


viewItemsHeader : Repartija -> Html Msg
viewItemsHeader repartija =
    let
        resueltos =
            repartija.items
                |> List.filter (Repartija.estaResuelto << Repartija.estadoDeItem repartija)
                |> List.length

        cantidad =
            List.length repartija.items
    in
    div [ class "d-flex align-items-center justify-content-between gap-2 mb-3" ]
        [ h5 [ class "mb-0" ] [ text "Items a repartir" ]
        , div [ class "d-flex align-items-center gap-2 small" ]
            [ text "Repartidos"
            , Bs.badge
                (if resueltos == cantidad then
                    "text-bg-success fs-6"

                 else
                    "text-bg-secondary fs-6"
                )
                []
                [ text (String.fromInt resueltos ++ " / " ++ String.fromInt cantidad) ]
            ]
        ]


viewAlertaNoResueltos : Model -> Repartija -> Html Msg
viewAlertaNoResueltos model repartija =
    if Repartija.hayItemsNoResueltos repartija then
        div [ class "mb-3" ]
            [ div [ class "rounded text-bg-warning mb-2" ]
                [ -- `text-reset`: el `.btn` trae su propio color (blanco en modo
                  -- oscuro) y tiene que heredar el de contraste del fondo.
                  button
                    [ type_ "button"
                    , class "btn text-reset w-100 d-flex align-items-center gap-2 fw-semibold text-start border-0"
                    , Attr.attribute "aria-expanded"
                        (if model.alertaExpandida then
                            "true"

                         else
                            "false"
                        )
                    , onClick ToggleAlerta
                    ]
                    [ i [ class "bi bi-exclamation-triangle-fill" ] []
                    , span [ class "flex-grow-1" ] [ text "Aún hay items no resueltos" ]
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
                    p [ class "px-3 pb-3 mb-0" ]
                        [ text "Para que esta repartija sea válida y pueda ser tomada en cuenta en las divisiones es necesario resolver cada situación presente en todos los items que tengan etiqueta de alerta." ]

                  else
                    text ""
                ]
            , linkDeAyuda ComoDividirUnItem "¿Cómo dividir un item?"
            ]

    else
        div [ class "mb-3" ] [ linkDeAyuda ComoDividirUnItem "¿Cómo dividir un item?" ]


viewFiltros : Maybe ULID -> GrupoLike g -> Filtros -> Html Msg
viewFiltros userId grupo filtros =
    let
        pillClass activo =
            classList
                [ ( "btn btn-sm rounded-pill d-inline-flex align-items-center gap-2 fw-semibold", True )
                , ( "btn-outline-primary", activo )
                , ( "btn-outline-secondary text-body", not activo )
                ]

        checkbox activo =
            i
                [ class
                    (if activo then
                        "bi bi-check-square-fill text-primary"

                     else
                        "bi bi-square"
                    )
                ]
                []

        porParticipante =
            div [ class "dropdown" ]
                [ button
                    [ type_ "button"
                    , pillClass (filtros.porParticipante /= Nothing)
                    , Attr.attribute "data-bs-toggle" "dropdown"
                    , Attr.attribute "aria-expanded" "false"
                    ]
                    [ i [ class "bi bi-search" ] []
                    , text
                        (case filtros.porParticipante of
                            Just participanteId ->
                                "Por participante: " ++ lookupNombreParticipante grupo participanteId

                            Nothing ->
                                "Por participante"
                        )
                    ]
                , ul [ class "dropdown-menu shadow" ]
                    ((grupo.participantes
                        |> List.map
                            (\p ->
                                li []
                                    [ button
                                        [ type_ "button"
                                        , class "dropdown-item"
                                        , classList [ ( "active", filtros.porParticipante == Just p.id ) ]
                                        , onClick (SetFiltroParticipante (Just p.id))
                                        ]
                                        [ text p.nombre ]
                                    ]
                            )
                     )
                        ++ (case filtros.porParticipante of
                                Just _ ->
                                    [ li [] [ Html.hr [ class "dropdown-divider" ] [] ]
                                    , li []
                                        [ button
                                            [ type_ "button"
                                            , class "dropdown-item"
                                            , onClick (SetFiltroParticipante Nothing)
                                            ]
                                            [ i [ class "bi bi-x-lg me-2" ] []
                                            , text "Quitar filtro"
                                            ]
                                        ]
                                    ]

                                Nothing ->
                                    []
                           )
                    )
                ]
    in
    Bs.card [ class "mb-3" ]
        [ Bs.cardBody []
            [ div [ Css.repartija_label, class "mb-2" ] [ text "Filtrar" ]
            , div [ class "d-flex flex-wrap gap-2" ]
                [ case userId of
                    Just _ ->
                        button
                            [ type_ "button"
                            , pillClass filtros.reclamadosPorVos
                            , onClick ToggleFiltroReclamadosPorVos
                            ]
                            [ checkbox filtros.reclamadosPorVos
                            , text "Reclamados por vos"
                            ]

                    Nothing ->
                        text ""
                , button
                    [ type_ "button"
                    , pillClass filtros.noResueltos
                    , onClick ToggleFiltroNoResueltos
                    ]
                    [ checkbox filtros.noResueltos
                    , i [ class "bi bi-exclamation-triangle-fill text-warning" ] []
                    , text "No resueltos"
                    ]
                , porParticipante
                ]
            ]
        ]


cumpleFiltros : Maybe ULID -> Filtros -> Repartija -> RepartijaItem -> Bool
cumpleFiltros userId filtros repartija item =
    let
        claims =
            Repartija.claimsDeItem repartija item

        reclamadoPor participanteId =
            List.any (\c -> c.participante == participanteId) claims
    in
    (not filtros.reclamadosPorVos || (userId |> Maybe.map reclamadoPor |> Maybe.withDefault False))
        && (not filtros.noResueltos || not (Repartija.estaResuelto (Repartija.estadoDeItem repartija item)))
        && (filtros.porParticipante |> Maybe.map reclamadoPor |> Maybe.withDefault True)


viewItems : Model -> Maybe ULID -> Maybe ParteDeParticipante -> GrupoLike g -> Repartija -> Html Msg
viewItems model userId miParte grupo repartija =
    case List.filter (cumpleFiltros userId model.filtros repartija) repartija.items of
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
                    |> List.map (viewItemCard model userId miParte grupo repartija)
                )


unidades : Int -> String
unidades n =
    if n == 1 then
        "1 unidad"

    else
        String.fromInt n ++ " unidades"


viewItemCard : Model -> Maybe ULID -> Maybe ParteDeParticipante -> GrupoLike g -> Repartija -> RepartijaItem -> Html Msg
viewItemCard model userId miParte grupo repartija item =
    let
        claims =
            Repartija.claimsDeItem repartija item

        estado =
            Repartija.estadoDeItem repartija item

        userClaim =
            claims |> find (\c -> Just c.participante == userId)

        pending =
            model.pendingItemOperation == Just item.id || userId == Nothing

        boton variant texto msg habilitado =
            button
                [ type_ "button"
                , class ("btn btn-sm px-3 " ++ variant)
                , disabled (pending || not habilitado)
                , onClick msg
                ]
                [ text texto ]

        area nombre children =
            div [ style "grid-area" nombre ] children

        label txt =
            div [ Css.repartija_label, class "mb-1" ] [ text txt ]

        header =
            div [ class "card-header repartija-item-header" ]
                [ div [ class "d-flex align-items-start justify-content-between gap-2" ]
                    [ h6 [ class "fs-5 mb-0" ] [ text item.nombre ]
                    , if estado == SinRepartir then
                        Bs.badge "text-bg-warning text-nowrap" [] [ text "Aún no repartido" ]

                      else
                        text ""
                    ]
                , div [ class "small fw-semibold d-flex align-items-center gap-2 text-nowrap" ]
                    [ text ("CANTIDAD: " ++ String.fromInt item.cantidad)
                    , span [ class "text-body-tertiary" ] [ text "|" ]
                    , text ("TOTAL: $" ++ Monto.toString item.monto)
                    ]
                ]

        body =
            case estado of
                SinRepartir ->
                    div [ class "card-body text-center" ]
                        [ boton "btn-primary" "Repartir" (OpenRepartirModal item) True ]

                _ ->
                    let
                        reparto =
                            area "reparto"
                                [ label "Reparto"
                                , text
                                    (case estado of
                                        RepartidoExactamente _ ->
                                            "Por cantidad"

                                        RepartidoEquitativamenteEntre _ ->
                                            "Equitativo"

                                        _ ->
                                            "Mixto"
                                    )
                                ]

                        estadoDeReparto =
                            case estado of
                                RepartidoExactamente { cantidadReclamada, deltaDeCantidad } ->
                                    area "estado"
                                        [ label "Repartidos"
                                        , viewClaimsDropdown grupo
                                            claims
                                            (if deltaDeCantidad == 0 then
                                                "text-bg-success"

                                             else
                                                "text-bg-warning"
                                            )
                                            (String.fromInt cantidadReclamada ++ " / " ++ String.fromInt item.cantidad)
                                        ]

                                RepartidoEquitativamenteEntre { cantidadDeParticipantes } ->
                                    area "estado"
                                        [ label "Dividido entre"
                                        , viewClaimsDropdown grupo
                                            claims
                                            (if cantidadDeParticipantes >= 2 then
                                                "text-bg-success"

                                             else
                                                "text-bg-warning"
                                            )
                                            (String.fromInt cantidadDeParticipantes)
                                        ]

                                RepartidoIncorrectamente { cantidadDeParticipantes } ->
                                    area "estado"
                                        [ label "Repartido entre"
                                        , viewClaimsDropdown grupo claims "text-bg-danger" (String.fromInt cantidadDeParticipantes)
                                        ]

                                SinRepartir ->
                                    text ""

                        tieneCantidad =
                            userClaim |> Maybe.andThen .cantidad |> (/=) Nothing

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

                        reclamar =
                            area "reclamar"
                                [ label "Reclamar"
                                , case estado of
                                    RepartidoExactamente _ ->
                                        div [ class "d-flex justify-content-center gap-2" ]
                                            [ boton "btn-primary" "+1" (ChangeCurrentClaim item 1) True
                                            , boton "btn-dark" "-1" (ChangeCurrentClaim item -1) tieneCantidad
                                            ]

                                    RepartidoEquitativamenteEntre _ ->
                                        if participaEquitativo then
                                            boton "btn-dark" "Salirse" salirse True

                                        else
                                            boton "btn-primary" "Participar" (JoinCurrentClaim item) True

                                    RepartidoIncorrectamente _ ->
                                        div [ class "d-flex flex-column align-items-center gap-2" ]
                                            [ if participaEquitativo then
                                                boton "btn-dark" "Salirse" salirse True

                                              else
                                                boton "btn-primary" "Participar" (JoinCurrentClaim item) True
                                            , div [ class "d-flex justify-content-center gap-2" ]
                                                [ boton "btn-primary" "Sumar 1" (ChangeCurrentClaim item 1) True
                                                , boton "btn-dark" "Restar 1" (ChangeCurrentClaim item -1) tieneCantidad
                                                ]
                                            ]

                                    SinRepartir ->
                                        text ""
                                ]

                        participacion =
                            case userClaim of
                                Just claim ->
                                    let
                                        miConsumo =
                                            miParte
                                                |> Maybe.andThen (\parte -> parte.consumoPorItem |> find (\( consumido, _ ) -> consumido.id == item.id))
                                                |> Maybe.map Tuple.second
                                                |> Maybe.withDefault Monto.zero
                                    in
                                    area "participacion"
                                        [ div [ class "small" ]
                                            [ case claim.cantidad of
                                                Just n ->
                                                    span []
                                                        [ text "Reclamaste "
                                                        , span [ class "text-primary" ] [ text (unidades n) ]
                                                        ]

                                                Nothing ->
                                                    span [ class "text-primary" ] [ text "Estás participando" ]
                                            ]
                                        , Html.hr [ class "my-1" ] []
                                        , div [ class "small" ]
                                            [ text "Tu consumo "
                                            , span [ class "text-danger" ] [ text ("$ " ++ Monto.toString miConsumo) ]
                                            ]
                                        ]

                                Nothing ->
                                    text ""
                    in
                    div [ class "card-body repartija-item-body" ]
                        [ reparto, estadoDeReparto, reclamar, participacion ]

        advertencia color icono txt =
            span [ class ("d-inline-flex align-items-center gap-1 " ++ color) ]
                [ i [ class ("bi " ++ icono) ] [], text txt ]

        advertencias =
            List.filterMap identity
                [ if userClaim /= Nothing then
                    Just (advertencia "text-primary" "bi-check-lg" "Reclamado")

                  else
                    Nothing
                , case estado of
                    RepartidoExactamente { deltaDeCantidad } ->
                        case Repartija.compararConCero deltaDeCantidad of
                            QuedaCortoPor n ->
                                Just (advertencia "text-warning" "bi-exclamation-triangle-fill" ("Falta repartir " ++ unidades n))

                            SePasaPor n ->
                                Just (advertencia "text-warning" "bi-exclamation-triangle-fill" ("Sobran " ++ unidades n))

                            ExactamenteCero ->
                                Nothing

                    RepartidoEquitativamenteEntre { cantidadDeParticipantes } ->
                        if cantidadDeParticipantes < 2 then
                            Just
                                (advertencia "text-warning"
                                    "bi-exclamation-triangle-fill"
                                    (if userClaim /= Nothing then
                                        "Solo hay 1 persona"

                                     else
                                        "Solo hay 1 persona en esta división"
                                    )
                                )

                        else
                            Nothing

                    RepartidoIncorrectamente _ ->
                        Just (advertencia "text-danger" "bi-exclamation-triangle-fill" "Repartido incorrectamente")

                    SinRepartir ->
                        Nothing
                ]

        footer =
            div [ class "card-footer bg-transparent d-flex align-items-center justify-content-between gap-2 small" ]
                [ viewAcciones userId claims userClaim item
                , div [ class "d-flex flex-wrap justify-content-end column-gap-3" ] advertencias
                ]
    in
    div
        [ Css.repartija_item
        , class "card mb-3"
        , classList [ ( "border-primary", userClaim /= Nothing ) ]
        ]
        [ header, body, footer ]


{-| El badge con el estado del reparto ("5 / 10", "2") que al tocarlo despliega
quién reclamó qué.
-}
viewClaimsDropdown : GrupoLike g -> List RepartijaClaim -> String -> String -> Html Msg
viewClaimsDropdown grupo claims colorClass label =
    div [ class "dropdown d-inline-block" ]
        [ button
            [ type_ "button"
            , class ("btn btn-sm fw-semibold py-0 d-inline-flex align-items-center gap-2 " ++ colorClass)
            , Attr.attribute "data-bs-toggle" "dropdown"
            , Attr.attribute "aria-expanded" "false"
            , Attr.title "Ver quiénes reclamaron"
            ]
            [ text label, i [ class "bi bi-eye-fill" ] [] ]
        , ul [ class "dropdown-menu shadow" ]
            (claims
                |> List.map
                    (\claim ->
                        li []
                            [ div [ class "dropdown-item-text d-flex justify-content-between gap-3" ]
                                [ span [] [ text (lookupNombreParticipante grupo claim.participante) ]
                                , span [ class "text-muted small" ]
                                    [ text
                                        (case claim.cantidad of
                                            Just cantidad ->
                                                unidades cantidad

                                            Nothing ->
                                                "Equitativo"
                                        )
                                    ]
                                ]
                            ]
                    )
            )
        ]


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
            , class "btn btn-link btn-sm p-0 text-body text-decoration-none dropdown-toggle"
            , Attr.attribute "data-bs-toggle" "dropdown"
            , Attr.attribute "aria-expanded" "false"
            ]
            [ text "Acciones" ]
        , ul [ class "dropdown-menu shadow" ]
            (if List.isEmpty acciones then
                [ li [] [ span [ class "dropdown-item-text text-muted small" ] [ text "No hay acciones disponibles" ] ] ]

             else
                acciones
            )
        ]


viewTotales : Maybe ParteDeParticipante -> Repartija -> Html Msg
viewTotales miParte repartija =
    let
        fila attrs { label, monto, extra, tuParte } =
            div (class "card mb-2" :: attrs)
                [ div [ class "card-body py-2 d-flex align-items-center flex-wrap column-gap-3" ]
                    [ span [ class "fw-semibold", style "min-width" "4.5rem" ] [ text label ]
                    , span [ class "flex-grow-1 d-inline-flex align-items-center gap-2" ]
                        (text ("$" ++ Monto.toString monto) :: extra)
                    , case tuParte of
                        Just parte ->
                            span [ class "small" ] parte

                        Nothing ->
                            text ""
                    ]
                ]

        explicacionPropina =
            case repartija.distribucionDeSobras of
                SobrasProporcional ->
                    "La propina, junto con los items que nadie reclamó, se reparte en proporción a lo que consumió cada uno."

                SobrasNoDistribuir ->
                    "La propina se reparte en proporción a lo que consumió cada uno."
    in
    div [ class "mb-4" ]
        [ fila []
            { label = "Propina"
            , monto = repartija.extra
            , extra =
                [ i
                    [ class "bi bi-info-circle text-primary"
                    , Attr.title explicacionPropina
                    , Attr.attribute "aria-label" explicacionPropina
                    ]
                    []
                ]
            , tuParte =
                miParte
                    |> Maybe.map
                        (\parte ->
                            [ text "Tu parte "
                            , span [ class "text-danger" ] [ text ("$ " ++ Monto.toString parte.propina) ]
                            ]
                        )
            }
        , fila [ class "text-bg-dark fs-5 fw-bold" ]
            { label = "Total"
            , monto = Repartija.total repartija
            , extra = []
            , tuParte =
                miParte
                    |> Maybe.map
                        (\parte ->
                            [ text ("Tu parte $ " ++ Monto.toString parte.total) ]
                        )
            }
        ]


viewDistribucionDeSobras : Repartija -> Html Msg
viewDistribucionDeSobras repartija =
    div []
        [ h5 [] [ text "Distribución de items no reclamados" ]
        , case repartija.distribucionDeSobras of
            SobrasProporcional ->
                p []
                    [ strong [] [ text "Proporcional: " ]
                    , text "Los items que nadie reclamó se dividirán de manera proporcional entre todos los participantes que hayan reclamado items."
                    ]

            SobrasNoDistribuir ->
                p []
                    [ strong [] [ text "No se distribuyen: " ]
                    , text "Los items que nadie reclamó no se le cobran a ningún participante, quedan a cargo de quienes pagaron."
                    ]
        ]


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

                _ ->
                    "¿Cómo funciona la Repartija?"
        , body =
            case ayuda of
                Just ComoFuncionaLaRepartija ->
                    [ p [] [ text "La repartija divide un gasto según lo que consumió cada uno. Cada participante busca en la lista los items en los que participó y los reclama." ]
                    , p [] [ text "El primero en reclamar un item decide cómo se reparte: por cantidad o en partes iguales. Los demás se suman a esa forma de reparto." ]
                    , p [] [ text "La propina se reparte en proporción a lo que consumió cada uno. Abajo vas a ver cuánto te toca a vos." ]
                    , p [ class "mb-0" ] [ text "Los items con etiqueta de alerta todavía no están bien repartidos: revisalos hasta que no quede ninguno." ]
                    ]

                Just ComoDividirUnItem ->
                    [ h6 [ class "fw-semibold" ] [ text "Por cantidad" ]
                    , p [] [ text "Cada participante indica cuántas unidades consumió con los botones +1 y -1. El item queda resuelto cuando entre todos se reclaman exactamente las unidades que hay." ]
                    , h6 [ class "fw-semibold" ] [ text "División equitativa" ]
                    , p [] [ text "El total del item se divide en partes iguales entre quienes tocaron \"Participar\". Hacen falta al menos dos personas para que quede resuelto." ]
                    , h6 [ class "fw-semibold" ] [ text "Repartido incorrectamente" ]
                    , p [ class "mb-0" ] [ text "Si un item tiene reclamos por cantidad y equitativos a la vez no se puede repartir: pónganse de acuerdo en una sola forma." ]
                    ]

                Nothing ->
                    []
        , footer =
            [ Bs.btn Bs.Transparent [ onClick CerrarAyuda ] [ text "Cerrar" ] ]
        }
