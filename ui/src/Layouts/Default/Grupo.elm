module Layouts.Default.Grupo exposing (Model, Msg, Props, layout)

import Components.Bootstrap as Bs
import Components.PagoDetalleModal as PagoDetalleModal
import Css
import Dict
import Effect exposing (Effect)
import Generated.Api exposing (Grupo, ULID, User)
import Html exposing (Html, a, button, div, h2, i, label, li, node, option, p, select, span, text, ul)
import Html.Attributes as Attr exposing (class, classList, selected, style, type_, value)
import Html.Events exposing (on, onClick, preventDefaultOn)
import Json.Decode as Decode
import Layout exposing (Layout)
import Layouts.Default
import Models.Grupo exposing (GrupoLike, currentParticipante, estaCongelado, grupoIdFromPath, ownedParticipante)
import Models.Repartija as Repartija
import Models.Store as Store
import Models.Store.Types exposing (Store)
import QRCode
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared.Model as Shared
import Shared.Msg as Shared
import Svg.Attributes as SvgAttr
import View exposing (View)


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout Layouts.Default.Props Model Msg contentMsg
layout _ shared route =
    Layout.new
        { init = \() -> init route
        , update = update (PagoDetalleModal.context shared route) shared.store
        , view = view shared.store route.path shared.participanteId shared.origin shared.currentUser
        , subscriptions = subscriptions
        }
        |> Layout.withParentProps {}
        |> Layout.withOnUrlChanged (PagoModalMsg << PagoDetalleModal.onUrlChanged)



-- MODEL


type alias Model =
    { qrShare : Maybe { title : String, url : String }
    , pagoModal : PagoDetalleModal.Model
    }


init : Route () -> ( Model, Effect Msg )
init route =
    let
        ( pagoModal, modalEffect ) =
            PagoDetalleModal.init route
    in
    ( { qrShare = Nothing
      , pagoModal = pagoModal
      }
    , Effect.map PagoModalMsg modalEffect
    )



-- UPDATE


type Msg
    = ForwardSharedMessage Shared.Msg
    | ShareUrl { title : String, url : String }
    | OpenQrShare { title : String, url : String }
    | CloseQrShare
    | PagoModalMsg PagoDetalleModal.Msg


update : PagoDetalleModal.Context -> Store -> Msg -> Model -> ( Model, Effect Msg )
update ctx store msg model =
    case msg of
        PagoModalMsg subMsg ->
            let
                ( pagoModal, eff ) =
                    PagoDetalleModal.update ctx store subMsg model.pagoModal
            in
            ( { model | pagoModal = pagoModal }, Effect.map PagoModalMsg eff )

        ForwardSharedMessage sharedMsg ->
            ( model
            , Effect.sendSharedMsg sharedMsg
            )

        ShareUrl data ->
            ( model
            , Effect.share data
            )

        OpenQrShare data ->
            ( { model | qrShare = Just data }
            , Effect.none
            )

        CloseQrShare ->
            ( { model | qrShare = Nothing }
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view :
    Store
    -> Path.Path
    -> Maybe ULID
    -> String
    -> WebData User
    -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model }
    -> View contentMsg
view store currentPath manualPick origin currentUser { toContentMsg, model, content } =
    let
        remoteGrupo =
            grupoIdFromPath currentPath
                |> Maybe.map (\grupoId -> Store.getGrupo grupoId store)
                |> Maybe.withDefault NotAsked

        -- The participante to act as: the manual pick, or the participante the
        -- logged-in account owns in this grupo (derived, never persisted).
        activeUser =
            case remoteGrupo of
                Success grupo ->
                    currentParticipante manualPick currentUser grupo

                _ ->
                    manualPick
    in
    { title = content.title
    , body =
        [ case remoteGrupo of
            Success grupo ->
                node "pwa-manifest"
                    [ Attr.attribute "grupo-id" grupo.id
                    , Attr.attribute "nombre" grupo.nombre
                    ]
                    []

            _ ->
                text ""
        , case remoteGrupo of
            Success grupo ->
                Html.map toContentMsg <|
                    viewGroupHeader origin currentPath activeUser currentUser store grupo

            _ ->
                text ""
        , Html.map toContentMsg <|
            viewQrModal model.qrShare
        , case remoteGrupo of
            Success grupo ->
                Html.map (toContentMsg << PagoModalMsg) <|
                    PagoDetalleModal.view store grupo model.pagoModal

            _ ->
                text ""
        , case ( activeUser, remoteGrupo ) of
            ( Nothing, Success grupo ) ->
                if List.isEmpty grupo.participantes then
                    div [ class "container-fluid py-3" ] content.body

                else
                    Html.map toContentMsg <|
                        div [ class "container-fluid py-5 text-center" ]
                            [ p [ class "mb-3" ] [ text "Por favor seleccioná quién sos para comenzar:" ]
                            , div [ class "mx-auto", style "max-width" "20rem" ]
                                [ viewGlobalUserSelector activeUser grupo ]
                            ]

            _ ->
                div [ class "container-fluid py-3" ] content.body
        ]
    }


viewGroupHeader : String -> Path.Path -> Maybe ULID -> WebData User -> Store -> Grupo -> Html Msg
viewGroupHeader origin currentPath activeUser currentUser store grupo =
    let
        info =
            headerInfo currentPath store currentUser grupo

        share =
            { title = info.share.title
            , url = origin ++ Path.toString info.share.path
            }
    in
    div [ class "border-bottom" ]
        [ div [ class "container-fluid py-3" ]
            [ div [ class "d-flex flex-wrap align-items-start justify-content-between gap-3" ]
                [ div []
                    [ viewBreadcrumb info.breadcrumb
                    , h2 [ class "mb-0 fw-bold d-flex align-items-center gap-2" ]
                        (viewIconoDelTitulo currentPath
                            :: text info.title
                            :: viewAlertaDelTitulo currentPath store
                        )
                    , viewBadgeCongelado grupo
                    ]

                -- Solo desktop. En mobile el "Ver como" baja a su propia banda
                -- y las acciones se van a la navbar de abajo: compartir al menú
                -- "Más", y agregar gasto al botón del medio.
                , div [ class "d-none d-md-flex flex-column align-items-end gap-2" ]
                    [ label [ class "d-flex align-items-center gap-2 small text-muted text-nowrap" ]
                        [ text "Ver como:"
                        , viewGlobalUserSelector activeUser grupo
                        ]
                    , viewVerComoWarning currentUser activeUser grupo
                    , div [ class "d-flex flex-wrap align-items-center gap-2" ]
                        [ viewShareDropdown share

                        -- Fuera de las secciones con tabs (la repartija) se
                        -- está mirando un gasto puntual, no cargando gastos.
                        , if info.showTabs then
                            a
                                [ class "btn btn-primary d-inline-flex align-items-center"
                                , PagoDetalleModal.hrefNuevoGasto currentPath
                                ]
                                [ i [ class "bi bi-plus-lg me-1" ] []
                                , text "Agregar gasto"
                                ]

                          else
                            text ""
                        ]
                    ]
                ]
            ]

        -- La banda de "Ver como" de mobile: separada del título por su propio
        -- borde, como en el diseño.
        , div [ class "d-md-none border-top" ]
            [ div [ class "container-fluid py-2 d-flex align-items-center flex-wrap gap-2" ]
                [ label [ class "d-flex align-items-center gap-2 small text-muted text-nowrap mb-0" ]
                    [ text "Ver como:"
                    , viewGlobalUserSelector activeUser grupo
                    ]
                , viewVerComoWarning currentUser activeUser grupo

                -- Compartir vive en el menú "Más" de la navbar inferior, pero
                -- esa navbar solo existe en las secciones con tabs. Donde no
                -- está —la repartija, que es justamente la que más se
                -- comparte— el botón se queda acá para no perderlo.
                , if info.showTabs then
                    text ""

                  else
                    div [ class "ms-auto" ] [ viewShareDropdown share ]
                ]
            ]
        , if info.showTabs then
            div [ class "container-fluid d-none d-md-block" ]
                [ viewTabNav currentPath grupo ]

          else
            text ""
        , if info.showTabs then
            viewBottomNav currentPath share grupo

          else
            text ""
        ]


viewIconoDelTitulo : Path.Path -> Html Msg
viewIconoDelTitulo currentPath =
    case currentPath of
        Path.Grupos_GrupoId__Repartijas_RepartijaId_ _ ->
            span
                [ class "d-inline-flex align-items-center justify-content-center rounded-circle bg-body-secondary fs-5 flex-shrink-0"
                , style "width" "2.5rem"
                , style "height" "2.5rem"
                , Attr.attribute "aria-hidden" "true"
                ]
                [ i [ class "bi bi-people-fill" ] [] ]

        _ ->
            text ""


{-| La alerta al lado del título de la repartija cuando todavía hay items que
no están bien repartidos.
-}
viewAlertaDelTitulo : Path.Path -> Store -> List (Html Msg)
viewAlertaDelTitulo currentPath store =
    case currentPath of
        Path.Grupos_GrupoId__Repartijas_RepartijaId_ params ->
            case Store.getRepartija params.repartijaId store of
                Success repartijaPage ->
                    if Repartija.hayItemsNoResueltos repartijaPage.repartija then
                        [ i
                            [ class "bi bi-exclamation-triangle-fill text-warning fs-4"
                            , Attr.title "Aún hay items no resueltos"
                            , Attr.attribute "aria-label" "Aún hay items no resueltos"
                            ]
                            []
                        ]

                    else
                        []

                _ ->
                    []

        _ ->
            []


viewBadgeCongelado : Grupo -> Html Msg
viewBadgeCongelado grupo =
    if estaCongelado grupo then
        div [ class "mt-2" ]
            [ Bs.badge "d-inline-flex align-items-center gap-1 text-uppercase text-white"
                [ Bs.fondoCongelado ]
                [ i [ class "bi bi-cash-stack" ] []
                , text "Saldando deudas"
                ]
            ]

    else
        text ""


{-| Inline warning shown next to the "Ver como" toggle when a logged-in user is
acting as a participante that isn't theirs. If that participante is unclaimed we
offer a quick "reclamar"; if it's taken by someone else we just warn.
-}
viewVerComoWarning : WebData User -> Maybe ULID -> Grupo -> Html Msg
viewVerComoWarning currentUser activeUser grupo =
    case ( currentUser, activeUser ) of
        ( Success u, Just uid ) ->
            case grupo.participantes |> List.filter (\p -> p.id == uid) |> List.head of
                Just p ->
                    if (p.user |> Maybe.map .id) == Just u.id then
                        text ""

                    else
                        let
                            -- Only offer a quick "reclamar" when the participante
                            -- is unclaimed AND the user doesn't already own one in
                            -- the grupo (a user owns at most one).
                            canClaim =
                                p.user == Nothing && ownedParticipante u.id grupo == Nothing
                        in
                        div [ class "d-flex align-items-center gap-1 text-warning small" ]
                            (i [ class "bi bi-exclamation-triangle" ] []
                                :: text "No sos vos"
                                :: (if canClaim then
                                        [ button
                                            [ type_ "button"
                                            , class "btn btn-sm btn-link p-0 text-decoration-none align-baseline"
                                            , onClick
                                                (ForwardSharedMessage <|
                                                    Shared.ClaimParticipante { grupoId = grupo.id, participanteId = p.id }
                                                )
                                            ]
                                            [ text "reclamar" ]
                                        ]

                                    else
                                        []
                                   )
                            )

                Nothing ->
                    text ""

        _ ->
            text ""


{-| Adónde te devuelve el "‹". Es uno solo: no un camino completo desde la raíz,
sino un salto para atrás. En casi todas las secciones es el grupo, en la
repartija es el gasto, y en el resumen del grupo es la lista de grupos.

El `label` de una entidad lleva el tipo adelante —"Grupo: Salidita de Jueves"—
para que no haya que adivinar qué es ese nombre.

Lleva una ruta entera y no un path pelado porque la del gasto apunta al popup,
que vive en la query.

-}
type alias Crumb =
    { label : String, ruta : PagoDetalleModal.Ruta }


{-| Qué te ofrece el breadcrumb. Casi siempre volver a la entidad de la que
colgás, pero en el resumen de un grupo depende de si hay sesión: sin sesión no
hay grupos que listar, así que mandar a alguien a "Tus grupos" lo deja mirando
una página vacía, y en ese caso ofrece tener una cuenta.

Va envuelto en un `Maybe` donde `Nothing` es "todavía no sé cuál de las dos" —
falta el gasto de la repartija, o falta saber si hay sesión—. Eso no es una
acción, así que no entra como un caso más acá adentro.

-}
type AccionDelBreadcrumb
    = Volver Crumb
    | InvitarASesion PagoDetalleModal.Ruta


sinQuery : Path.Path -> PagoDetalleModal.Ruta
sinQuery path =
    { path = path, query = Dict.empty, hash = Nothing }


{-| El login con el `redirect` puesto, para volver a donde estabas. Es el mismo
parámetro que usa el "Iniciar sesión" de la navbar.
-}
rutaDeLogin : Path.Path -> PagoDetalleModal.Ruta
rutaDeLogin volverA =
    { path = Path.Login
    , query = Dict.singleton "redirect" (Path.toString volverA)
    , hash = Nothing
    }


{-| Computes everything the group header needs from the current path and store:
de qué entidad colgás, the big `h2` title, and whether the section tabs should
be shown (only on top-level sections).

Entity names are resolved from the store, falling back to `"Cargando..."` while the
data is still loading.

-}
headerInfo :
    Path.Path
    -> Store
    -> WebData User
    -> Grupo
    -> { breadcrumb : Maybe AccionDelBreadcrumb, title : String, showTabs : Bool, share : { title : String, path : Path.Path } }
headerInfo currentPath store currentUser grupo =
    let
        -- De acá cuelgan todas las secciones del grupo. La única que no es
        -- esta es la repartija, que cuelga de su gasto.
        volverAlGrupo : Maybe AccionDelBreadcrumb
        volverAlGrupo =
            Just <|
                Volver
                    { label = "Grupo: " ++ grupo.nombre
                    , ruta = sinQuery (Path.Grupos_Id_ { id = grupo.id })
                    }

        grupoShare =
            { title = grupo.nombre, path = Path.Grupos_Id_ { id = grupo.id } }
    in
    case currentPath of
        Path.Grupos_GrupoId__Gastos _ ->
            { breadcrumb = volverAlGrupo
            , title = "Gastos"
            , showTabs = True
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Transferencias _ ->
            { breadcrumb = volverAlGrupo
            , title = "Transferencias"
            , showTabs = True
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Liquidaciones _ ->
            { breadcrumb = volverAlGrupo
            , title = "Transferencias"
            , showTabs = True
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Participantes _ ->
            { breadcrumb = volverAlGrupo
            , title = "Participantes"
            , showTabs = True
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Settings _ ->
            { breadcrumb = volverAlGrupo
            , title = "Ajustes"
            , showTabs = True
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Repartijas_RepartijaId_ params ->
            let
                maybeRepartija =
                    Store.getRepartija params.repartijaId store
                        |> RemoteData.toMaybe

                pagoNombre =
                    maybeRepartija
                        |> Maybe.map .pagoNombre
                        |> Maybe.withDefault "Cargando"
            in
            { -- La repartija es lo único que no cuelga del grupo: cuelga del
              -- gasto que la generó, y ahí es adonde tenés que poder volver.
              -- Mientras el gasto no cargó todavía no hay a dónde apuntar.
              breadcrumb =
                maybeRepartija
                    |> Maybe.map
                        (\r ->
                            Volver
                                { label = "Gasto: " ++ pagoNombre
                                , ruta =
                                    PagoDetalleModal.rutaGasto
                                        (Path.Grupos_GrupoId__Gastos { grupoId = grupo.id })
                                        r.pagoId
                                }
                        )
            , title = "Repartija"
            , showTabs = False
            , share = { title = "Deudores de " ++ pagoNombre, path = Path.Grupos_GrupoId__Repartijas_RepartijaId_ params }
            }

        -- Rutas viejas: redirigen apenas se montan, así que nunca llegan a
        -- pintar chrome. Están acá solo para que el case sea exhaustivo.
        Path.Grupos_GrupoId__Gastos_New _ ->
            { breadcrumb = volverAlGrupo
            , title = "Nuevo gasto"
            , showTabs = False
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Gastos_GastoId_ _ ->
            { breadcrumb = volverAlGrupo
            , title = "Cargando..."
            , showTabs = False
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Pagos _ ->
            { breadcrumb = volverAlGrupo
            , title = "Gastos"
            , showTabs = True
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Pagos_New _ ->
            { breadcrumb = volverAlGrupo
            , title = "Nuevo gasto"
            , showTabs = False
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Pagos_PagoId_ _ ->
            { breadcrumb = volverAlGrupo
            , title = "Cargando..."
            , showTabs = False
            , share = { path = currentPath, title = grupo.nombre }
            }

        -- El grupo no cuelga de otra entidad, cuelga de tu lista de grupos. Y
        -- el título ya es el nombre del grupo, así que repetirlo acá arriba no
        -- diría nada nuevo.
        --
        -- Sin sesión esa lista está vacía, así que en vez del link va la
        -- invitación a tener una cuenta. Y mientras el pedido está en vuelo
        -- todavía no se sabe cuál de las dos corresponde.
        Path.Grupos_Id_ _ ->
            { breadcrumb =
                case currentUser of
                    Success _ ->
                        Just <| Volver { label = "Tus grupos", ruta = sinQuery Path.Home_ }

                    Failure _ ->
                        Just <| InvitarASesion (rutaDeLogin currentPath)

                    NotAsked ->
                        Just <| InvitarASesion (rutaDeLogin currentPath)

                    Loading ->
                        Nothing
            , title = grupo.nombre
            , showTabs = True
            , share = grupoShare
            }

        Path.NotFound_ ->
            { breadcrumb = Nothing
            , title = grupo.nombre
            , showTabs = False
            , share = grupoShare
            }

        Path.Home_ ->
            { breadcrumb = Nothing
            , title = "Banana split"
            , showTabs = False
            , share = grupoShare
            }

        Path.Login ->
            { breadcrumb = Nothing
            , title = "Banana split"
            , showTabs = False
            , share = grupoShare
            }

        Path.Cuenta ->
            { breadcrumb = Nothing
            , title = "Banana split"
            , showTabs = False
            , share = grupoShare
            }


{-| La vuelta a la entidad de la que colgás, con forma de "‹ Grupo: Salidita de
Jueves". No es un camino desde la raíz sino un solo salto para atrás, así que se
pinta como un link y no como una lista de migajas.

La invitación a tener cuenta ocupa el mismo lugar pero no lleva el "‹": no te
devuelve a ningún lado, te ofrece algo.

-}
viewBreadcrumb : Maybe AccionDelBreadcrumb -> Html Msg
viewBreadcrumb accion =
    case accion of
        -- Mientras no se sabe qué ofrecer no va nada: un placeholder acá
        -- movería el título para abajo y después lo acomodaría, que molesta
        -- más que esperar.
        Nothing ->
            text ""

        Just (Volver crumbDeVuelta) ->
            Html.nav [ Attr.attribute "aria-label" "breadcrumb", class "mb-1" ]
                [ Html.a
                    [ Route.href crumbDeVuelta.ruta
                    , class "text-decoration-none d-inline-flex align-items-center gap-1"
                    ]
                    [ Html.span [ Attr.attribute "aria-hidden" "true" ] [ text "‹" ]
                    , Html.span [] [ text crumbDeVuelta.label ]
                    ]
                ]

        Just (InvitarASesion rutaLogin) ->
            div [ class "mb-1" ]
                [ Html.a
                    [ Route.href rutaLogin
                    , class "text-decoration-none d-inline-flex align-items-center gap-1 small"
                    , Attr.title "Con una cuenta ves todos los grupos en los que participás, sin tener que acordarte las URLs."
                    ]
                    [ i [ class "bi bi-person-circle" ] []
                    , Html.span [] [ text "Logueate para ver tus grupos" ]
                    ]
                ]


viewTabNav : Path.Path -> Grupo -> Html Msg
viewTabNav currentPath grupo =
    Bs.navTabs [ class "border-0 mb-0" ]
        [ Bs.navTab
            { active = currentPath == Path.Grupos_Id_ { id = grupo.id }
            , attrs = [ Path.href <| Path.Grupos_Id_ { id = grupo.id } ]
            }
            [ text "Resumen" ]
        , Bs.navTab
            { active = currentPath == Path.Grupos_GrupoId__Gastos { grupoId = grupo.id }
            , attrs = [ Path.href <| Path.Grupos_GrupoId__Gastos { grupoId = grupo.id } ]
            }
            [ text "Gastos" ]
        , Bs.navTab
            { active = currentPath == Path.Grupos_GrupoId__Transferencias { grupoId = grupo.id }
            , attrs = [ Path.href <| Path.Grupos_GrupoId__Transferencias { grupoId = grupo.id } ]
            }
            [ text "Transferencias" ]
        , Bs.navTab
            { active = currentPath == Path.Grupos_GrupoId__Participantes { grupoId = grupo.id }
            , attrs = [ Path.href <| Path.Grupos_GrupoId__Participantes { grupoId = grupo.id } ]
            }
            [ text "Participantes" ]
        , Bs.navTab
            { active = currentPath == Path.Grupos_GrupoId__Settings { grupoId = grupo.id }
            , attrs = [ Path.href <| Path.Grupos_GrupoId__Settings { grupoId = grupo.id } ]
            }
            [ text "Ajustes" ]
        ]


{-| The mobile-only bottom tab bar. Mirrors the desktop top tab nav plus a
raised "Ingresar pago" FAB in the centre and a "Más" popover (opening upward)
for the sections that don't fit on the bar. Hidden on `md+` via the
`navbar-bottom` component's own media query.

El menú "Más" también es donde vive compartir en mobile: en el header solo está
de `md` para arriba.

-}
viewBottomNav :
    Path.Path
    -> { title : String, url : String }
    -> Grupo
    -> Html Msg
viewBottomNav currentPath share grupo =
    let
        item icon label path =
            a
                [ classList [ ( "navbar-item", True ), ( "active", currentPath == path ) ]
                , Path.href path
                ]
                [ i [ class ("bi " ++ icon) ] []
                , Html.span [ class "text-truncate" ] [ text label ]
                ]

        masActive =
            (currentPath == Path.Grupos_GrupoId__Participantes { grupoId = grupo.id })
                || (currentPath == Path.Grupos_GrupoId__Settings { grupoId = grupo.id })
    in
    Html.nav [ Css.navbar_bottom, Css.barra_inferior_fija ]
        [ item "bi-house-door" "Resumen" (Path.Grupos_Id_ { id = grupo.id })
        , item "bi-card-list" "Gastos" (Path.Grupos_GrupoId__Gastos { grupoId = grupo.id })
        , a
            [ Css.navbar_item
            , PagoDetalleModal.hrefNuevoGasto currentPath
            ]
            [ Html.span [ Css.navbar_big_button ]
                [ i [ class "bi bi-plus-lg" ] [] ]
            , Html.span [] [ text "Ingresar gasto" ]
            ]
        , item "bi-wallet2" "Transferencias" (Path.Grupos_GrupoId__Transferencias { grupoId = grupo.id })
        , div [ Css.navbar_more, class "dropup" ]
            [ a
                [ classList [ ( "navbar-item", True ), ( "active", masActive ) ]
                , Attr.attribute "role" "button"
                , Attr.attribute "data-bs-toggle" "dropdown"
                , Attr.attribute "aria-expanded" "false"
                ]
                [ i [ class "bi bi-three-dots" ] []
                , Html.span [] [ text "Más" ]
                ]
            , ul [ class "dropdown-menu dropdown-menu-end shadow" ]
                [ li []
                    [ a
                        [ class "dropdown-item"
                        , classList [ ( "active", currentPath == Path.Grupos_GrupoId__Participantes { grupoId = grupo.id } ) ]
                        , Path.href (Path.Grupos_GrupoId__Participantes { grupoId = grupo.id })
                        ]
                        [ i [ class "bi bi-people me-2" ] []
                        , text "Participantes"
                        ]
                    ]
                , li []
                    [ a
                        [ class "dropdown-item"
                        , classList [ ( "active", currentPath == Path.Grupos_GrupoId__Settings { grupoId = grupo.id } ) ]
                        , Path.href (Path.Grupos_GrupoId__Settings { grupoId = grupo.id })
                        ]
                        [ i [ class "bi bi-gear me-2" ] []
                        , text "Ajustes"
                        ]
                    ]
                , li [] [ Html.hr [ class "dropdown-divider" ] [] ]
                , li []
                    [ a
                        [ class "dropdown-item"
                        , Attr.href "#"
                        , preventDefaultOn "click"
                            (Decode.succeed ( ShareUrl share, True ))
                        ]
                        [ i [ class "bi bi-link-45deg me-2" ] []
                        , text "Compartir link"
                        ]
                    ]
                , li []
                    [ a
                        [ class "dropdown-item"
                        , Attr.href "#"
                        , preventDefaultOn "click"
                            (Decode.succeed ( OpenQrShare share, True ))
                        ]
                        [ i [ class "bi bi-qr-code me-2" ] []
                        , text "Código QR"
                        ]
                    ]
                ]
            ]
        ]


{-| The "Compartir" split button: a native/clipboard share plus a QR code
option, both pointing at the current page's share target. Opening and closing
(including closing on blur) is handled by Bootstrap's own dropdown JS via
`data-bs-toggle`.
-}
viewShareDropdown : { title : String, url : String } -> Html Msg
viewShareDropdown { title, url } =
    div [ class "dropdown" ]
        [ button
            [ type_ "button"
            , class "btn btn-outline-secondary dropdown-toggle"
            , Attr.attribute "data-bs-toggle" "dropdown"
            , Attr.attribute "aria-expanded" "false"
            ]
            [ i [ class "bi bi-share me-1" ] []
            , text "Compartir"
            ]
        , ul [ class "dropdown-menu dropdown-menu-end shadow" ]
            [ li []
                [ a
                    [ class "dropdown-item"
                    , Attr.href "#"
                    , preventDefaultOn "click"
                        (Decode.succeed ( ShareUrl { title = title, url = url }, True ))
                    ]
                    [ i [ class "bi bi-link-45deg me-2" ] []
                    , text "Compartir link"
                    ]
                ]
            , li []
                [ a
                    [ class "dropdown-item"
                    , Attr.href "#"
                    , preventDefaultOn "click"
                        (Decode.succeed ( OpenQrShare { title = title, url = url }, True ))
                    ]
                    [ i [ class "bi bi-qr-code me-2" ] []
                    , text "Código QR"
                    ]
                ]
            ]
        ]


{-| Modal showing a scannable QR code of the group's link.
-}
viewQrModal : Maybe { title : String, url : String } -> Html Msg
viewQrModal qrShare =
    let
        qrImage url =
            QRCode.fromString url
                |> Result.map
                    (QRCode.toSvg
                        [ SvgAttr.width "240px"
                        , SvgAttr.height "240px"
                        ]
                    )
                |> Result.withDefault (text "No se pudo generar el código QR")
    in
    Bs.modal
        { isOpen = qrShare /= Nothing
        , onClose = CloseQrShare
        , centered = False
        , title =
            qrShare
                |> Maybe.map .title
                |> Maybe.withDefault "Código QR"
        , body =
            case qrShare of
                Just { url } ->
                    [ div [ class "text-center" ]
                        [ div
                            [ class "d-inline-block bg-white rounded p-3"
                            , style "line-height" "0"
                            ]
                            [ qrImage url ]
                        , p [ class "text-muted small mt-3 mb-0 text-break" ] [ text url ]
                        ]
                    ]

                Nothing ->
                    []
        , footer =
            [ Bs.btn Bs.Transparent [ onClick CloseQrShare ] [ text "Cerrar" ] ]
        }


viewGlobalUserSelector : Maybe ULID -> GrupoLike r -> Html Msg
viewGlobalUserSelector activeUser grupo =
    select
        [ class "form-select form-select-sm"
        , on "change"
            (Decode.at [ "target", "value" ] Decode.string
                |> Decode.map
                    (\value ->
                        ForwardSharedMessage <|
                            Shared.SetCurrentParticipante
                                { grupoId = grupo.id
                                , participanteId =
                                    if value == "" then
                                        Nothing

                                    else
                                        Just value
                                }
                    )
            )
        ]
        (option [ value "", selected (activeUser == Nothing) ] [ text "" ]
            :: (grupo.participantes
                    |> List.map
                        (\p ->
                            option
                                [ value p.id
                                , selected (activeUser == Just p.id)
                                ]
                                [ text p.nombre ]
                        )
               )
        )
