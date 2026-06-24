module Layouts.Default.Grupo exposing (Model, Msg, Props, layout)

import Components.Bootstrap as Bs
import Css
import Effect exposing (Effect)
import Generated.Api exposing (ShallowGrupo, ULID)
import Html exposing (Html, a, button, div, h2, i, li, node, ol, option, p, select, strong, text, ul)
import Html.Attributes as Attr exposing (class, classList, selected, style, type_, value)
import Html.Events exposing (on, onClick, preventDefaultOn)
import Json.Decode as Decode
import Layout exposing (Layout)
import Layouts.Default
import Models.Grupo exposing (GrupoLike, grupoIdFromPath)
import Models.Store as Store
import Models.Store.Types exposing (Store)
import QRCode
import RemoteData exposing (RemoteData(..))
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
        { init = \() -> init
        , update = update
        , view = view shared.store route.path shared.userId shared.origin
        , subscriptions = subscriptions
        }
        |> Layout.withParentProps {}



-- MODEL


type alias Model =
    { openDropdown : Maybe String
    , qrShare : Maybe { title : String, url : String }
    }


init : ( Model, Effect Msg )
init =
    ( { openDropdown = Nothing
      , qrShare = Nothing
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = ForwardSharedMessage Shared.Msg
    | ToggleDropdown String
    | ShareUrl { title : String, url : String }
    | OpenQrShare { title : String, url : String }
    | CloseQrShare


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ForwardSharedMessage sharedMsg ->
            ( { model | openDropdown = Nothing }
            , Effect.sendSharedMsg sharedMsg
            )

        ToggleDropdown name ->
            ( { model
                | openDropdown =
                    if model.openDropdown == Just name then
                        Nothing

                    else
                        Just name
              }
            , Effect.none
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
    -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model }
    -> View contentMsg
view store currentPath activeUser origin { toContentMsg, model, content } =
    let
        remoteGrupo =
            grupoIdFromPath currentPath
                |> Maybe.map (\grupoId -> Store.getGrupo grupoId store)
                |> Maybe.withDefault NotAsked
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
                    viewGroupHeader model origin currentPath activeUser store grupo

            _ ->
                text ""
        , Html.map toContentMsg <|
            viewQrModal model.qrShare
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


viewGroupHeader : Model -> String -> Path.Path -> Maybe ULID -> Store -> ShallowGrupo -> Html Msg
viewGroupHeader model origin currentPath activeUser store grupo =
    let
        info =
            headerInfo currentPath store grupo
    in
    div [ class "border-bottom" ]
        [ div [ class "container-fluid py-3" ]
            [ div [ class "d-flex flex-wrap align-items-start justify-content-between gap-3" ]
                [ div []
                    [ viewBreadcrumb info.crumbs
                    , h2 [ class "mb-0 fw-bold" ] [ text info.title ]
                    ]
                , div [ class "d-flex flex-column align-items-end gap-2" ]
                    [ div
                        [ classList
                            [ ( "dropdown", True )
                            , ( "show", model.openDropdown == Just "ver-como" )
                            ]
                        ]
                        [ button
                            [ type_ "button"
                            , class "btn btn-link btn-sm text-muted text-decoration-none p-0 dropdown-toggle"
                            , Attr.attribute "aria-expanded"
                                (if model.openDropdown == Just "ver-como" then
                                    "true"

                                 else
                                    "false"
                                )
                            , onClick (ToggleDropdown "ver-como")
                            ]
                            [ text "Ver como: "
                            , strong []
                                [ text
                                    (activeUser
                                        |> Maybe.andThen
                                            (\uid ->
                                                grupo.participantes
                                                    |> List.filter (\p -> p.id == uid)
                                                    |> List.head
                                                    |> Maybe.map .nombre
                                            )
                                        |> Maybe.withDefault "—"
                                    )
                                ]
                            ]
                        , ul
                            [ classList
                                [ ( "dropdown-menu", True )
                                , ( "dropdown-menu-end", True )
                                , ( "show", model.openDropdown == Just "ver-como" )
                                ]
                            ]
                            (li [ class "px-2 py-1" ]
                                [ button
                                    [ type_ "button"
                                    , class "dropdown-item rounded"
                                    , classList [ ( "active", activeUser == Nothing ) ]
                                    , onClick
                                        (ForwardSharedMessage <|
                                            Shared.SetCurrentUser { grupoId = grupo.id, userId = "" }
                                        )
                                    ]
                                    [ text "—" ]
                                ]
                                :: (grupo.participantes
                                        |> List.map
                                            (\p ->
                                                li [ class "px-2 py-1" ]
                                                    [ button
                                                        [ type_ "button"
                                                        , class "dropdown-item rounded"
                                                        , classList [ ( "active", activeUser == Just p.id ) ]
                                                        , onClick
                                                            (ForwardSharedMessage <|
                                                                Shared.SetCurrentUser { grupoId = grupo.id, userId = p.id }
                                                            )
                                                        ]
                                                        [ text p.nombre ]
                                                    ]
                                            )
                                   )
                            )
                        ]
                    , div [ class "d-flex flex-wrap align-items-center gap-2" ]
                        [ viewShareDropdown
                            { title = info.share.title
                            , url = origin ++ Path.toString info.share.path
                            }
                        , Bs.btn Bs.Primary
                            [ class "d-none d-md-inline-flex"
                            , onClick
                                (ForwardSharedMessage <|
                                    Shared.NavigateTo <|
                                        Path.Grupos_GrupoId__Pagos_New { grupoId = grupo.id }
                                )
                            ]
                            [ i [ class "bi bi-plus-lg me-1" ] []
                            , text "Agregar pago"
                            ]
                        ]
                    ]
                ]
            ]
        , if info.showTabs then
            div [ class "container-fluid d-none d-md-block" ]
                [ viewTabNav currentPath grupo ]

          else
            text ""
        , if info.showTabs then
            viewBottomNav currentPath grupo

          else
            text ""
        ]


{-| A single breadcrumb segment. `path` is `Just` when the segment should be a
client-side link, `Nothing` when it is rendered as plain text.
-}
type alias Crumb =
    { label : String, path : Maybe Path.Path }


{-| Computes everything the group header needs from the current path and store:
the breadcrumb trail, the big `h2` title, and whether the section tabs should be
shown (only on top-level sections).

Entity names are resolved from the store, falling back to `"Cargando..."` while the
data is still loading.

-}
headerInfo :
    Path.Path
    -> Store
    -> ShallowGrupo
    -> { crumbs : List Crumb, title : String, showTabs : Bool, share : { title : String, path : Path.Path } }
headerInfo currentPath store grupo =
    let
        grupoCrumb : Crumb
        grupoCrumb =
            { label = grupo.nombre, path = Just (Path.Grupos_Id_ { id = grupo.id }) }

        pagosCrumb : Crumb
        pagosCrumb =
            { label = "Pagos", path = Just (Path.Grupos_GrupoId__Pagos { grupoId = grupo.id }) }

        gruposCrumb : Crumb
        gruposCrumb =
            { label = "Grupos", path = Nothing }

        grupoShare =
            { title = grupo.nombre, path = Path.Grupos_Id_ { id = grupo.id } }
    in
    case currentPath of
        Path.Grupos_GrupoId__Pagos _ ->
            { crumbs = [ gruposCrumb, grupoCrumb ]
            , title = "Pagos"
            , showTabs = True
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Liquidaciones _ ->
            { crumbs = [ gruposCrumb, grupoCrumb ]
            , title = "Liquidaciones"
            , showTabs = True
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Participantes _ ->
            { crumbs = [ gruposCrumb, grupoCrumb ]
            , title = "Participantes"
            , showTabs = True
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Settings _ ->
            { crumbs = [ gruposCrumb, grupoCrumb ]
            , title = "Ajustes"
            , showTabs = True
            , share = { path = currentPath, title = grupo.nombre }
            }

        Path.Grupos_GrupoId__Pagos_New params ->
            { crumbs =
                [ gruposCrumb
                , grupoCrumb
                , pagosCrumb
                ]
            , title = "Nuevo pago"
            , showTabs = False
            , share = { title = "Nuevo pago", path = Path.Grupos_GrupoId__Pagos_New params }
            }

        Path.Grupos_GrupoId__Pagos_PagoId_ params ->
            let
                pagoNombre =
                    Store.getPago params.pagoId store
                        |> RemoteData.toMaybe
                        |> Maybe.map .nombre
                        |> Maybe.withDefault "Cargando..."
            in
            { crumbs =
                [ gruposCrumb
                , grupoCrumb
                , pagosCrumb
                ]
            , title = pagoNombre
            , showTabs = False
            , share = { title = pagoNombre, path = Path.Grupos_GrupoId__Pagos_PagoId_ params }
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

                pagoCrumbPath =
                    maybeRepartija
                        |> Maybe.map (\r -> Path.Grupos_GrupoId__Pagos_PagoId_ { grupoId = grupo.id, pagoId = r.pagoId })
            in
            { crumbs =
                [ gruposCrumb
                , grupoCrumb
                , pagosCrumb
                , { label = pagoNombre, path = pagoCrumbPath }
                ]
            , title = "Deudores de '" ++ pagoNombre ++ "'"
            , showTabs = False
            , share = { title = "Deudores de " ++ pagoNombre, path = Path.Grupos_GrupoId__Repartijas_RepartijaId_ params }
            }

        Path.Grupos_Id_ _ ->
            { crumbs = [ gruposCrumb ]
            , title = grupo.nombre
            , showTabs = True
            , share = grupoShare
            }

        Path.NotFound_ ->
            { crumbs = []
            , title = grupo.nombre
            , showTabs = False
            , share = grupoShare
            }

        Path.Home_ ->
            { crumbs = []
            , title = "Banana split"
            , showTabs = False
            , share = grupoShare
            }


viewBreadcrumb : List Crumb -> Html Msg
viewBreadcrumb crumbs =
    let
        viewCrumb crumb =
            li [ class "breadcrumb-item" ]
                [ case crumb.path of
                    Just path ->
                        Html.a [ Path.href path ] [ text crumb.label ]

                    Nothing ->
                        text crumb.label
                ]
    in
    Html.nav [ Attr.attribute "aria-label" "breadcrumb" ]
        [ ol [ class "breadcrumb mb-1 small" ]
            (crumbs |> List.map viewCrumb)
        ]


viewTabNav : Path.Path -> ShallowGrupo -> Html Msg
viewTabNav currentPath grupo =
    Bs.navTabs [ class "border-0 mb-0" ]
        [ Bs.navTab
            { active = currentPath == Path.Grupos_Id_ { id = grupo.id }
            , attrs = [ Path.href <| Path.Grupos_Id_ { id = grupo.id } ]
            }
            [ text "Resumen" ]
        , Bs.navTab
            { active = currentPath == Path.Grupos_GrupoId__Pagos { grupoId = grupo.id }
            , attrs = [ Path.href <| Path.Grupos_GrupoId__Pagos { grupoId = grupo.id } ]
            }
            [ text "Pagos" ]
        , Bs.navTab
            { active = currentPath == Path.Grupos_GrupoId__Liquidaciones { grupoId = grupo.id }
            , attrs = [ Path.href <| Path.Grupos_GrupoId__Liquidaciones { grupoId = grupo.id } ]
            }
            [ text "Liquidaciones" ]
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
-}
viewBottomNav :
    Path.Path
    -> ShallowGrupo
    -> Html Msg
viewBottomNav currentPath grupo =
    let
        item icon label path =
            a
                [ classList [ ( "navbar-item", True ), ( "active", currentPath == path ) ]
                , Path.href path
                ]
                [ i [ class ("bi " ++ icon) ] []
                , Html.span [] [ text label ]
                ]

        masActive =
            (currentPath == Path.Grupos_GrupoId__Participantes { grupoId = grupo.id })
                || (currentPath == Path.Grupos_GrupoId__Settings { grupoId = grupo.id })
    in
    Html.nav [ Css.navbar_bottom ]
        [ item "bi-house-door" "Resumen" (Path.Grupos_Id_ { id = grupo.id })
        , item "bi-card-list" "Pagos" (Path.Grupos_GrupoId__Pagos { grupoId = grupo.id })
        , a
            [ Css.navbar_item
            , Path.href (Path.Grupos_GrupoId__Pagos_New { grupoId = grupo.id })
            ]
            [ Html.span [ Css.navbar_big_button ]
                [ i [ class "bi bi-plus-lg" ] [] ]
            , Html.span [] [ text "Ingresar pago" ]
            ]
        , item "bi-wallet2" "Saldos" (Path.Grupos_GrupoId__Liquidaciones { grupoId = grupo.id })
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
                    (\userId ->
                        ForwardSharedMessage <|
                            Shared.SetCurrentUser { grupoId = grupo.id, userId = userId }
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
