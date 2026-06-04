module Layouts.Default exposing (Model, Msg(..), Props, ShouldHideNavbar(..), layout, viewGlobalUserSelector)

import Changelog
import Components.Bootstrap as Bs
import Date exposing (Date)
import Effect exposing (Effect)
import Generated.Api exposing (ShallowGrupo, ULID)
import Html exposing (Html, button, div, h2, h5, i, li, node, ol, option, p, select, span, strong, text, ul)
import Html.Attributes as Attr exposing (class, classList, selected, style, type_, value)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Layout exposing (Layout)
import Models.Grupo exposing (GrupoLike)
import Models.Store as Store
import Models.Store.Types exposing (Store)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Route.Path as Path
import Shared.Model as Shared
import Shared.Msg as Shared
import Utils.Toasts as Toasts
import Utils.Toasts.Types exposing (Toast, ToastLevel(..), ToastMsg, Toasts)
import View exposing (View)


type alias Props =
    { navBarContent : Maybe (Bool -> Html Msg)
    }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = \() -> init
        , update = update
        , view = view props shared.store route.path shared.userId shared.toasties shared.lastReadChangelog shared.today
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { navBarOpen : Bool
    , changelogOpen : Bool
    , openDropdown : Maybe String
    }


init : ( Model, Effect Msg )
init =
    ( { navBarOpen = False
      , changelogOpen = False
      , openDropdown = Nothing
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = ToastMsg ToastMsg
    | ToggleNavBar
    | ForwardSharedMessage ShouldHideNavbar Shared.Msg
    | OpenChangelog
    | CloseChangelog
    | MarkChangelogReadAndClose
    | ToggleDropdown String



-- This variant is not used but may be in the future
-- | LeaveNavbarAsBefore


type ShouldHideNavbar
    = HideNavbarAfterEvent


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ToastMsg toastMsg ->
            ( model
            , Effect.sendToastMsg toastMsg
            )

        ForwardSharedMessage shouldHideNavbar sharedMsg ->
            ( { model
                | navBarOpen =
                    case shouldHideNavbar of
                        HideNavbarAfterEvent ->
                            False
                , openDropdown = Nothing
              }
            , Effect.sendSharedMsg sharedMsg
            )

        ToggleNavBar ->
            ( { model | navBarOpen = not model.navBarOpen }
            , Effect.none
            )

        OpenChangelog ->
            ( { model | changelogOpen = True }
            , Effect.none
            )

        CloseChangelog ->
            ( { model | changelogOpen = False }
            , Effect.none
            )

        MarkChangelogReadAndClose ->
            ( { model | changelogOpen = False }
            , Effect.sendSharedMsg Shared.MarkChangelogRead
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view :
    Props
    -> Store
    -> Path.Path
    -> Maybe ULID
    -> Toasts
    -> Maybe Date
    -> Date
    -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model }
    -> View contentMsg
view props store currentPath activeUser toasts lastReadChangelog now { toContentMsg, model, content } =
    let
        recentEntries =
            Changelog.recentChangelog lastReadChangelog now

        unread =
            List.length recentEntries

        remoteGrupo =
            grupoIdFromPath currentPath
                |> Maybe.map (\grupoId -> Store.getGrupo grupoId store)
                |> Maybe.withDefault NotAsked
    in
    { title =
        if content.title == "" then
            "Banana split"

        else
            content.title
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
        , Html.map toContentMsg <|
            viewNavbar unread
        , case remoteGrupo of
            Success grupo ->
                Html.map toContentMsg <|
                    viewGroupHeader model currentPath activeUser store grupo

            _ ->
                text ""
        , Html.map toContentMsg <|
            viewOffcanvas model props.navBarContent
        , Html.map toContentMsg <|
            viewChangelogModal model.changelogOpen recentEntries
        , div [ class "position-fixed bottom-0 start-50 translate-middle-x p-3", Attr.style "z-index" "1090" ]
            [ Html.map toContentMsg <|
                Toasts.view Toasts.config renderToast ToastMsg toasts
            ]
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


viewNavbar : Int -> Html Msg
viewNavbar unread =
    Bs.navbar []
        [ button
            [ type_ "button"
            , class "navbar-toggler border-0 me-2"
            , Attr.attribute "aria-label" "Abrir menú"
            , onClick ToggleNavBar
            ]
            [ span [ class "navbar-toggler-icon" ] [] ]
        , Bs.navbarBrand
            [ Attr.href "#"
            , onClick (ForwardSharedMessage HideNavbarAfterEvent <| Shared.NavigateTo Path.Home_)
            ]
            [ text "Banana Split" ]
        , div [ class "ms-auto d-flex align-items-center gap-2" ]
            [ if unread > 0 then
                button
                    [ type_ "button"
                    , class "btn btn-link text-reset position-relative p-1"
                    , Attr.attribute "aria-label" ("Ver " ++ String.fromInt unread ++ " novedades")
                    , onClick OpenChangelog
                    ]
                    [ i [ class "bi bi-bell-fill" ] []
                    , Bs.badge "bg-danger rounded-pill position-absolute top-0 start-100 translate-middle"
                        [ style "font-size" "0.65em" ]
                        [ text (String.fromInt unread) ]
                    ]

              else
                text ""

            -- TODO(ludat) this is useless until we have proper users
            --, Bs.dropdown
            --    { isOpen = model.openDropdown == Just "user"
            --    , onToggle = ToggleDropdown "user"
            --    , label = [ text (Maybe.withDefault "Usuario" currentUserName) ]
            --    , items =
            --        case remoteGrupo of
            --            Success grupo ->
            --                [ Html.li [ class "px-3 py-2" ]
            --                    [ small [ class "text-muted d-block mb-1" ] [ text "Ver como:" ]
            --                    , viewGlobalUserSelector activeUser grupo
            --                    ]
            --                ]
            --
            --            _ ->
            --                []
            --    , attrs = []
            --    }
            ]
        ]


viewGroupHeader : Model -> Path.Path -> Maybe ULID -> Store -> ShallowGrupo -> Html Msg
viewGroupHeader model currentPath activeUser store grupo =
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
                                        (ForwardSharedMessage HideNavbarAfterEvent <|
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
                                                            (ForwardSharedMessage HideNavbarAfterEvent <|
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
                        [ Bs.btn Bs.Primary
                            [ onClick
                                (ForwardSharedMessage HideNavbarAfterEvent <|
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
            div [ class "container-fluid" ]
                [ viewTabNav currentPath grupo ]

          else
            text ""
        ]


{-| Extracts the grupo id carried by a route, if any. Used to look up the grupo
shown in the header straight from the store.
-}
grupoIdFromPath : Path.Path -> Maybe ULID
grupoIdFromPath currentPath =
    case currentPath of
        Path.Grupos_Id_ params ->
            Just params.id

        Path.Grupos_GrupoId__Pagos params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Pagos_New params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Pagos_PagoId_ params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Liquidaciones params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Participantes params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Settings params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Repartijas_RepartijaId_ params ->
            Just params.grupoId

        _ ->
            Nothing


{-| A single breadcrumb segment. `path` is `Just` when the segment should be a
client-side link, `Nothing` when it is rendered as plain text.
-}
type alias Crumb =
    { label : String, path : Maybe Path.Path }


{-| Computes everything the group header needs from the current path and store:
the breadcrumb trail, the big `h2` title, and whether the section tabs should be
shown (only on top-level sections).

Entity names are resolved from the store, falling back to `"Cargando"` while the
data is still loading.

-}
headerInfo :
    Path.Path
    -> Store
    -> ShallowGrupo
    -> { crumbs : List Crumb, title : String, showTabs : Bool }
headerInfo currentPath store grupo =
    let
        grupoCrumb =
            { label = grupo.nombre, path = Just (Path.Grupos_Id_ { id = grupo.id }) }

        pagosCrumb =
            { label = "Pagos", path = Just (Path.Grupos_GrupoId__Pagos { grupoId = grupo.id }) }

        prefix crumbs =
            { label = "Grupo", path = Nothing } :: crumbs

        topLevel sectionLabel =
            { crumbs =
                prefix
                    [ grupoCrumb
                    , { label = sectionLabel, path = Nothing }
                    ]
            , title = grupo.nombre
            , showTabs = True
            }
    in
    case currentPath of
        Path.Grupos_GrupoId__Pagos _ ->
            topLevel "Pagos"

        Path.Grupos_GrupoId__Liquidaciones _ ->
            topLevel "Liquidaciones"

        Path.Grupos_GrupoId__Participantes _ ->
            topLevel "Participantes"

        Path.Grupos_GrupoId__Settings _ ->
            topLevel "Ajustes"

        Path.Grupos_GrupoId__Pagos_New _ ->
            { crumbs =
                prefix
                    [ grupoCrumb
                    , { label = "Nuevo Pago", path = Nothing }
                    ]
            , title = "Nuevo pago"
            , showTabs = False
            }

        Path.Grupos_GrupoId__Pagos_PagoId_ params ->
            let
                pagoNombre =
                    Store.getPago params.pagoId store
                        |> RemoteData.toMaybe
                        |> Maybe.map .nombre
                        |> Maybe.withDefault "Cargando"
            in
            { crumbs =
                prefix
                    [ grupoCrumb
                    , pagosCrumb
                    , { label = pagoNombre, path = Nothing }
                    ]
            , title = pagoNombre
            , showTabs = False
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
                prefix
                    [ grupoCrumb
                    , pagosCrumb
                    , { label = pagoNombre, path = pagoCrumbPath }
                    , { label = "Deudores", path = Nothing }
                    ]
            , title = pagoNombre
            , showTabs = False
            }

        Path.Grupos_Id_ _ ->
            { crumbs = prefix [ grupoCrumb ]
            , title = grupo.nombre
            , showTabs = True
            }

        Path.NotFound_ ->
            { crumbs = prefix [ grupoCrumb ]
            , title = grupo.nombre
            , showTabs = True
            }

        Path.Home_ ->
            { crumbs = []
            , title = "Banana split"
            , showTabs = False
            }


viewBreadcrumb : List Crumb -> Html Msg
viewBreadcrumb crumbs =
    let
        lastIndex =
            List.length crumbs - 1

        viewCrumb index crumb =
            if index == lastIndex then
                li
                    [ class "breadcrumb-item active"
                    , Attr.attribute "aria-current" "page"
                    ]
                    [ text crumb.label ]

            else
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
            (List.indexedMap viewCrumb crumbs)
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


viewOffcanvas : Model -> Maybe (Bool -> Html Msg) -> Html Msg
viewOffcanvas model navBarContent =
    div []
        [ if model.navBarOpen then
            div
                [ class "offcanvas-backdrop fade show"
                , onClick ToggleNavBar
                ]
                []

          else
            text ""
        , div
            [ classList
                [ ( "offcanvas offcanvas-start", True )
                , ( "show", model.navBarOpen )
                ]
            , Attr.attribute "tabindex" "-1"
            ]
            [ div [ class "offcanvas-header" ]
                [ h5 [ class "offcanvas-title" ] [ text "Menú" ]
                , button
                    [ type_ "button"
                    , class "btn-close"
                    , Attr.attribute "aria-label" "Cerrar"
                    , onClick ToggleNavBar
                    ]
                    []
                ]
            , div [ class "offcanvas-body" ]
                [ case navBarContent of
                    Just f ->
                        f model.navBarOpen

                    Nothing ->
                        text ""
                ]
            ]
        ]


viewChangelogModal : Bool -> List Changelog.Entry -> Html Msg
viewChangelogModal isOpen entries =
    Bs.modal
        { isOpen = isOpen
        , onClose = CloseChangelog
        , title = "Novedades"
        , body = [ Bs.listGroup [] (List.map viewChangelogEntry entries) ]
        , footer =
            [ Bs.btn Bs.Transparent [ onClick CloseChangelog ] [ text "Cerrar" ]
            , Bs.btn Bs.Primary [ onClick MarkChangelogReadAndClose ] [ text "Marcar como leído" ]
            ]
        }


viewChangelogEntry : Changelog.Entry -> Html Msg
viewChangelogEntry entry =
    Bs.listGroupItem []
        [ strong [] [ text entry.title ]
        , p [ class "mb-0 text-muted small mt-1" ] [ text entry.description ]
        ]


viewGlobalUserSelector : Maybe ULID -> GrupoLike r -> Html Msg
viewGlobalUserSelector activeUser grupo =
    select
        [ class "form-select form-select-sm"
        , on "change"
            (Decode.at [ "target", "value" ] Decode.string
                |> Decode.map
                    (\userId ->
                        ForwardSharedMessage HideNavbarAfterEvent <|
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


renderToast : Toast -> Html Msg
renderToast toast =
    let
        bgClass =
            case toast.level of
                ToastSuccess ->
                    "text-bg-success"

                ToastDanger ->
                    "text-bg-danger"
    in
    div
        [ class ("toast show align-items-center border-0 " ++ bgClass)
        , Attr.attribute "role" "alert"
        , Attr.attribute "aria-live" "assertive"
        , Attr.attribute "aria-atomic" "true"
        ]
        [ div [ class "toast-body" ] [ text toast.content ] ]
