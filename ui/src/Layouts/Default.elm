module Layouts.Default exposing (Model, Msg(..), Props, ShouldHideNavbar(..), layout, viewGlobalUserSelector)

import Changelog
import Components.Bootstrap as Bs
import Date exposing (Date)
import Effect exposing (Effect)
import Generated.Api exposing (ShallowGrupo, ULID)
import Html exposing (Html, button, div, h2, h5, i, li, node, option, p, select, small, span, strong, text, ul)
import Html.Attributes as Attr exposing (class, classList, selected, style, type_, value)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Layout exposing (Layout)
import Models.Grupo exposing (GrupoLike)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared.Model as Shared
import Shared.Msg as Shared
import Utils.Toasts as Toasts
import Utils.Toasts.Types exposing (Toast, ToastLevel(..), ToastMsg, Toasts)
import View exposing (View)


type alias Props =
    { navBarContent : Maybe (Bool -> Html Msg)
    , grupo : WebData ShallowGrupo
    }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = \() -> init
        , update = update
        , view = view props route.path shared.userId shared.toasties shared.lastReadChangelog shared.today
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
    -> Path.Path
    -> Maybe ULID
    -> Toasts
    -> Maybe Date
    -> Date
    -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model }
    -> View contentMsg
view props currentPath activeUser toasts lastReadChangelog now { toContentMsg, model, content } =
    let
        recentEntries =
            Changelog.recentChangelog lastReadChangelog now

        unread =
            List.length recentEntries

        remoteGrupo =
            props.grupo
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
                    viewGroupHeader model currentPath activeUser grupo

            _ ->
                text ""
        , Html.map toContentMsg <|
            viewOffcanvas model props.navBarContent
        , Html.map toContentMsg <|
            viewChangelogModal model.changelogOpen recentEntries
        , div [ class "position-fixed bottom-0 start-50 translate-middle-x p-3" ]
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
                            , viewGlobalUserSelector activeUser grupo
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


viewGroupHeader : Model -> Path.Path -> Maybe ULID -> ShallowGrupo -> Html Msg
viewGroupHeader model currentPath activeUser grupo =
    div [ class "border-bottom" ]
        [ div [ class "container-fluid py-3" ]
            [ div [ class "d-flex flex-wrap align-items-start justify-content-between gap-3" ]
                [ div []
                    [ small [ class "text-muted" ] [ text "Grupo" ]
                    , h2 [ class "mb-0 fw-bold" ] [ text grupo.nombre ]
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
                        [ Bs.dropdown
                            { isOpen = model.openDropdown == Just "compartir"
                            , onToggle = ToggleDropdown "compartir"
                            , label = [ text "Compartir grupo" ]
                            , items = []
                            , attrs = []
                            }
                        , Bs.btn Bs.Primary
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
        , div [ class "container-fluid" ]
            [ viewTabNav currentPath grupo ]
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
            { active = isPayosPath currentPath grupo.id
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


isPayosPath : Path.Path -> ULID -> Bool
isPayosPath path grupoId =
    case path of
        Path.Grupos_GrupoId__Pagos params ->
            params.grupoId == grupoId

        Path.Grupos_GrupoId__Pagos_PagoId_ params ->
            params.grupoId == grupoId

        Path.Grupos_GrupoId__Pagos_New params ->
            params.grupoId == grupoId

        _ ->
            False


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
