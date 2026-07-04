module Layouts.Default exposing (Model, Msg, Props, layout)

import Changelog
import Components.Bootstrap as Bs
import Components.Toasts
import Date exposing (Date)
import Effect exposing (Effect)
import Generated.Api exposing (User)
import Html exposing (Html, a, button, div, h5, hr, i, li, span, text, ul)
import Html.Attributes as Attr exposing (class, classList, type_)
import Html.Events exposing (onClick)
import Layout exposing (Layout)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared.Model as Shared
import Shared.Msg as Shared
import Utils.Toasts.Types exposing (ToastMsg, Toasts)
import View exposing (View)


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout _ shared _ =
    Layout.new
        { init = \() -> init
        , update = update
        , view = view shared.toasties shared.lastReadChangelog shared.today shared.currentUser
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { navBarOpen : Bool
    , changelogOpen : Bool
    }


init : ( Model, Effect Msg )
init =
    ( { navBarOpen = False
      , changelogOpen = False
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = ToastMsg ToastMsg
    | ToggleNavBar
    | NavigateAndClose Path.Path
    | OpenChangelog
    | CloseChangelog
    | MarkChangelogReadAndClose
    | DoLogout


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ToastMsg toastMsg ->
            ( model
            , Effect.sendToastMsg toastMsg
            )

        ToggleNavBar ->
            ( { model | navBarOpen = not model.navBarOpen }
            , Effect.none
            )

        NavigateAndClose path ->
            ( { model | navBarOpen = False }
            , Effect.sendSharedMsg (Shared.NavigateTo path)
            )

        OpenChangelog ->
            ( { model | changelogOpen = True, navBarOpen = False }
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

        DoLogout ->
            ( { model | navBarOpen = False }
            , Effect.sendSharedMsg Shared.Logout
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view :
    Toasts
    -> Maybe Date
    -> Date
    -> WebData User
    -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model }
    -> View contentMsg
view toasts lastReadChangelog now currentUser { toContentMsg, model, content } =
    let
        recentEntries =
            Changelog.recentChangelog lastReadChangelog now

        unread =
            List.length recentEntries

        -- The modal always shows the recent entries as if they were unread, so
        -- it isn't empty after the user has marked them as read.
        modalEntries =
            Changelog.recentChangelog Nothing now
    in
    { title =
        if content.title == "" then
            "Banana split"

        else
            content.title
    , body =
        [ Html.map toContentMsg <|
            viewNavbar unread currentUser
        , Html.map toContentMsg <|
            viewOffcanvas model unread currentUser
        , Html.map toContentMsg <|
            viewChangelogModal model.changelogOpen modalEntries
        , Html.map toContentMsg <|
            Components.Toasts.view ToastMsg toasts
        ]
            ++ content.body
    }


viewNavbar : Int -> WebData User -> Html Msg
viewNavbar unread currentUser =
    Bs.navbar []
        [ button
            [ type_ "button"
            , class "navbar-toggler border-0 me-2"
            , Attr.attribute "aria-label" "Abrir menú"
            , onClick ToggleNavBar
            ]
            [ span [ class "navbar-toggler-icon" ] [] ]
        , Bs.navbarBrand [ Attr.href "#" ] [ text "Banana Split" ]
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
                        [ Attr.style "font-size" "0.65em" ]
                        [ text (String.fromInt unread) ]
                    ]

              else
                text ""
            , viewUserIcon currentUser
            ]
        ]


viewUserIcon : WebData User -> Html Msg
viewUserIcon currentUser =
    let
        icon =
            case currentUser of
                Success _ ->
                    "bi bi-person-circle fs-5"

                _ ->
                    "bi bi-person fs-5"

        menuItems =
            case currentUser of
                Success user ->
                    [ li [] [ span [ class "dropdown-header text-truncate" ] [ text user.email ] ]
                    , li [] [ hr [ class "dropdown-divider" ] [] ]
                    , li []
                        [ button
                            [ type_ "button", class "dropdown-item", onClick DoLogout ]
                            [ text "Cerrar sesión" ]
                        ]
                    ]

                _ ->
                    [ li []
                        [ button
                            [ type_ "button", class "dropdown-item", onClick (NavigateAndClose Path.Login) ]
                            [ text "Iniciar sesión" ]
                        ]
                    ]
    in
    div [ class "dropdown" ]
        [ button
            [ type_ "button"
            , class "btn btn-link text-reset p-1"
            , Attr.attribute "data-bs-toggle" "dropdown"
            , Attr.attribute "aria-expanded" "false"
            , Attr.attribute "aria-label" "Cuenta"
            ]
            [ i [ class icon ] [] ]
        , ul [ class "dropdown-menu dropdown-menu-end" ] menuItems
        ]


{-| The global app menu shown in the offcanvas, identical on every page. Items
that don't have a page/feature yet render as non-functional placeholders.
-}
viewOffcanvas : Model -> Int -> WebData User -> Html Msg
viewOffcanvas model unread currentUser =
    let
        menuLink : Msg -> List (Html Msg) -> Html Msg
        menuLink msg children =
            a
                [ class "nav-link"
                , Attr.href "#"
                , onClick msg
                ]
                children

        menuPlaceholder : String -> Html Msg
        menuPlaceholder label =
            span
                [ class "nav-link disabled d-flex align-items-center justify-content-between"
                , Attr.attribute "aria-disabled" "true"
                ]
                [ text label
                , Bs.badge "bg-secondary-subtle text-secondary-emphasis rounded-pill ms-2"
                    []
                    [ text "Próximamente" ]
                ]
    in
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
                [ h5 [ class "offcanvas-title" ] [ text "Banana Split" ]
                , button
                    [ type_ "button"
                    , class "btn-close"
                    , Attr.attribute "aria-label" "Cerrar"
                    , onClick ToggleNavBar
                    ]
                    []
                ]
            , div [ class "offcanvas-body d-flex flex-column h-100" ]
                [ div [ class "nav nav-pills flex-column" ]
                    [ menuLink (NavigateAndClose Path.Home_) [ text "Inicio" ]
                    , menuLink OpenChangelog
                        [ text "Novedades"
                        , if unread > 0 then
                            Bs.badge "bg-danger rounded-pill ms-2"
                                []
                                [ text (String.fromInt unread) ]

                          else
                            text ""
                        ]
                    , menuPlaceholder "Ajustes generales"
                    , menuPlaceholder "Consejos"
                    , menuPlaceholder "Enviar comentarios"
                    , menuPlaceholder "Documentación"
                    ]
                , div [ class "nav nav-pills flex-column border-top pt-2 mt-2" ]
                    [ menuLink (NavigateAndClose Path.Home_) [ text "Crear nuevo Grupo" ]
                    ]
                , div [ class "nav nav-pills flex-column border-top pt-2 mt-2" ] <|
                    case currentUser of
                        Success user ->
                            [ span [ class "nav-link disabled text-truncate" ]
                                [ text ("Sesión: " ++ user.email) ]
                            , menuLink DoLogout [ text "Cerrar sesión" ]
                            ]

                        _ ->
                            [ menuLink (NavigateAndClose Path.Login) [ text "Iniciar sesión" ] ]
                , div [ class "nav nav-pills flex-column border-top pt-2 mt-auto" ]
                    [ menuPlaceholder "Idioma: Español" ]
                , div [ class "nav nav-pills flex-column border-top pt-2" ]
                    [ menuPlaceholder "Reportar problema" ]
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
        [ Html.strong [] [ text entry.title ]
        , Html.p [ class "mb-0 text-muted small mt-1" ] [ text entry.description ]
        ]
