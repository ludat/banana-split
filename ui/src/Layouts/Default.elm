module Layouts.Default exposing (Model, Msg, Props, layout)

import Changelog
import Components.Bootstrap as Bs
import Date exposing (Date)
import Effect exposing (Effect)
import Html exposing (Html, a, button, div, h5, i, span, text)
import Html.Attributes as Attr exposing (class, classList, type_)
import Html.Events exposing (onClick)
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path as Path
import Shared.Model as Shared
import Shared.Msg as Shared
import Utils.Toasts as Toasts
import Utils.Toasts.Types exposing (Toast, ToastLevel(..), ToastMsg, Toasts)
import View exposing (View)


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout _ shared _ =
    Layout.new
        { init = \() -> init
        , update = update
        , view = view shared.toasties shared.lastReadChangelog shared.today
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view :
    Toasts
    -> Maybe Date
    -> Date
    -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model }
    -> View contentMsg
view toasts lastReadChangelog now { toContentMsg, model, content } =
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
            viewNavbar unread
        , Html.map toContentMsg <|
            viewOffcanvas model unread
        , Html.map toContentMsg <|
            viewChangelogModal model.changelogOpen modalEntries
        , div [ class "position-fixed bottom-0 start-50 translate-middle-x p-3", Attr.style "z-index" "1090" ]
            [ Html.map toContentMsg <|
                Toasts.view Toasts.config renderToast ToastMsg toasts
            ]
        ]
            ++ content.body
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
            , onClick (NavigateAndClose Path.Home_)
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
                        [ Attr.style "font-size" "0.65em" ]
                        [ text (String.fromInt unread) ]
                    ]

              else
                text ""
            ]
        ]


{-| The global app menu shown in the offcanvas, identical on every page. Items
that don't have a page/feature yet render as non-functional placeholders.
-}
viewOffcanvas : Model -> Int -> Html Msg
viewOffcanvas model unread =
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
