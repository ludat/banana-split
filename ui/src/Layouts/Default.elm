module Layouts.Default exposing (Model, Msg, Props, layout)

import Changelog
import Components.Bootstrap as Bs
import Components.Toasts
import Date exposing (Date)
import Dict
import Effect exposing (Effect, Location)
import Generated.Api exposing (User)
import Html exposing (Html, a, button, div, h5, hr, i, li, span, text, textarea, ul)
import Html.Attributes as Attr exposing (class, classList, type_)
import Html.Events exposing (onClick, onInput)
import Layout exposing (Layout)
import Models.Opinion as Opinion exposing (Opinion(..))
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared.Model as Shared
import Shared.Msg as Shared
import Utils.Telemetry as Telemetry
import Utils.Toasts
import Utils.Toasts.Types exposing (ToastLevel(..), ToastMsg, Toasts)
import View exposing (View)


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout _ shared route =
    Layout.new
        { init = \() -> init
        , update = update shared.today
        , view = view (currentLocation route) shared.toasties shared.lastReadChangelog shared.today shared.currentUser
        , subscriptions = subscriptions
        }


currentLocation : Route () -> Location
currentLocation route =
    { path = route.path, query = route.query, hash = route.hash }



-- MODEL


type alias Model =
    { navBarOpen : Bool
    , changelogOpen : Bool
    , changelogOpinion : Maybe Opinion
    , feedbackOpen : Bool
    , feedbackText : String
    }


init : ( Model, Effect Msg )
init =
    ( { navBarOpen = False
      , changelogOpen = False
      , changelogOpinion = Nothing
      , feedbackOpen = False
      , feedbackText = ""
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = ToastMsg ToastMsg
    | ToggleNavBar
    | NavigateAndClose Path.Path
    | GoToLoginAndClose Location
    | OpenChangelog
    | CloseChangelog
    | MarkChangelogReadAndClose
    | OpinarChangelog Opinion
    | OpenFeedback
    | CloseFeedback
    | FeedbackChanged String
    | SendFeedback
    | DoLogout


update : Date -> Msg -> Model -> ( Model, Effect Msg )
update now msg model =
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

        GoToLoginAndClose from ->
            ( { model | navBarOpen = False }
            , Effect.pushRoute
                { path = Path.Login
                , query = Dict.singleton "redirect" <| Route.toString from
                , hash = Nothing
                }
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

        OpinarChangelog opinion ->
            -- Las mismas novedades que muestra el modal (ver `view`).
            ( { model | changelogOpinion = Just opinion }
            , Effect.telemetryEvent
                (Telemetry.changelogFeedback
                    { opinion = Opinion.toString opinion
                    , novedades = List.map .title (Changelog.recentChangelog Nothing now)
                    }
                )
            )

        OpenFeedback ->
            ( { model | navBarOpen = False, feedbackOpen = True }
            , Effect.none
            )

        CloseFeedback ->
            ( { model | feedbackOpen = False, feedbackText = "" }
            , Effect.none
            )

        FeedbackChanged texto ->
            ( { model | feedbackText = texto }
            , Effect.none
            )

        SendFeedback ->
            -- No hay confirmación de entrega posible: el comentario se va por
            -- telemetría, no por la API, así que el toast dice que se mandó y
            -- nada más.
            ( { model | feedbackOpen = False, feedbackText = "" }
            , Effect.batch
                [ Effect.sendFeedback Telemetry.feedbackGeneral model.feedbackText
                , Utils.Toasts.pushToast ToastSuccess "¡Gracias! Comentario enviado."
                ]
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
    Location
    -> Toasts
    -> Maybe Date
    -> Date
    -> WebData User
    -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model }
    -> View contentMsg
view location toasts lastReadChangelog now currentUser { toContentMsg, model, content } =
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
            viewNavbar location unread currentUser
        , Html.map toContentMsg <|
            viewOffcanvas location model unread currentUser
        , Html.map toContentMsg <|
            viewChangelogModal model modalEntries
        , Html.map toContentMsg <|
            viewFeedbackModal model
        , Html.map toContentMsg <|
            Components.Toasts.view ToastMsg toasts
        ]
            ++ content.body
    }


viewNavbar : Location -> Int -> WebData User -> Html Msg
viewNavbar location unread currentUser =
    Bs.navbar [ Attr.id "navbar" ]
        [ button
            [ type_ "button"
            , class "navbar-toggler border-0 me-2"
            , Attr.attribute "aria-label" "Abrir menú"
            , Telemetry.click "navbar.open_menu"
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
                    , Telemetry.click "navbar.changelog"
                    , onClick OpenChangelog
                    ]
                    [ i [ class "bi bi-bell-fill" ] []
                    , Bs.badge "bg-danger rounded-pill position-absolute top-0 start-100 translate-middle"
                        [ Attr.style "font-size" "0.65em" ]
                        [ text (String.fromInt unread) ]
                    ]

              else
                text ""
            , viewUserIcon location currentUser
            ]
        ]


viewUserIcon : Location -> WebData User -> Html Msg
viewUserIcon location currentUser =
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
                            [ type_ "button", class "dropdown-item", Telemetry.click "navbar.account", onClick (NavigateAndClose Path.Cuenta) ]
                            [ text "Mi cuenta" ]
                        ]
                    , li []
                        [ button
                            [ type_ "button", class "dropdown-item", Telemetry.click "navbar.logout", onClick DoLogout ]
                            [ text "Cerrar sesión" ]
                        ]
                    ]

                _ ->
                    [ li []
                        [ button
                            [ type_ "button", class "dropdown-item", Telemetry.click "navbar.login", onClick (GoToLoginAndClose location) ]
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
        , ul [ Attr.id "navbar-menu-cuenta", class "dropdown-menu dropdown-menu-end" ] menuItems
        ]


{-| The global app menu shown in the offcanvas, identical on every page. Items
that don't have a page/feature yet render as non-functional placeholders.
-}
viewOffcanvas : Location -> Model -> Int -> WebData User -> Html Msg
viewOffcanvas location model unread currentUser =
    let
        menuLink : String -> Msg -> List (Html Msg) -> Html Msg
        menuLink track msg children =
            a
                [ class "nav-link"
                , Attr.href "#"
                , Telemetry.click track
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
                , Telemetry.click "menu.close_backdrop"
                , onClick ToggleNavBar
                ]
                []

          else
            text ""
        , div
            [ Attr.id "menu-lateral"
            , classList
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
                    , Telemetry.click "menu.close"
                    , onClick ToggleNavBar
                    ]
                    []
                ]
            , div [ class "offcanvas-body d-flex flex-column h-100" ]
                [ div [ class "nav nav-pills flex-column" ]
                    [ menuLink "menu.home" (NavigateAndClose Path.Home_) [ text "Inicio" ]
                    , menuLink "menu.changelog"
                        OpenChangelog
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
                    , menuLink "menu.feedback" OpenFeedback [ text "Enviar comentarios" ]
                    , menuPlaceholder "Documentación"
                    ]
                , div [ class "nav nav-pills flex-column border-top pt-2 mt-2" ]
                    [ menuLink "menu.new_grupo" (NavigateAndClose Path.Home_) [ text "Crear nuevo Grupo" ]
                    ]
                , div [ class "nav nav-pills flex-column border-top pt-2 mt-2" ] <|
                    case currentUser of
                        Success user ->
                            [ span [ class "nav-link disabled text-truncate" ]
                                [ text ("Sesión: " ++ user.email) ]
                            , menuLink "menu.account" (NavigateAndClose Path.Cuenta) [ text "Mi cuenta" ]
                            , menuLink "menu.logout" DoLogout [ text "Cerrar sesión" ]
                            ]

                        _ ->
                            [ menuLink "menu.login" (GoToLoginAndClose location) [ text "Iniciar sesión" ] ]
                , div [ class "nav nav-pills flex-column border-top pt-2 mt-auto" ]
                    [ menuPlaceholder "Idioma: Español" ]
                , div [ class "nav nav-pills flex-column border-top pt-2" ]
                    [ menuPlaceholder "Reportar problema" ]
                ]
            ]
        ]


viewFeedbackModal : Model -> Html Msg
viewFeedbackModal model =
    Bs.modal
        { id = "modal-comentarios"
        , isOpen = model.feedbackOpen
        , onClose = CloseFeedback
        , title = "Enviar comentarios"
        , centered = False
        , body =
            [ Html.p [ class "text-muted small" ]
                [ text "Contanos qué te gustaría que mejore, o qué encontraste roto." ]
            , textarea
                [ class "form-control"
                , Attr.rows 5
                , Attr.maxlength 2000
                , Attr.placeholder "Escribí acá..."
                , Attr.value model.feedbackText
                , onInput FeedbackChanged
                ]
                []
            , Html.p [ class "text-muted small mt-2 mb-0" ]
                [ text "Esto se manda a quienes desarrollan Banana Split. No lleva tus datos ni tu sesión, así que no podemos responderte — si querés respuesta, dejanos cómo contactarte." ]
            ]
        , footer =
            [ Bs.btn Bs.Transparent [ Telemetry.click "feedback.cancel", onClick CloseFeedback ] [ text "Cancelar" ]
            , Bs.btn Bs.Primary
                [ Telemetry.click "feedback.send"
                , onClick SendFeedback
                , Attr.disabled (String.trim model.feedbackText == "")
                ]
                [ text "Enviar" ]
            ]
        }


viewChangelogModal : Model -> List Changelog.Entry -> Html Msg
viewChangelogModal model entries =
    Bs.modal
        { id = "modal-novedades"
        , isOpen = model.changelogOpen
        , onClose = CloseChangelog
        , title = "Novedades"
        , centered = False
        , body =
            [ Bs.listGroup [] (List.map viewChangelogEntry entries)
            , if List.isEmpty entries then
                text ""

              else
                viewChangelogOpinion model.changelogOpinion
            ]
        , footer =
            [ Bs.btn Bs.Transparent [ Telemetry.click "changelog.close", onClick CloseChangelog ] [ text "Cerrar" ]
            , Bs.btn Bs.Primary [ Telemetry.click "changelog.mark_read", onClick MarkChangelogReadAndClose ] [ text "Marcar como leído" ]
            ]
        }


{-| Con tocar una opción ya se manda, como la encuesta de la repartija. La
respuesta queda mientras dure el layout, así no se vuelve a preguntar cada vez
que se abre el modal.
-}
viewChangelogOpinion : Maybe Opinion -> Html Msg
viewChangelogOpinion respuesta =
    case respuesta of
        Nothing ->
            div [ class "mt-3" ]
                [ Html.p [ class "mb-2 text-center" ] [ text "¿Con estos cambios la app está mejor o peor?" ]
                , div [ class "d-flex gap-2" ]
                    (List.map
                        (\opinion ->
                            Bs.btn Bs.Secondary
                                [ class "flex-grow-1 d-flex flex-column align-items-center gap-1"
                                , Telemetry.click "changelog.opinion"
                                , onClick (OpinarChangelog opinion)
                                ]
                                [ i [ class ("bi fs-4 " ++ Opinion.icono opinion) ] []
                                , span [] [ text (Opinion.label opinion) ]
                                ]
                        )
                        [ Mejor, Igual, Peor ]
                    )
                ]

        Just opinion ->
            Html.p [ class "mt-3 mb-0 text-center" ]
                [ i [ class ("bi text-success me-1 " ++ Opinion.icono opinion) ] []
                , text "¡Gracias! Respondiste "
                , Html.strong [] [ text (Opinion.label opinion) ]
                , text "."
                ]


viewChangelogEntry : Changelog.Entry -> Html Msg
viewChangelogEntry entry =
    Bs.listGroupItem []
        [ Html.strong [] [ text entry.title ]
        , Html.p [ class "mb-0 text-muted small mt-1" ] [ text entry.description ]
        ]
