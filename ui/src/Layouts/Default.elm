module Layouts.Default exposing (Model, Msg(..), Props, ShouldHideNavbar(..), layout, viewGlobalUserSelector)

import Changelog
import Components.Ui5 as Ui5
import Css
import Date exposing (Date)
import Effect exposing (Effect)
import Generated.Api exposing (ShallowGrupo, ULID)
import Html exposing (Html, div, img, p, text)
import Html.Attributes as Attr exposing (selected, src, style, value)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Json.Encode
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
layout props shared _ =
    Layout.new
        { init = \() -> init
        , update = update
        , view = view props.navBarContent props.grupo shared.userId shared.toasties shared.lastReadChangelog (Date.fromPosix shared.zone shared.now)
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
    | ForwardSharedMessage ShouldHideNavbar Shared.Msg
    | OpenChangelog
    | CloseChangelog
    | MarkChangelogReadAndClose



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
              }
            , Effect.batch
                [ Effect.sendSharedMsg sharedMsg
                ]
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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view :
    Maybe (Bool -> Html Msg)
    -> WebData ShallowGrupo
    -> Maybe ULID
    -> Toasts
    -> Maybe Date
    -> Date
    -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model }
    -> View contentMsg
view navBarFunction remoteGrupo activeUser toasts lastReadChangelog now { toContentMsg, model, content } =
    let
        recentEntries =
            Changelog.recentChangelog lastReadChangelog now

        unread =
            List.length recentEntries
    in
    { title =
        if content.title == "" then
            "Banana split"

        else
            content.title
    , body =
        [ Ui5.navigationLayout
            [ Attr.attribute "mode" <|
                case ( navBarFunction, model.navBarOpen ) of
                    ( _, True ) ->
                        "Expanded"

                    ( _, False ) ->
                        "Auto"
            ]
            [ Ui5.shellBar
                (Ui5.slot "header"
                    :: (if unread > 0 then
                            [ Attr.attribute "notifications-count" <| String.fromInt unread
                            , Attr.attribute "show-notifications" ""
                            , on "notifications-click" (Decode.succeed <| toContentMsg OpenChangelog)
                            ]

                        else
                            []
                       )
                )
                [ Ui5.button
                    [ Attr.attribute "icon" "menu"
                    , Ui5.slot "startButton"
                    , onClick (toContentMsg ToggleNavBar)
                    ]
                    []
                , Ui5.shellbarBranding
                    [ Ui5.slot "branding"
                    , onClick (toContentMsg <| ForwardSharedMessage HideNavbarAfterEvent <| Shared.NavigateTo <| Path.Home_)
                    ]
                    [ text "Banana Split"
                    , img [ Ui5.slot "logo", src "/favicon.png" ] []
                    ]
                ]
            , Html.map toContentMsg <|
                viewChangelogPopover model.changelogOpen recentEntries
            , case navBarFunction of
                Just navBarF ->
                    Html.map (\e -> toContentMsg e) <| navBarF model.navBarOpen

                Nothing ->
                    text ""
            , div [ Css.toasts_container ]
                [ Html.map toContentMsg <|
                    Toasts.view Toasts.config renderToast ToastMsg toasts
                ]
            , case ( activeUser, remoteGrupo ) of
                ( Nothing, Success grupo ) ->
                    if List.isEmpty grupo.participantes then
                        div [ style "padding" "1rem" ] content.body

                    else
                        Html.map toContentMsg <|
                            div [ style "padding" "1rem", style "text-align" "center" ]
                                [ p [ style "margin-bottom" "1rem" ] [ Ui5.text "Por favor seleccioná quién sos para comenzar:" ]
                                , viewGlobalUserSelector activeUser grupo
                                ]

                _ ->
                    div [ style "padding" "1rem" ] content.body
            ]
        ]
    }


viewGlobalUserSelector : Maybe ULID -> GrupoLike r -> Html Msg
viewGlobalUserSelector activeUser grupo =
    Ui5.select
        [ on "change"
            (Decode.at [ "detail", "selectedOption", "value" ] Decode.string
                |> Decode.map (\userId -> ForwardSharedMessage HideNavbarAfterEvent <| Shared.SetCurrentUser { grupoId = grupo.id, userId = userId })
            )
        , Ui5.slot "fixedItems"
        ]
        (Ui5.option [ selected (activeUser == Nothing), value "" ] [ text "" ]
            :: (grupo.participantes
                    |> List.map
                        (\participante ->
                            Ui5.option
                                [ selected (activeUser == Just participante.participanteId)
                                , value participante.participanteId
                                ]
                                [ text participante.participanteNombre ]
                        )
               )
        )


renderToast : Toast -> Html Msg
renderToast toast =
    div [ Css.toast ]
        [ Ui5.messageStrip
            [ Attr.attribute "design"
                (case toast.level of
                    ToastSuccess ->
                        "Positive"

                    ToastDanger ->
                        "Negative"
                )
            , Attr.property "hideCloseButton" (Json.Encode.bool True)
            ]
            [ text toast.content ]
        ]


viewChangelogPopover : Bool -> List Changelog.Entry -> Html Msg
viewChangelogPopover isOpen entries =
    Ui5.dialog
        ([ Attr.attribute "header-text" "Novedades"
         , on "close" (Decode.succeed CloseChangelog)
         ]
            ++ (if isOpen then
                    [ Attr.attribute "open" "" ]

                else
                    []
               )
        )
        [ Ui5.list []
            (entries
                |> List.map viewChangelogEntry
            )
        , div [ Attr.attribute "slot" "footer", style "display" "flex", style "gap" "0.5rem", style "width" "100%", style "justify-content" "end" ]
            [ Ui5.button
                [ Attr.attribute "design" "Emphasized"
                , onClick MarkChangelogReadAndClose
                ]
                [ text "Marcar como leído" ]
            , Ui5.button
                [ Attr.attribute "design" "Transparent"
                , onClick CloseChangelog
                ]
                [ text "Cerrar" ]
            ]
        ]


viewChangelogEntry : Changelog.Entry -> Html Msg
viewChangelogEntry entry =
    Ui5.li
        [ Attr.attribute "description" entry.description
        , Attr.attribute "additional-text" (Date.toIsoString entry.date)
        ]
        [ text entry.title ]
