module Layouts.Default exposing (Model, Msg, Props, layout)

import Components.NavBar exposing (viewGlobalUserSelector)
import Components.Ui5 as Ui5
import Css
import Effect exposing (Effect)
import Generated.Api exposing (ShallowGrupo, ULID)
import Html exposing (Html, div, p, text)
import Html.Attributes as Attr exposing (attribute, class, style)
import Html.Events exposing (on, onClick)
import Json.Encode
import Layout exposing (Layout)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import Utils.Toasts as Toasts
import Utils.Toasts.Types exposing (Toast, ToastLevel(..), ToastMsg, Toasts)
import View exposing (View)


type alias Props =
    { navBarContent : Maybe (Bool -> Html Shared.Msg)
    , grupo : WebData ShallowGrupo
    }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared _ =
    Layout.new
        { init = \() -> init
        , update = update
        , view = view props.navBarContent props.grupo shared.userId shared.toasties
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { navBarOpen : Bool
    }


init : ( Model, Effect Msg )
init =
    ( { navBarOpen = False
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = ToastMsg ToastMsg
    | ToggleNavBar
    | ForwardSharedMessage Shared.Msg


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ToastMsg toastMsg ->
            ( model
            , Effect.sendToastMsg toastMsg
            )

        ForwardSharedMessage sharedMsg ->
            ( model
            , Effect.sendSharedMsg sharedMsg
            )

        ToggleNavBar ->
            ( { model | navBarOpen = not model.navBarOpen }
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view :
    Maybe (Bool -> Html Shared.Msg)
    -> WebData ShallowGrupo
    -> Maybe ULID
    -> Toasts
    -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model }
    -> View contentMsg
view navBarFunction remoteGrupo activeUser toasts { toContentMsg, model, content } =
    { title =
        if content.title == "" then
            "Banana split"

        else
            content.title
    , body =
        [ Ui5.navigationLayout
            [ Attr.attribute "mode" <|
                if model.navBarOpen then
                    "Expanded"

                else
                    "Collapsed"
            ]
            [ Ui5.shellBar
                [ Ui5.slot "header"
                ]
                [ Ui5.button
                    [ Attr.attribute "icon" "menu"
                    , Ui5.slot "startButton"
                    , Attr.id "startButton"
                    , onClick (toContentMsg ToggleNavBar)
                    ]
                    []
                , Ui5.shellbarBranding
                    [ Attr.attribute "slot" "branding"
                    ]
                    [ text "Banana Split" ]
                ]
            , Ui5.sideNavigation
                [ Attr.id "sn1"
                , Ui5.slot "sideContent"
                ]
                [ Ui5.sideNavigationItem
                    [ Attr.attribute "text" "Home"
                    , Attr.href "#home"
                    , Attr.attribute "icon" "home"
                    ]
                    []
                , Ui5.sideNavigationGroup
                    [ Attr.attribute "text" "Group 1"
                    , Attr.attribute "expanded" ""
                    ]
                    [ Ui5.sideNavigationItem
                        [ Attr.attribute "text" "Item 1"
                        , Attr.href "#item1"
                        , Attr.attribute "icon" "locate-me"
                        , Attr.attribute "expanded" ""
                        ]
                        [ Ui5.sideNavigationSubItem
                            [ Attr.attribute "text" "Sub Item 1"
                            , Attr.href "#subitem1"
                            ]
                            []
                        , Ui5.sideNavigationSubItem
                            [ Attr.attribute "text" "Sub Item 2"
                            , Attr.href "#subitem2"
                            ]
                            []
                        ]
                    , Ui5.sideNavigationItem
                        [ Attr.attribute "text" "Item 2"
                        , Attr.href "#item2"
                        , Attr.attribute "icon" "calendar"
                        , Attr.attribute "expanded" ""
                        ]
                        [ Ui5.sideNavigationSubItem
                            [ Attr.attribute "text" "Sub Item 3"
                            , Attr.href "#subitem3"
                            ]
                            []
                        , Ui5.sideNavigationSubItem
                            [ Attr.attribute "text" "Sub Item 4"
                            , Attr.href "#subitem4"
                            ]
                            []
                        ]
                    , Ui5.sideNavigationItem
                        [ Attr.attribute "text" "Item 3"
                        , Attr.href "#item2"
                        , Attr.attribute "icon" "activity-assigned-to-goal"
                        , Attr.attribute "expanded" ""
                        ]
                        [ Ui5.sideNavigationSubItem
                            [ Attr.attribute "text" "Sub Item 5"
                            , Attr.href "#subitem5"
                            ]
                            []
                        , Ui5.sideNavigationSubItem
                            [ Attr.attribute "text" "Sub Item 6"
                            , Attr.href "#subitem6"
                            ]
                            []
                        ]
                    ]
                , Ui5.sideNavigationGroup
                    [ Attr.attribute "text" "Group 2"
                    , Attr.attribute "expanded" ""
                    ]
                    [ Ui5.sideNavigationItem
                        [ Attr.attribute "text" "Item 4"
                        , Attr.href "#item4"
                        , Attr.attribute "icon" "history"
                        ]
                        []
                    , Ui5.sideNavigationItem
                        [ Attr.attribute "text" "Item 5"
                        , Attr.href "#item5"
                        , Attr.attribute "icon" "source-code"
                        ]
                        []
                    , Ui5.sideNavigationItem
                        [ Attr.attribute "text" "Item 6"
                        , Attr.href "#item6"
                        , Attr.attribute "icon" "background"
                        ]
                        []
                    ]
                , {- Fixed Items -}
                  Ui5.sideNavigationItem
                    [ Attr.attribute "slot" "fixedItems"
                    , Attr.attribute "text" "Legal"
                    , Attr.href "https://www.sap.com/about/legal/impressum.html"
                    , Attr.target "_blank"
                    , Attr.attribute "unselectable" ""
                    , Attr.attribute "icon" "compare"
                    ]
                    []
                , Ui5.sideNavigationItem
                    [ Attr.attribute "slot" "fixedItems"
                    , Attr.attribute "text" "Privacy"
                    , Attr.href "https://www.sap.com/about/legal/privacy.html"
                    , Attr.target "_blank"
                    , Attr.attribute "unselectable" ""
                    , Attr.attribute "icon" "locked"
                    ]
                    []
                , Ui5.sideNavigationItem
                    [ Attr.attribute "slot" "fixedItems"
                    , Attr.attribute "text" "Terms of Use"
                    , Attr.href "https://www.sap.com/terms-of-use"
                    , Attr.target "_blank"
                    , Attr.attribute "unselectable" ""
                    , Attr.attribute "icon" "document-text"
                    ]
                    []
                ]
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
                            Html.map ForwardSharedMessage <|
                                div [ style "padding" "1rem", style "text-align" "center" ]
                                    [ p [ style "margin-bottom" "1rem" ] [ text "Por favor seleccioná quién sos para comenzar:" ]
                                    , viewGlobalUserSelector activeUser grupo
                                    ]

                _ ->
                    div [ style "padding" "1rem", class "content" ] content.body
            ]
        ]
    }


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
