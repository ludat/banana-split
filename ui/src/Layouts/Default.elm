module Layouts.Default exposing (Model, Msg, Props, layout)

import Components.NavBar exposing (navBarItem, viewGlobalUserSelector)
import Css
import Effect exposing (Effect)
import Generated.Api exposing (Grupo, ShallowGrupo, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Layout exposing (Layout)
import Models.Grupo exposing (GrupoLike)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path exposing (Path)
import Shared
import Utils.Toasts as Toasts
import Utils.Toasts.Types exposing (Toast, ToastLevel(..), ToastMsg, Toasts)
import View exposing (View)


type alias Props =
    { navBarContent : Maybe (Bool -> Html Shared.Msg)
    , grupo : WebData ShallowGrupo
    }


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = \() -> init props
        , update = update
        , view = view props.navBarContent props.grupo shared.userId shared.toasties route.path
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { navBarOpen : Bool
    }


init : Props -> ( Model, Effect Msg )
init props =
    ( { navBarOpen = False
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = NoOp
    | ToggleNavBar
    | ToastMsg ToastMsg
    | ForwardSharedMessage Shared.Msg


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ToggleNavBar ->
            ( { model
                | navBarOpen = not model.navBarOpen
              }
            , Effect.none
            )

        NoOp ->
            ( model
            , Effect.none
            )

        ToastMsg toastMsg ->
            ( model
            , Effect.sendToastMsg toastMsg
            )

        ForwardSharedMessage sharedMsg ->
            ( model
            , Effect.sendSharedMsg sharedMsg
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view :
    Maybe (Bool -> Html Shared.Msg)
    -> WebData ShallowGrupo
    -> Maybe ULID
    -> Toasts
    -> Path
    -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model }
    -> View contentMsg
view navBarFunction remoteGrupo activeUser toasts path { toContentMsg, model, content } =
    { title =
        if content.title == "" then
            "Banana split"

        else
            content.title
    , body =
        [ Html.node "ui5-page"
            [ style "height" "100vh" ]
            [ Html.node "ui5-bar"
                [ attribute "slot" "header"
                , attribute "design" "Header"
                ]
              <|
                case navBarFunction of
                    Just navBarF ->
                        [ Html.map toContentMsg <|
                            Html.map ForwardSharedMessage <|
                                navBarF model.navBarOpen
                        ]

                    Nothing ->
                        []
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

                ( _, _ ) ->
                    div [ style "padding" "1rem" ] content.body
            ]
        ]
    }


renderToast : Toast -> Html Msg
renderToast toast =
    div [ Css.toast ]
        [ div
            [ class "notification"
            , case toast.level of
                ToastNoLevel ->
                    class ""

                ToastSuccess ->
                    class "is-success"

                ToastInfo ->
                    class "is-info"

                ToastWarning ->
                    class "is-warning"

                ToastDanger ->
                    class "is-danger"
            ]
            [ text toast.content ]
        ]
