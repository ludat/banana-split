module Layouts.Default exposing (Model, Msg, Props, layout)

import Components.NavBar exposing (viewGlobalUserSelector)
import Components.Ui5 as Ui5
import Css
import Effect exposing (Effect)
import Generated.Api exposing (ShallowGrupo, ULID)
import Html exposing (Html, div, p, text)
import Html.Attributes as Attr exposing (attribute, style)
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
        [ Ui5.page
            [ style "height" "100vh" ]
            [ Ui5.bar
                [ Ui5.slot "header"
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

                _ ->
                    div [ style "padding" "1rem" ] content.body
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
