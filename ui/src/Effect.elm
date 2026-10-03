port module Effect exposing
    ( Effect
    , Location
    , back
    , batch
    , clearCurrentUser
    , copy
    , getCurrentUser
    , incoming
    , loadExternalUrl
    , map
    , none
    , outgoing
    , pushRoute
    , pushRoutePath
    , replaceRoute
    , replaceRoutePath
    , saveCurrentUser
    , saveLastReadChangelog
    , sendCmd
    , sendFeedback
    , sendMsg
    , sendSharedMsg
    , sendStoreMsg
    , sendToast
    , sendToastMsg
    , setUnsavedChangesWarning
    , share
    , telemetryEvent
    , toCmd
    )

import Browser.Navigation
import Dict exposing (Dict)
import Json.Encode
import Models.Store.Types exposing (StoreMsg)
import Route
import Route.Path
import Shared.Model
import Shared.Msg
import Task
import Url exposing (Url)
import Utils.Toasts.Types exposing (Toast, ToastMsg)


type Effect msg
    = -- BASICS
      None
    | Batch (List (Effect msg))
    | SendCmd (Cmd msg)
      -- ROUTING
    | PushUrl String
    | ReplaceUrl String
    | LoadExternalUrl String
    | Back
      -- SHARED
    | SendSharedMsg Shared.Msg.Msg



-- PORTS


port outgoing :
    { tag : String
    , data : Json.Encode.Value
    }
    -> Cmd msg


port incoming :
    ({ tag : String
     , data : Json.Encode.Value
     }
     -> msg
    )
    -> Sub msg



-- BASICS


{-| Don't send any effect.
-}
none : Effect msg
none =
    None


{-| Send multiple effects at once.
-}
batch : List (Effect msg) -> Effect msg
batch =
    Batch


{-| Send a normal `Cmd msg` as an effect, something like `Http.get` or `Random.generate`.
-}
sendCmd : Cmd msg -> Effect msg
sendCmd =
    SendCmd


{-| Send a message as an effect. Useful when emitting events from UI components.
-}
sendMsg : msg -> Effect msg
sendMsg msg =
    Task.succeed msg
        |> Task.perform identity
        |> SendCmd



-- ROUTING


type alias Location =
    { path : Route.Path.Path
    , query : Dict String String
    , hash : Maybe String
    }


{-| Set the new route, and make the back button go back to the current route.
-}
pushRoute : { path : Route.Path.Path, query : Dict String String, hash : Maybe String } -> Effect msg
pushRoute route =
    PushUrl (Route.toString route)


{-| Same as `Effect.pushRoute`, but without `query` or `hash` support
-}
pushRoutePath : Route.Path.Path -> Effect msg
pushRoutePath path =
    PushUrl (Route.Path.toString path)


{-| Set the new route, but replace the previous one, so clicking the back
button **won't** go back to the previous route.
-}
replaceRoute : { path : Route.Path.Path, query : Dict String String, hash : Maybe String } -> Effect msg
replaceRoute route =
    ReplaceUrl (Route.toString route)


{-| Same as `Effect.replaceRoute`, but without `query` or `hash` support
-}
replaceRoutePath : Route.Path.Path -> Effect msg
replaceRoutePath path =
    ReplaceUrl (Route.Path.toString path)


{-| Redirect users to a new URL, somewhere external to your web application.
-}
loadExternalUrl : String -> Effect msg
loadExternalUrl =
    LoadExternalUrl


{-| Navigate back one page
-}
back : Effect msg
back =
    Back


{-| Propagate a toast msg to the top of the state tree.
-}
sendToastMsg : ToastMsg -> Effect msg
sendToastMsg toastMsg =
    sendSharedMsg <| Shared.Msg.ToastMsg toastMsg


sendToast : Toast -> Effect msg
sendToast toast =
    sendSharedMsg <| Shared.Msg.AddToast toast


sendStoreMsg : StoreMsg -> Effect msg
sendStoreMsg toast =
    sendSharedMsg <| Shared.Msg.StoreMsg toast


sendSharedMsg : Shared.Msg.Msg -> Effect msg
sendSharedMsg msg =
    SendSharedMsg msg



-- STORAGE PORTS


saveCurrentUser : String -> String -> Effect msg
saveCurrentUser grupoId userId =
    SendCmd <|
        outgoing
            { tag = "SAVE_CURRENT_USER"
            , data =
                Json.Encode.object
                    [ ( "grupoId", Json.Encode.string grupoId )
                    , ( "participanteId", Json.Encode.string userId )
                    ]
            }


clearCurrentUser : String -> Effect msg
clearCurrentUser grupoId =
    SendCmd <|
        outgoing
            { tag = "CLEAR_CURRENT_USER"
            , data =
                Json.Encode.object
                    [ ( "grupoId", Json.Encode.string grupoId )
                    ]
            }


getCurrentUser : String -> Effect msg
getCurrentUser grupoId =
    SendCmd <|
        outgoing
            { tag = "GET_CURRENT_USER"
            , data =
                Json.Encode.object
                    [ ( "grupoId", Json.Encode.string grupoId )
                    ]
            }


setUnsavedChangesWarning : Bool -> Effect msg
setUnsavedChangesWarning enabled =
    SendCmd <|
        outgoing
            { tag = "SET_UNSAVED_CHANGES_WARNING"
            , data =
                Json.Encode.object
                    [ ( "enabled", Json.Encode.bool enabled )
                    ]
            }


saveLastReadChangelog : Effect msg
saveLastReadChangelog =
    SendCmd <|
        outgoing
            { tag = "SAVE_LAST_READ_CHANGELOG"
            , data = Json.Encode.null
            }


{-| Open the native share sheet (or fall back to copying the link to the
clipboard) for the given title and URL.
-}
share : { title : String, url : String } -> Effect msg
share { title, url } =
    SendCmd <|
        outgoing
            { tag = "SHARE"
            , data =
                Json.Encode.object
                    [ ( "title", Json.Encode.string title )
                    , ( "url", Json.Encode.string url )
                    ]
            }


{-| Mandar un comentario escrito por la persona.

Es el único lugar de la app por el que sale texto libre, y va como log record a
la telemetría, no a la API. No hay forma de responderlo: no se adjunta ningún
identificador. La pantalla que lo use tiene que decir que se manda.

-}
sendFeedback : String -> Effect msg
sendFeedback message =
    SendCmd <|
        outgoing
            { tag = "SEND_FEEDBACK"
            , data = Json.Encode.object [ ( "message", Json.Encode.string message ) ]
            }


{-| Reportar un evento de telemetría.

El nombre del evento y los atributos permitidos están declarados en
`src/js/telemetry.js`: lo que no esté en ese vocabulario se descarta y no se
manda. No pasar texto escrito por el usuario ni ids por acá; ver
`Utils.Telemetry` para los helpers de más alto nivel.

-}
telemetryEvent : String -> List ( String, String ) -> Effect msg
telemetryEvent name attributes =
    SendCmd <|
        outgoing
            { tag = "TELEMETRY_EVENT"
            , data =
                Json.Encode.object
                    [ ( "name", Json.Encode.string name )
                    , ( "attributes"
                      , Json.Encode.object <|
                            List.map (Tuple.mapSecond Json.Encode.string) attributes
                      )
                    ]
            }


{-| Copiar texto al portapapeles, sin pasar por la hoja de compartir. Para
contenido que no es un link (una dirección de email, por ejemplo).
-}
copy : String -> Effect msg
copy text =
    SendCmd <|
        outgoing
            { tag = "COPY"
            , data =
                Json.Encode.object
                    [ ( "text", Json.Encode.string text )
                    ]
            }



-- INTERNALS


{-| Elm Land depends on this function to connect pages and layouts
together into the overall app.
-}
map : (msg1 -> msg2) -> Effect msg1 -> Effect msg2
map fn effect =
    case effect of
        None ->
            None

        Batch list ->
            Batch (List.map (map fn) list)

        SendCmd cmd ->
            SendCmd (Cmd.map fn cmd)

        PushUrl url ->
            PushUrl url

        ReplaceUrl url ->
            ReplaceUrl url

        Back ->
            Back

        LoadExternalUrl url ->
            LoadExternalUrl url

        SendSharedMsg sharedMsg ->
            SendSharedMsg sharedMsg


{-| Elm Land depends on this function to perform your effects.
-}
toCmd :
    { key : Browser.Navigation.Key
    , url : Url
    , shared : Shared.Model.Model
    , fromSharedMsg : Shared.Msg.Msg -> msg
    , batch : List msg -> msg
    , toCmd : msg -> Cmd msg
    }
    -> Effect msg
    -> Cmd msg
toCmd options effect =
    case effect of
        None ->
            Cmd.none

        Batch list ->
            Cmd.batch (List.map (toCmd options) list)

        SendCmd cmd ->
            cmd

        PushUrl url ->
            Browser.Navigation.pushUrl options.key url

        ReplaceUrl url ->
            Browser.Navigation.replaceUrl options.key url

        Back ->
            Browser.Navigation.back options.key 1

        LoadExternalUrl url ->
            Browser.Navigation.load url

        SendSharedMsg sharedMsg ->
            Task.succeed sharedMsg
                |> Task.perform options.fromSharedMsg
