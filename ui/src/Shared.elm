module Shared exposing
    ( Flags
    , Model
    , Msg
    , decoder
    , init
    , subscriptions
    , update
    )

import Date
import Effect exposing (Effect, incoming)
import Generated.Api as Api exposing (ULID)
import Json.Decode
import Json.Encode
import Models.Store as Store
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg exposing (Msg(..))
import Time
import Utils.Toasts as Toast
import Utils.Toasts.Types exposing (ToastLevel(..))



-- FLAGS


type alias Flags =
    { now : Int, offset : Int, timeZone : String, lastReadChangelog : Maybe Int, origin : String }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map5 Flags
        (Json.Decode.field "now" Json.Decode.int)
        (Json.Decode.field "offset" Json.Decode.int)
        (Json.Decode.field "timeZone" Json.Decode.string)
        (Json.Decode.field "lastReadChangelog" (Json.Decode.nullable Json.Decode.int))
        (Json.Decode.field "origin" Json.Decode.string)



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init possiblyFlags _ =
    let
        flags =
            Result.withDefault { now = 0, offset = 0, timeZone = "UTC", lastReadChangelog = Nothing, origin = "" } possiblyFlags

        now =
            Time.millisToPosix flags.now

        timezone =
            Time.customZone flags.offset []
    in
    ( { toasties = Toast.initialState
      , store = Store.empty
      , userId = Nothing
      , currentUser = Loading
      , now = now
      , today = Date.fromPosix timezone now
      , timezoneName = flags.timeZone
      , timezoneOffset = flags.offset
      , timezone = timezone
      , lastReadChangelog =
            flags.lastReadChangelog
                |> Maybe.map (\ms -> Date.fromPosix timezone (Time.millisToPosix ms))
      , origin = flags.origin
      }
    , Effect.sendCmd (Api.getMe (RemoteData.fromResult >> GotCurrentUser))
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        Shared.Msg.NoOp ->
            ( model
            , Effect.none
            )

        Shared.Msg.NavigateTo path ->
            ( model
            , Effect.pushRoutePath path
            )

        ToastMsg toastMsg ->
            let
                ( newModel, cmd ) =
                    Toast.update Toast.config ToastMsg toastMsg model
            in
            ( newModel, Effect.sendCmd cmd )

        AddToast toast ->
            let
                ( newModel, cmd ) =
                    Toast.addToast Toast.config ToastMsg toast ( model, Cmd.none )
            in
            ( newModel, Effect.sendCmd cmd )

        StoreMsg storeMsg ->
            let
                ( store, cmd ) =
                    Store.update storeMsg model.store
            in
            ( { model | store = store }, cmd )

        SetCurrentUser { grupoId, userId } ->
            let
                newUserId : Maybe ULID
                newUserId =
                    if userId == "" then
                        Nothing

                    else
                        Just userId
            in
            ( { model | userId = newUserId }
            , case newUserId of
                Just uid ->
                    Effect.saveCurrentUser grupoId uid

                Nothing ->
                    Effect.clearCurrentUser grupoId
            )

        CurrentUserLoaded { userId } ->
            ( { model
                | userId = userId
              }
            , Effect.none
            )

        GotCurrentUser currentUser ->
            ( { model | currentUser = currentUser }
            , Effect.none
            )

        Logout ->
            ( { model | currentUser = Loading }
            , Effect.sendCmd (Api.postAuthLogout (\_ -> LoggedOut))
            )

        LoggedOut ->
            ( { model | currentUser = NotAsked }
            , Effect.pushRoutePath Route.Path.Login
            )

        MarkChangelogRead ->
            ( { model | lastReadChangelog = Just <| Date.fromPosix model.timezone model.now }
            , Effect.saveLastReadChangelog
            )

        Shared.Msg.Tick datetime ->
            ( { model | now = datetime, today = Date.fromPosix model.timezone datetime }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.batch
        [ incoming (decodeIncomingPortMessage >> Maybe.withDefault NoOp)
        , Time.every (60 * 1000) Shared.Msg.Tick
        ]


decodeIncomingPortMessage : { tag : String, data : Json.Encode.Value } -> Maybe Msg
decodeIncomingPortMessage { tag, data } =
    case tag of
        "CURRENT_USER_LOADED" ->
            Json.Decode.decodeValue
                (Json.Decode.map2
                    (\grupoId userId ->
                        CurrentUserLoaded
                            { grupoId = grupoId
                            , userId = userId
                            }
                    )
                    (Json.Decode.field "grupoId" Json.Decode.string)
                    (Json.Decode.field "userId" (Json.Decode.nullable Json.Decode.string))
                )
                data
                |> Result.toMaybe

        "SHARE_LINK_COPIED" ->
            Just <| AddToast { level = ToastSuccess, content = "Link copiado al portapapeles" }

        _ ->
            Nothing
