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
import Generated.Api as Api exposing (ClaimParticipanteResult(..), ClaimRejection(..), ULID)
import Json.Decode
import Json.Encode
import Models.Grupo
import Models.Store as Store
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path
import Set
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
      , autoSelectedGrupos = Set.empty
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
update route msg model =
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

                ( newModel, autoEffect ) =
                    maybeAutoSelect route { model | store = store }
            in
            ( newModel, Effect.batch [ cmd, autoEffect ] )

        SetCurrentUser { grupoId, userId } ->
            let
                newUserId : Maybe ULID
                newUserId =
                    if userId == "" then
                        Nothing

                    else
                        Just userId
            in
            ( { model
                | userId = newUserId

                -- An explicit choice (including "—") sticks for the session:
                -- guard the grupo so the owned participante won't override it.
                , autoSelectedGrupos = Set.insert grupoId model.autoSelectedGrupos
              }
            , case newUserId of
                Just uid ->
                    Effect.saveCurrentUser grupoId uid

                Nothing ->
                    Effect.clearCurrentUser grupoId
            )

        CurrentUserLoaded { userId } ->
            maybeAutoSelect route { model | userId = userId }

        GotCurrentUser currentUser ->
            maybeAutoSelect route { model | currentUser = currentUser }

        ClaimParticipante { grupoId, participanteId } ->
            ( model
            , Effect.sendCmd <|
                Api.putGrupoByIdParticipantesByParticipanteIdClaim grupoId
                    participanteId
                    (RemoteData.fromResult >> GotClaimResult { grupoId = grupoId })
            )

        UnclaimParticipante { grupoId, participanteId } ->
            ( model
            , Effect.sendCmd <|
                Api.deleteGrupoByIdParticipantesByParticipanteIdClaim grupoId
                    participanteId
                    (RemoteData.fromResult >> GotUnclaimResult { grupoId = grupoId })
            )

        GotClaimResult { grupoId } result ->
            handleClaimResult grupoId result model

        GotUnclaimResult { grupoId } result ->
            handleUnclaimResult grupoId result model

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


{-| When a logged-in user opens a grupo where they own a participante, select it
as the active "Ver como" participante automatically — but only when they haven't
picked one yet this session (`userId == Nothing` and the grupo isn't already in
`autoSelectedGrupos`). An explicit pick (see `SetCurrentUser`) marks the grupo so
this never overrides it. Fires opportunistically whenever the current user, the
local pick, or the store changes.
-}
maybeAutoSelect : Route () -> Model -> ( Model, Effect Msg )
maybeAutoSelect route model =
    let
        noop =
            ( model, Effect.none )
    in
    case ( model.userId, model.currentUser ) of
        ( Nothing, Success u ) ->
            case Models.Grupo.grupoIdFromPath route.path of
                Just grupoId ->
                    if Set.member grupoId model.autoSelectedGrupos then
                        noop

                    else
                        case Store.getGrupo grupoId model.store of
                            Success grupo ->
                                case Models.Grupo.ownedParticipante u.id grupo of
                                    Just p ->
                                        ( { model
                                            | userId = Just p.id
                                            , autoSelectedGrupos = Set.insert grupoId model.autoSelectedGrupos
                                          }
                                        , Effect.saveCurrentUser grupoId p.id
                                        )

                                    Nothing ->
                                        noop

                            _ ->
                                noop

                Nothing ->
                    noop

        _ ->
            noop


{-| Handle a claim response. On acceptance we switch "Ver como" to the claimed
participante and remember the choice; on a typed rejection we show the matching
message (the UI normally prevents rejections, so these cover stale state/races).
-}
handleClaimResult : ULID -> WebData ClaimParticipanteResult -> Model -> ( Model, Effect Msg )
handleClaimResult grupoId result model =
    case result of
        Success (ClaimAccepted p) ->
            ( { model
                | userId = Just p.id
                , autoSelectedGrupos = Set.insert grupoId model.autoSelectedGrupos
              }
            , Effect.batch
                [ Store.refreshGrupo grupoId
                , Effect.saveCurrentUser grupoId p.id
                , Effect.sendToast { level = ToastSuccess, content = "Ahora sos " ++ p.nombre }
                ]
            )

        Success (ClaimRejected reason) ->
            ( model
            , Effect.sendToast { level = ToastDanger, content = claimRejectionMessage reason }
            )

        Failure _ ->
            ( model
            , Effect.sendToast { level = ToastDanger, content = genericClaimError }
            )

        _ ->
            ( model, Effect.none )


{-| Handle an unclaim response. Just refresh the grupo (keeping whatever view was
active) and confirm.
-}
handleUnclaimResult : ULID -> WebData Api.Participante -> Model -> ( Model, Effect Msg )
handleUnclaimResult grupoId result model =
    case result of
        Success p ->
            ( model
            , Effect.batch
                [ Store.refreshGrupo grupoId
                , Effect.sendToast { level = ToastSuccess, content = "Dejaste de reclamar a " ++ p.nombre }
                ]
            )

        Failure _ ->
            ( model
            , Effect.sendToast { level = ToastDanger, content = genericClaimError }
            )

        _ ->
            ( model, Effect.none )


{-| A user-facing message for each way a claim can be refused.
-}
claimRejectionMessage : ClaimRejection -> String
claimRejectionMessage reason =
    case reason of
        ClaimedByOtherUser ->
            "Ese participante ya fue reclamado por otra persona"

        AlreadyOwnAnotherParticipante ->
            "Ya reclamaste otro participante en este grupo. Dejá de reclamarlo primero."

        ParticipanteNotFound ->
            "No encontramos ese participante"


genericClaimError : String
genericClaimError =
    "No pudimos completar la operación, probá de nuevo"



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
