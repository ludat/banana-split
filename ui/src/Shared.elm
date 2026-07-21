module Shared exposing
    ( Flags
    , Model
    , Msg
    , currentParticipante
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
            possiblyFlags
                |> Result.withDefault
                    { now = 0
                    , offset = 0
                    , timeZone = "UTC"
                    , lastReadChangelog = Nothing
                    , origin = ""
                    }

        now =
            Time.millisToPosix flags.now

        timezone =
            Time.customZone flags.offset []
    in
    ( { toasties = Toast.initialState
      , store = Store.empty
      , participanteId = Nothing
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
    , Effect.sendCmd (Api.postAuthRefresh CurrentUserLoaded)
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
            ( { model | store = store }, Effect.batch [ cmd ] )

        SetCurrentParticipante { grupoId, participanteId } ->
            case participanteId of
                Just pid ->
                    let
                        -- The participante the logged-in account owns in this grupo, if
                        -- any. Picking it is the same as the default derivation, so we
                        -- clear the stored pick instead of persisting a redundant one.
                        ownedId =
                            case ( model.currentUser, Store.getGrupo grupoId model.store ) of
                                ( Success u, Success grupo ) ->
                                    Models.Grupo.ownedParticipante u.id grupo |> Maybe.map .id

                                _ ->
                                    Nothing
                    in
                    if Just pid == ownedId then
                        ( { model | participanteId = Nothing }
                        , Effect.clearCurrentUser grupoId
                        )

                    else
                        ( { model | participanteId = Just pid }
                        , Effect.saveCurrentUser grupoId pid
                        )

                Nothing ->
                    ( { model | participanteId = Nothing }
                    , Effect.clearCurrentUser grupoId
                    )

        CurrentParticipanteLoaded { participanteId } ->
            ( { model | participanteId = participanteId }
            , Effect.none
            )

        CurrentUserLoaded currentUser ->
            ( { model | currentUser = RemoteData.fromResult currentUser }
            , Effect.none
            )

        ClaimParticipante { grupoId, participanteId } ->
            ( model
            , Effect.sendCmd <|
                Api.putGrupoByIdParticipantesByParticipanteIdClaim grupoId
                    participanteId
                    (GotClaimResult { grupoId = grupoId })
            )

        UnclaimParticipante { grupoId, participanteId } ->
            ( model
            , Effect.sendCmd <|
                Api.deleteGrupoByIdParticipantesByParticipanteIdClaim grupoId
                    participanteId
                    (GotUnclaimResult { grupoId = grupoId })
            )

        GotClaimResult { grupoId } result ->
            case result of
                Ok (ClaimAccepted p) ->
                    ( { model
                        | participanteId = Just p.id
                      }
                    , Effect.batch
                        [ Store.refreshGrupo grupoId
                        , Effect.saveCurrentUser grupoId p.id
                        , Effect.sendToast { level = ToastSuccess, content = "Ahora sos " ++ p.nombre }
                        ]
                    )

                Ok (ClaimRejected reason) ->
                    ( model
                    , Effect.sendToast { level = ToastDanger, content = claimRejectionMessage reason }
                    )

                Err _ ->
                    ( model
                    , Effect.sendToast { level = ToastDanger, content = genericClaimError }
                    )

        GotUnclaimResult { grupoId } result ->
            case result of
                Ok p ->
                    ( model
                    , Effect.batch
                        [ Store.refreshGrupo grupoId
                        , Effect.sendToast { level = ToastSuccess, content = "Dejaste de reclamar a " ++ p.nombre }
                        ]
                    )

                Err _ ->
                    ( model
                    , Effect.sendToast { level = ToastDanger, content = genericClaimError }
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


{-| The participante the current session should act as in a grupo: the stored
manual pick when present, otherwise the participante the logged-in account owns
in that grupo (resolving the grupo from the store). Only the manual pick is ever
persisted; the owned fallback is derived at read-time.
-}
currentParticipante : Model -> ULID -> Maybe ULID
currentParticipante model grupoId =
    case Store.getGrupo grupoId model.store of
        Success grupo ->
            Models.Grupo.currentParticipante model.participanteId model.currentUser grupo

        _ ->
            model.participanteId


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
            data
                |> Json.Decode.decodeValue
                    (Json.Decode.map2
                        (\grupoId participanteId ->
                            CurrentParticipanteLoaded
                                { grupoId = grupoId
                                , participanteId = participanteId
                                }
                        )
                        (Json.Decode.field "grupoId" Json.Decode.string)
                        (Json.Decode.field "participanteId" (Json.Decode.nullable Json.Decode.string))
                    )
                |> Result.toMaybe

        "SHARE_LINK_COPIED" ->
            Just <| AddToast { level = ToastSuccess, content = "Link copiado al portapapeles" }

        _ ->
            Nothing
