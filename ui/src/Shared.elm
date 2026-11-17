module Shared exposing
    ( Flags
    , InitialData
    , Model
    , Msg
    , currentParticipante
    , decoder
    , init
    , participantePropio
    , preferenciasDeGrupo
    , subscriptions
    , update
    )

import Date
import Dict exposing (Dict)
import Effect exposing (Effect, incoming)
import Generated.Api as Api exposing (ClaimParticipanteResult(..), ULID)
import Json.Decode
import Json.Encode
import Models.Grupo
import Models.PreferenciasDeGrupo as PreferenciasDeGrupo exposing (PreferenciasDeGrupo)
import Models.Store as Store
import Process
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared.Model
import Shared.Msg exposing (Msg(..))
import Task
import Time
import Utils.Telemetry as Telemetry
import Utils.Toasts as Toast
import Utils.Toasts.Types exposing (ToastLevel(..))



-- FLAGS


type alias Flags =
    { now : Int
    , offset : Int
    , timeZone : String
    , lastReadChangelog : Maybe Int
    , repartijaIntroDismissed : Bool
    , preferenciasDeGrupos : PreferenciasDeGrupo.Leidas
    , origin : String
    , initialData : Maybe InitialData
    }


{-| Lo que el backend inyecta en el index.html (ver `Site.Frontend`), o en dev
`/api/initial-data.js`. Lo lee `interop.js`.
-}
type alias InitialData =
    { user : Maybe Api.User
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.map8 Flags
        (Json.Decode.field "now" Json.Decode.int)
        (Json.Decode.field "offset" Json.Decode.int)
        (Json.Decode.field "timeZone" Json.Decode.string)
        (Json.Decode.field "lastReadChangelog" (Json.Decode.nullable Json.Decode.int))
        (Json.Decode.field "repartijaIntroDismissed" Json.Decode.bool)
        (Json.Decode.oneOf
            [ Json.Decode.field "preferenciasDeGrupos" PreferenciasDeGrupo.decoder
            , Json.Decode.succeed { preferencias = Dict.empty, ilegibles = [ "preferencias" ] }
            ]
        )
        (Json.Decode.field "origin" Json.Decode.string)
        (Json.Decode.field "initialData"
            (Json.Decode.nullable
                (Json.Decode.map InitialData
                    (Json.Decode.field "user" (Json.Decode.nullable Api.jsonDecUser))
                )
            )
        )



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
                    , repartijaIntroDismissed = False
                    , preferenciasDeGrupos = { preferencias = Dict.empty, ilegibles = [] }
                    , origin = ""
                    , initialData = Nothing
                    }

        now =
            Time.millisToPosix flags.now

        timezone =
            Time.customZone flags.offset []

        -- El usuario inyectado sale del token, así que se muestra ya pero igual
        -- se refresca: eso renueva la cookie y trae el usuario de la base. Se
        -- demora para no ocupar una de las conexiones del navegador mientras
        -- carga la página. Sin token no hay nada que refrescar.
        ( currentUser, refrescarSesion ) =
            case flags.initialData of
                Just { user } ->
                    case user of
                        Just u ->
                            ( Success u, Effect.sendCmd (Task.perform (\() -> RefrescarSesion) (Process.sleep 10000)) )

                        Nothing ->
                            ( NotAsked, Effect.none )

                Nothing ->
                    ( Loading, Effect.sendCmd (Api.postAuthRefresh CurrentUserLoaded) )
    in
    ( { toasties = Toast.initialState
      , store = Store.empty
      , participanteElegido = Dict.empty
      , preferenciasDeGrupos = flags.preferenciasDeGrupos.preferencias
      , currentUser = currentUser
      , now = now
      , today = Date.fromPosix timezone now
      , timezoneName = flags.timeZone
      , timezoneOffset = flags.offset
      , timezone = timezone
      , lastReadChangelog =
            flags.lastReadChangelog
                |> Maybe.map (\ms -> Date.fromPosix timezone (Time.millisToPosix ms))
      , repartijaIntroDismissed = flags.repartijaIntroDismissed
      , origin = flags.origin
      }
    , Effect.batch
        [ refrescarSesion
        , Effect.telemetryEvent (Telemetry.preferenciasIlegibles flags.preferenciasDeGrupos.ilegibles)
        ]
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
            -- Sólo por esta sesión: lo que se guarda es lo de los ajustes.
            ( { model | participanteElegido = model.participanteElegido |> Dict.insert grupoId participanteId }
            , Store.ensurePagadoYConsumido grupoId participanteId model.store
            )

        CambiarPreferenciaDeGrupo grupoId cambio ->
            case cambio of
                PreferenciasDeGrupo.ParticipanteId (Just participanteId) ->
                    serParticipante grupoId participanteId model

                PreferenciasDeGrupo.ParticipanteId Nothing ->
                    ( { model
                        | preferenciasDeGrupos = aplicarPreferencia grupoId cambio model
                        , participanteElegido = model.participanteElegido |> Dict.remove grupoId
                      }
                    , Effect.savePreferenciaDeGrupo grupoId cambio
                    )

        RefrescarSesion ->
            ( model, Effect.sendCmd (Api.postAuthRefresh CurrentUserLoaded) )

        CurrentUserLoaded currentUser ->
            -- Al arrancar el usuario ya viene en el HTML; esto es por loguearse
            -- sin recargar (`Pages.Login`), con grupos que ya estaban en el
            -- store y no pudieron saber a quién reclamó.
            ( { model | currentUser = RemoteData.fromResult currentUser }
            , model.store.grupos
                |> Dict.keys
                |> List.map (Effect.sendSharedMsg << GrupoCargado)
                |> Effect.batch
            )

        GrupoCargado grupoId ->
            -- El reclamado pasa a ser el de este navegador, así la próxima vez
            -- se sabe quién sos antes de que llegue el grupo. Lo elegido en la
            -- sesión ("Ver como") se respeta.
            case reclamadoEn grupoId model of
                Just reclamado ->
                    if (preferenciasDeGrupo model grupoId).participanteId == Just reclamado then
                        ( model, Effect.none )

                    else
                        let
                            cambio =
                                PreferenciasDeGrupo.ParticipanteId (Just reclamado)
                        in
                        ( { model | preferenciasDeGrupos = aplicarPreferencia grupoId cambio model }
                        , Effect.batch
                            [ Effect.savePreferenciaDeGrupo grupoId cambio
                            , if Dict.member grupoId model.participanteElegido then
                                Effect.none

                              else
                                Store.ensurePagadoYConsumido grupoId reclamado model.store
                            ]
                        )

                Nothing ->
                    ( model, Effect.none )

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
                    let
                        ( conParticipante, efecto ) =
                            serParticipante grupoId p.id model
                    in
                    ( conParticipante
                    , Effect.batch
                        [ efecto
                        , Store.refreshGrupo grupoId
                        , Effect.sendToast { level = ToastSuccess, content = "Ahora sos " ++ p.nombre }
                        ]
                    )

                Ok (ClaimRejected reason) ->
                    ( model
                    , Effect.sendToast { level = ToastDanger, content = Models.Grupo.claimErrorMessage reason }
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
            -- Lo elegido en "Ver como" era de la sesión con cuenta. Donde había
            -- algo elegido se vuelve a lo guardado, y eso es lo que hay que pedir.
            ( { model | currentUser = NotAsked, participanteElegido = Dict.empty }
            , model.participanteElegido
                |> Dict.keys
                |> List.filterMap
                    (\grupoId ->
                        participantePropio model grupoId
                            |> Maybe.map (\participanteId -> Store.ensurePagadoYConsumido grupoId participanteId model.store)
                    )
                |> Effect.batch
            )

        MarkChangelogRead ->
            ( { model | lastReadChangelog = Just <| Date.fromPosix model.timezone model.now }
            , Effect.saveLastReadChangelog
            )

        DismissRepartijaIntro ->
            ( { model | repartijaIntroDismissed = True }
            , Effect.saveRepartijaIntroDismissed
            )

        Shared.Msg.Tick datetime ->
            ( { model | now = datetime, today = Date.fromPosix model.timezone datetime }
            , Effect.none
            )


{-| Pasa a ser quién sos en el grupo en este navegador. Lo elegido en la sesión
se descarta para que se vea en seguida.
-}
serParticipante : ULID -> ULID -> Model -> ( Model, Effect Msg )
serParticipante grupoId participanteId model =
    let
        cambio =
            PreferenciasDeGrupo.ParticipanteId (Just participanteId)
    in
    ( { model
        | preferenciasDeGrupos = aplicarPreferencia grupoId cambio model
        , participanteElegido = model.participanteElegido |> Dict.remove grupoId
      }
    , Effect.batch
        [ Effect.savePreferenciaDeGrupo grupoId cambio
        , Store.ensurePagadoYConsumido grupoId participanteId model.store
        ]
    )


reclamadoEn : ULID -> Model -> Maybe ULID
reclamadoEn grupoId model =
    case model.currentUser of
        Success user ->
            Store.getGrupo grupoId model.store
                |> RemoteData.toMaybe
                |> Maybe.andThen (Models.Grupo.ownedParticipante user.id)
                |> Maybe.map .id

        NotAsked ->
            Nothing

        Loading ->
            Nothing

        Failure _ ->
            Nothing


aplicarPreferencia : ULID -> PreferenciasDeGrupo.CambioDePreferencia -> Model -> Dict ULID PreferenciasDeGrupo
aplicarPreferencia grupoId cambio model =
    model.preferenciasDeGrupos
        |> Dict.insert grupoId (PreferenciasDeGrupo.aplicar cambio (preferenciasDeGrupo model grupoId))


preferenciasDeGrupo : Model -> ULID -> PreferenciasDeGrupo
preferenciasDeGrupo model grupoId =
    model.preferenciasDeGrupos
        |> Dict.get grupoId
        |> Maybe.withDefault PreferenciasDeGrupo.vacias


{-| El participante como el que se está usando el grupo: el elegido en esta
sesión, y si no `participantePropio`. Cada mensaje que lo cambia pide lo que
pagó y consumió el nuevo: las páginas lo piden al montarse, y cambiar de
participante no navega a ningún lado.
-}
currentParticipante : Model -> ULID -> Maybe ULID
currentParticipante model grupoId =
    case Dict.get grupoId model.participanteElegido of
        Just enLaSesion ->
            Just enLaSesion

        Nothing ->
            participantePropio model grupoId


{-| Quién sos en el grupo, sin contar lo que se eligió en esta sesión: lo
guardado en el navegador. Con cuenta, en cuanto llega el grupo pasa a ser el que
reclamaste (lo guarda `GrupoCargado`); si no reclamaste a nadie, es lo que
hayas elegido.
-}
participantePropio : Model -> ULID -> Maybe ULID
participantePropio model grupoId =
    (preferenciasDeGrupo model grupoId).participanteId


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
decodeIncomingPortMessage { tag } =
    case tag of
        "SHARE_LINK_COPIED" ->
            Just <| AddToast { level = ToastSuccess, content = "Link copiado al portapapeles" }

        "TEXT_COPIED" ->
            Just <| AddToast { level = ToastSuccess, content = "Copiado al portapapeles" }

        _ ->
            Nothing
