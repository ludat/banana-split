module Models.Store exposing (..)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Generated.Api as Api exposing (Grupo, Netos, Repartija, ResumenGrupo, ShallowRepartija, ULID)
import Models.Store.Types exposing (Store, StoreMsg(..))
import RemoteData exposing (RemoteData(..), WebData)
import Shared.Msg exposing (Msg(..))


update : StoreMsg -> Store -> ( Store, Effect Shared.Msg.Msg )
update msg store =
    case msg of
        StoreNoOp string ->
            ( store, Effect.none )

        GrupoFetched grupoId grupo ->
            ( store |> saveGrupo grupoId grupo, Effect.none )

        FetchGrupo grupoId ->
            ( store
            , Effect.batch
                [ Effect.sendCmd <| Api.getGrupoById grupoId (RemoteData.fromResult >> GrupoFetched grupoId >> StoreMsg)
                ]
            )

        ResumenFetched grupoId resumen ->
            ( store |> saveResumen grupoId resumen, Effect.none )

        FetchResumen grupoId ->
            ( store
            , Effect.batch
                [ Effect.sendCmd <| Api.getGrupoByIdResumen grupoId (RemoteData.fromResult >> ResumenFetched grupoId >> StoreMsg)
                ]
            )

        RepartijasFetched grupoId repartijas ->
            ( store |> saveRepartijas grupoId repartijas
            , Effect.batch []
            )

        FetchRepartijas grupoId ->
            ( store
            , Effect.batch
                [ Effect.sendCmd <| Api.getGrupoByIdRepartijas grupoId (RemoteData.fromResult >> RepartijasFetched grupoId >> StoreMsg)
                ]
            )

        RepartijaFetched repartijaId repartija ->
            ( store |> saveRepartija repartijaId repartija
            , Effect.batch []
            )

        FetchRepartija repartijaId ->
            ( store
            , Effect.batch
                [ Effect.sendCmd <| Api.getRepartijasByRepartijaId repartijaId (RemoteData.fromResult >> RepartijaFetched repartijaId >> StoreMsg)
                ]
            )


saveResumen : ULID -> WebData ResumenGrupo -> Store -> Store
saveResumen grupoId resumen store =
    { store
        | resumenes =
            store.resumenes
                |> Dict.insert grupoId resumen
    }


saveGrupo : ULID -> WebData Grupo -> Store -> Store
saveGrupo grupoId grupo store =
    { store
        | grupos =
            store.grupos
                |> Dict.insert grupoId grupo
    }


saveRepartijas : ULID -> WebData (List ShallowRepartija) -> Store -> Store
saveRepartijas grupoId repartijas store =
    { store
        | repartijasPorGrupo =
            store.repartijasPorGrupo
                |> Dict.insert grupoId repartijas
    }


saveRepartija : ULID -> WebData Repartija -> Store -> Store
saveRepartija repartijaId repartija store =
    { store
        | repartijas =
            store.repartijas
                |> Dict.insert repartijaId repartija
    }


refreshGrupo : ULID -> Effect msg
refreshGrupo grupoId =
    Effect.sendStoreMsg <| FetchGrupo grupoId


refreshResumen : ULID -> Effect msg
refreshResumen grupoId =
    Effect.sendStoreMsg <| FetchResumen grupoId


refreshRepartijas : ULID -> Effect msg
refreshRepartijas grupoId =
    Effect.sendStoreMsg <| FetchRepartijas grupoId


refreshRepartija : ULID -> Effect msg
refreshRepartija repartijaId =
    Effect.sendStoreMsg <| FetchRepartija repartijaId


ensureGrupo : ULID -> Store -> Effect msg
ensureGrupo grupoId store =
    case getGrupo grupoId store of
        NotAsked ->
            Effect.sendStoreMsg <| FetchGrupo grupoId

        Loading ->
            Effect.none

        Failure _ ->
            Effect.none

        Success _ ->
            Effect.none


ensureRepartijas : ULID -> Store -> Effect msg
ensureRepartijas grupoId store =
    case getRepartijas grupoId store of
        NotAsked ->
            Effect.sendStoreMsg <| FetchRepartijas grupoId

        Loading ->
            Effect.none

        Failure _ ->
            Effect.none

        Success _ ->
            Effect.none


ensureRepartija : ULID -> Store -> Effect msg
ensureRepartija repartijaId store =
    case getRepartijas repartijaId store of
        NotAsked ->
            Effect.sendStoreMsg <| FetchRepartija repartijaId

        Loading ->
            Effect.none

        Failure _ ->
            Effect.none

        Success _ ->
            Effect.none


ensureResumen : ULID -> Store -> Effect msg
ensureResumen grupoId store =
    case getResumen grupoId store of
        NotAsked ->
            Effect.sendStoreMsg <| FetchResumen grupoId

        Loading ->
            Effect.none

        Failure _ ->
            Effect.none

        Success _ ->
            Effect.none


getGrupo : ULID -> Store -> WebData Grupo
getGrupo grupoId store =
    store.grupos
        |> Dict.get grupoId
        |> Maybe.withDefault NotAsked


getResumen : ULID -> Store -> WebData ResumenGrupo
getResumen grupoId store =
    store.resumenes
        |> Dict.get grupoId
        |> Maybe.withDefault NotAsked


getRepartijas : ULID -> Store -> WebData (List ShallowRepartija)
getRepartijas grupoId store =
    store.repartijasPorGrupo
        |> Dict.get grupoId
        |> Maybe.withDefault NotAsked


getRepartija : ULID -> Store -> WebData Repartija
getRepartija repartijaId store =
    store.repartijas
        |> Dict.get repartijaId
        |> Maybe.withDefault NotAsked


evictGroup : ULID -> Store -> Store
evictGroup grupoId store =
    { store
        | grupos = store.grupos |> Dict.remove grupoId
        , resumenes = store.resumenes |> Dict.remove grupoId
    }


empty : Store
empty =
    { grupos = Dict.empty
    , resumenes = Dict.empty
    , repartijasPorGrupo = Dict.empty
    , repartijas = Dict.empty
    }
