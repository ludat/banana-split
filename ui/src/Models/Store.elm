module Models.Store exposing (..)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Generated.Api as Api exposing (Grupo, Netos, ULID)
import Models.Store.Types exposing (Store, StoreMsg(..))
import RemoteData exposing (RemoteData(..), WebData)
import Shared.Msg exposing (Msg(..))


update : StoreMsg -> Store -> ( Store, Effect Shared.Msg.Msg )
update msg store =
    case msg of
        GrupoFetched grupoId grupo ->
            ( store |> saveGrupo grupoId grupo, Effect.none )

        FetchGrupo grupoId ->
            ( store
            , Effect.batch
                [ Effect.sendCmd <| Api.getGrupoById grupoId (RemoteData.fromResult >> GrupoFetched grupoId >> StoreMsg)
                ]
            )

        NetosFetched grupoId netos ->
            ( store |> saveNetos grupoId netos, Effect.none )

        FetchNetos grupoId ->
            ( store
            , Effect.batch
                [ Effect.sendCmd <| Api.getGrupoByIdNetos grupoId (RemoteData.fromResult >> NetosFetched grupoId >> StoreMsg)
                ]
            )

        StoreNoOp string ->
            ( store, Effect.none )


saveNetos : ULID -> WebData Netos -> Store -> Store
saveNetos grupoId netos store =
    { store
        | netos =
            store.netos
                |> Dict.insert grupoId netos
    }


saveGrupo : ULID -> WebData Grupo -> Store -> Store
saveGrupo grupoId grupo store =
    { store
        | grupos =
            store.grupos
                |> Dict.insert grupoId grupo
    }


refreshGrupo : ULID -> Store -> Effect msg
refreshGrupo grupoId store =
    Effect.sendStoreMsg <| FetchGrupo grupoId


refreshNetos : ULID -> Store -> Effect msg
refreshNetos grupoId store =
    Effect.sendStoreMsg <| FetchNetos grupoId


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


ensureNetos : ULID -> Store -> Effect msg
ensureNetos grupoId store =
    case getNetos grupoId store of
        NotAsked ->
            Effect.sendStoreMsg <| FetchNetos grupoId

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


getNetos : ULID -> Store -> WebData Netos
getNetos grupoId store =
    store.netos
        |> Dict.get grupoId
        |> Maybe.withDefault NotAsked


evictGroup : ULID -> Store -> Store
evictGroup grupoId store =
    { store
        | grupos =
            store.grupos
                |> Dict.remove grupoId
    }


empty : Store
empty =
    { grupos = Dict.empty
    , netos = Dict.empty
    }
