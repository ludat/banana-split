module Models.Store exposing (..)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Generated.Api as Api exposing (Grupo, Netos, Repartija, ShallowRepartija, ULID)
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

        NetosFetched grupoId netos ->
            ( store |> saveNetos grupoId netos, Effect.none )

        FetchNetos grupoId ->
            ( store
            , Effect.batch
                [ Effect.sendCmd <| Api.getGrupoByIdNetos grupoId (RemoteData.fromResult >> NetosFetched grupoId >> StoreMsg)
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


refreshGrupo : ULID -> Store -> Effect msg
refreshGrupo grupoId store =
    Effect.sendStoreMsg <| FetchGrupo grupoId


refreshNetos : ULID -> Store -> Effect msg
refreshNetos grupoId store =
    Effect.sendStoreMsg <| FetchNetos grupoId


refreshRepartijas : ULID -> Store -> Effect msg
refreshRepartijas grupoId store =
    Effect.sendStoreMsg <| FetchRepartijas grupoId


refreshRepartija : ULID -> Store -> Effect msg
refreshRepartija repartijaId store =
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
        , netos = store.netos |> Dict.remove grupoId
    }


empty : Store
empty =
    { grupos = Dict.empty
    , netos = Dict.empty
    , repartijasPorGrupo = Dict.empty
    , repartijas = Dict.empty
    }
