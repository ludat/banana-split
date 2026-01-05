module Models.Store exposing (..)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Generated.Api as Api exposing (Grupo, Netos, Pago, Repartija, ResumenGrupo, ShallowGrupo, ShallowPago, ULID)
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

        PagosFetched grupoId pagos ->
            ( store |> savePagos grupoId pagos
            , Effect.none
            )

        FetchPagos grupoId ->
            ( store
            , Effect.batch
                [ Effect.sendCmd <| Api.getGrupoByIdPagos grupoId (RemoteData.fromResult >> PagosFetched grupoId >> StoreMsg)
                ]
            )

        PagoFetched pagoId pago ->
            ( store |> savePago pagoId pago
            , Effect.none
            )

        FetchPago pagoId ->
            ( store
            , Effect.batch
                -- TODO This should be a grupo id instead of pagoId
                [ Effect.sendCmd <| Api.getGrupoByIdPagosByPagoId pagoId pagoId (RemoteData.fromResult >> PagoFetched pagoId >> StoreMsg)
                ]
            )

        RepartijaFetched repartijaId repartija ->
            ( store |> saveRepartija repartijaId repartija
            , Effect.none
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


saveGrupo : ULID -> WebData ShallowGrupo -> Store -> Store
saveGrupo grupoId grupo store =
    { store
        | grupos =
            store.grupos
                |> Dict.insert grupoId grupo
    }


savePagos : ULID -> WebData (List ShallowPago) -> Store -> Store
savePagos grupoId pagos store =
    { store
        | pagosPorGrupo =
            store.pagosPorGrupo
                |> Dict.insert grupoId pagos
    }


savePago : ULID -> WebData Pago -> Store -> Store
savePago pagoId pago store =
    { store
        | pagos =
            store.pagos
                |> Dict.insert pagoId pago
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


updateRepartija : ULID -> Repartija -> Effect msg
updateRepartija repartijaId repartija =
    Effect.sendStoreMsg <| RepartijaFetched repartijaId (Success repartija)


refreshPagos : ULID -> Effect msg
refreshPagos grupoId =
    Effect.sendStoreMsg <| FetchPagos grupoId


refreshPago : ULID -> Effect msg
refreshPago pagoId =
    Effect.sendStoreMsg <| FetchPago pagoId


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


ensurePagos : ULID -> Store -> Effect msg
ensurePagos grupoId store =
    case getPagos grupoId store of
        NotAsked ->
            Effect.sendStoreMsg <| FetchPagos grupoId

        Loading ->
            Effect.none

        Failure _ ->
            Effect.none

        Success _ ->
            Effect.none


ensurePago : ULID -> Store -> Effect msg
ensurePago pagoId store =
    case getPagos pagoId store of
        NotAsked ->
            Effect.sendStoreMsg <| FetchPago pagoId

        Loading ->
            Effect.none

        Failure _ ->
            Effect.none

        Success _ ->
            Effect.none


ensureRepartija : ULID -> Store -> Effect msg
ensureRepartija repartijaId store =
    case getRepartija repartijaId store of
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


getGrupo : ULID -> Store -> WebData ShallowGrupo
getGrupo grupoId store =
    store.grupos
        |> Dict.get grupoId
        |> Maybe.withDefault NotAsked


getResumen : ULID -> Store -> WebData ResumenGrupo
getResumen grupoId store =
    store.resumenes
        |> Dict.get grupoId
        |> Maybe.withDefault NotAsked


getPagos : ULID -> Store -> WebData (List ShallowPago)
getPagos grupoId store =
    store.pagosPorGrupo
        |> Dict.get grupoId
        |> Maybe.withDefault NotAsked


getPago : ULID -> Store -> WebData Pago
getPago pagoId store =
    store.pagos
        |> Dict.get pagoId
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
        , pagosPorGrupo = store.pagosPorGrupo |> Dict.remove grupoId
    }


empty : Store
empty =
    { grupos = Dict.empty
    , repartijas = Dict.empty
    , resumenes = Dict.empty
    , pagosPorGrupo = Dict.empty
    , pagos = Dict.empty
    }
