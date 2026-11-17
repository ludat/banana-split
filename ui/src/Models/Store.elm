module Models.Store exposing (empty, ensureGrupo, ensurePagadoYConsumido, ensurePagos, ensureResumen, ensureTransferencias, getGrupo, getPagadoYConsumido, getPago, getPagos, getRepartija, getResumen, getTransferencias, invalidatePagos, invalidateResumen, refreshGrupo, refreshPagadoYConsumido, refreshPago, refreshPagos, refreshRepartija, refreshResumen, refreshTransferencias, removeTransferencia, setGrupo, setPago, setTransferencia, update, updateRepartijaForFrontend)

import Dict exposing (Dict)
import Effect exposing (Effect)
import Generated.Api as Api exposing (Grupo, PagadoYConsumido, Pago, RepartijaForFrontend, ResumenGrupo, ShallowPago, Transferencia, ULID)
import Models.Store.Types exposing (Store, StoreMsg(..))
import RemoteData exposing (RemoteData(..), WebData)
import Shared.Msg exposing (Msg(..))
import Utils.Telemetry exposing (trackDecodeErrors)


update : StoreMsg -> Store -> ( Store, Effect Shared.Msg.Msg )
update msg store =
    case msg of
        GrupoFetched grupoId grupo ->
            ( store |> saveGrupo grupoId grupo
            , Effect.batch
                [ Effect.telemetryEvent <| trackDecodeErrors "getGrupoById" grupo
                , case grupo of
                    Success _ ->
                        Effect.sendSharedMsg (GrupoCargado grupoId)

                    NotAsked ->
                        Effect.none

                    Loading ->
                        Effect.none

                    Failure _ ->
                        Effect.none
                ]
            )

        FetchGrupo grupoId ->
            ( store
            , Effect.batch
                [ Effect.sendCmd <| Api.getGrupoById grupoId (RemoteData.fromResult >> GrupoFetched grupoId >> StoreMsg)
                ]
            )

        ResumenFetched grupoId resumen ->
            ( store |> saveResumen grupoId resumen
            , Effect.telemetryEvent <| trackDecodeErrors "getGrupoByIdResumen" resumen
            )

        FetchResumen grupoId ->
            ( store
            , Effect.batch
                [ Effect.sendCmd <| Api.getGrupoByIdResumen grupoId (RemoteData.fromResult >> ResumenFetched grupoId >> StoreMsg)
                ]
            )

        InvalidateResumen grupoId ->
            ( { store | resumenes = store.resumenes |> Dict.remove grupoId }, Effect.none )

        PagosFetched grupoId pagos ->
            ( store |> savePagos grupoId pagos
            , Effect.telemetryEvent <| trackDecodeErrors "getGrupoByIdPagos" pagos
            )

        FetchPagos grupoId ->
            -- Si se vuelven a pedir es que algún gasto cambió, y con él lo que
            -- pagó y consumió cada uno.
            ( { store | pagadoYConsumidoPorParticipante = store.pagadoYConsumidoPorParticipante |> sinLosDelGrupo grupoId }
            , Effect.sendCmd <| Api.getGrupoByIdPagos grupoId (RemoteData.fromResult >> PagosFetched grupoId >> StoreMsg)
            )

        InvalidatePagos grupoId ->
            ( { store
                | pagosPorGrupo = store.pagosPorGrupo |> Dict.remove grupoId
                , pagadoYConsumidoPorParticipante = store.pagadoYConsumidoPorParticipante |> sinLosDelGrupo grupoId
              }
            , Effect.none
            )

        PagadoYConsumidoFetched grupoId participanteId pagadoYConsumido ->
            ( { store
                | pagadoYConsumidoPorParticipante =
                    store.pagadoYConsumidoPorParticipante
                        |> Dict.insert ( grupoId, participanteId )
                            (pagadoYConsumido |> RemoteData.map (List.map (\delGasto -> ( delGasto.pagoId, delGasto )) >> Dict.fromList))
              }
            , Effect.telemetryEvent <| trackDecodeErrors "getGrupoByIdParticipantesByParticipanteIdPagadoyconsumido" pagadoYConsumido
            )

        FetchPagadoYConsumido grupoId participanteId ->
            -- Marcarlo hace que un segundo 'ensure' antes de la respuesta no lo
            -- vuelva a pedir.
            ( { store
                | pagadoYConsumidoPorParticipante =
                    store.pagadoYConsumidoPorParticipante
                        |> Dict.update ( grupoId, participanteId ) (Maybe.withDefault Loading >> Just)
              }
            , Effect.sendCmd <|
                Api.getGrupoByIdParticipantesByParticipanteIdPagadoyconsumido grupoId
                    participanteId
                    (RemoteData.fromResult >> PagadoYConsumidoFetched grupoId participanteId >> StoreMsg)
            )

        TransferenciasFetched grupoId transferencias ->
            ( { store | transferenciasPorGrupo = store.transferenciasPorGrupo |> Dict.insert grupoId transferencias }
            , Effect.telemetryEvent <| trackDecodeErrors "getGrupoByIdTransferencias" transferencias
            )

        FetchTransferencias grupoId ->
            ( store
            , Effect.sendCmd <| Api.getGrupoByIdTransferencias grupoId (RemoteData.fromResult >> TransferenciasFetched grupoId >> StoreMsg)
            )

        TransferenciaActualizada grupoId transferencia ->
            ( store
                |> mapTransferencias grupoId
                    (\transferencias ->
                        if List.any (\t -> t.id == transferencia.id) transferencias then
                            transferencias
                                |> List.map
                                    (\t ->
                                        if t.id == transferencia.id then
                                            transferencia

                                        else
                                            t
                                    )

                        else
                            transferencias ++ [ transferencia ]
                    )
            , Effect.none
            )

        TransferenciaBorrada grupoId transferenciaId ->
            ( store |> mapTransferencias grupoId (List.filter (\t -> t.id /= transferenciaId))
            , Effect.none
            )

        PagoFetched pagoId pago ->
            ( store |> savePago pagoId pago
            , Effect.telemetryEvent <| trackDecodeErrors "getGrupoByIdPagosByPagoId" pago
            )

        FetchPago grupoId pagoId ->
            ( store
            , Effect.batch
                [ Effect.sendCmd <| Api.getGrupoByIdPagosByPagoId grupoId pagoId (RemoteData.fromResult >> PagoFetched pagoId >> StoreMsg)
                ]
            )

        RepartijaFetched repartijaId repartija ->
            ( store |> saveRepartija repartijaId repartija
            , Effect.telemetryEvent <| trackDecodeErrors "getRepartijasByRepartijaId" repartija
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


{-| Si la lista no está cargada no hay nada que actualizar: cuando alguien la
pida va a venir ya con el cambio.
-}
mapTransferencias : ULID -> (List Transferencia -> List Transferencia) -> Store -> Store
mapTransferencias grupoId f store =
    { store
        | transferenciasPorGrupo =
            store.transferenciasPorGrupo
                |> Dict.update grupoId (Maybe.map (RemoteData.map f))
    }


saveGrupo : ULID -> WebData Grupo -> Store -> Store
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


setGrupo : ULID -> Grupo -> Effect msg
setGrupo grupoId grupo =
    Effect.sendStoreMsg <| GrupoFetched grupoId (Success grupo)


setPago : ULID -> Pago -> Effect msg
setPago pagoId pago =
    Effect.sendStoreMsg <| PagoFetched pagoId (Success pago)


saveRepartija : ULID -> WebData RepartijaForFrontend -> Store -> Store
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


updateRepartijaForFrontend : ULID -> RepartijaForFrontend -> Effect msg
updateRepartijaForFrontend repartijaId repartijaPage =
    Effect.sendStoreMsg <| RepartijaFetched repartijaId (Success repartijaPage)


refreshPagos : ULID -> Effect msg
refreshPagos grupoId =
    Effect.sendStoreMsg <| FetchPagos grupoId


refreshPagadoYConsumido : ULID -> ULID -> Effect msg
refreshPagadoYConsumido grupoId participanteId =
    Effect.sendStoreMsg <| FetchPagadoYConsumido grupoId participanteId


refreshPago : ULID -> ULID -> Effect msg
refreshPago grupoId pagoId =
    Effect.sendStoreMsg <| FetchPago grupoId pagoId


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
            refreshPagos grupoId

        Loading ->
            Effect.none

        Failure _ ->
            Effect.none

        Success _ ->
            Effect.none


ensurePagadoYConsumido : ULID -> ULID -> Store -> Effect msg
ensurePagadoYConsumido grupoId participanteId store =
    case getPagadoYConsumido grupoId participanteId store of
        NotAsked ->
            refreshPagadoYConsumido grupoId participanteId

        Loading ->
            Effect.none

        Failure _ ->
            Effect.none

        Success _ ->
            Effect.none



--ensureRepartija : ULID -> Store -> Effect msg
--ensureRepartija repartijaId store =
--    case getRepartija repartijaId store of
--        NotAsked ->
--            Effect.sendStoreMsg <| FetchRepartija repartijaId
--
--        Loading ->
--            Effect.none
--
--        Failure _ ->
--            Effect.none
--
--        Success _ ->
--            Effect.none


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


ensureTransferencias : ULID -> Store -> Effect msg
ensureTransferencias grupoId store =
    case getTransferencias grupoId store of
        NotAsked ->
            Effect.sendStoreMsg <| FetchTransferencias grupoId

        Loading ->
            Effect.none

        Failure _ ->
            Effect.none

        Success _ ->
            Effect.none


refreshTransferencias : ULID -> Effect msg
refreshTransferencias grupoId =
    Effect.sendStoreMsg <| FetchTransferencias grupoId


setTransferencia : ULID -> Transferencia -> Effect msg
setTransferencia grupoId transferencia =
    Effect.sendStoreMsg <| TransferenciaActualizada grupoId transferencia


removeTransferencia : ULID -> ULID -> Effect msg
removeTransferencia grupoId transferenciaId =
    Effect.sendStoreMsg <| TransferenciaBorrada grupoId transferenciaId


getTransferencias : ULID -> Store -> WebData (List Transferencia)
getTransferencias grupoId store =
    store.transferenciasPorGrupo
        |> Dict.get grupoId
        |> Maybe.withDefault NotAsked


invalidateResumen : ULID -> Effect msg
invalidateResumen grupoId =
    Effect.sendStoreMsg <| InvalidateResumen grupoId


invalidatePagos : ULID -> Effect msg
invalidatePagos grupoId =
    Effect.sendStoreMsg <| InvalidatePagos grupoId


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


getPagos : ULID -> Store -> WebData (List ShallowPago)
getPagos grupoId store =
    store.pagosPorGrupo
        |> Dict.get grupoId
        |> Maybe.withDefault NotAsked


{-| Por gasto. Un gasto que no está es uno donde el participante no puso ni
consumió nada.
-}
getPagadoYConsumido : ULID -> ULID -> Store -> WebData (Dict ULID PagadoYConsumido)
getPagadoYConsumido grupoId participanteId store =
    store.pagadoYConsumidoPorParticipante
        |> Dict.get ( grupoId, participanteId )
        |> Maybe.withDefault NotAsked


sinLosDelGrupo : ULID -> Dict ( ULID, ULID ) a -> Dict ( ULID, ULID ) a
sinLosDelGrupo grupoId =
    Dict.filter (\( deGrupo, _ ) _ -> deGrupo /= grupoId)


getPago : ULID -> Store -> WebData Pago
getPago pagoId store =
    store.pagos
        |> Dict.get pagoId
        |> Maybe.withDefault NotAsked


getRepartija : ULID -> Store -> WebData RepartijaForFrontend
getRepartija repartijaId store =
    store.repartijas
        |> Dict.get repartijaId
        |> Maybe.withDefault NotAsked


empty : Store
empty =
    { grupos = Dict.empty
    , repartijas = Dict.empty
    , resumenes = Dict.empty
    , pagosPorGrupo = Dict.empty
    , pagadoYConsumidoPorParticipante = Dict.empty
    , transferenciasPorGrupo = Dict.empty
    , pagos = Dict.empty
    }
