module Generated.Api exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import String
import Url.Builder

type alias CreateGrupoParams  =
   { grupoName: String
   }

jsonDecCreateGrupoParams : Json.Decode.Decoder ( CreateGrupoParams )
jsonDecCreateGrupoParams =
   Json.Decode.succeed (\pgrupoName -> {grupoName = pgrupoName}) |> custom (Json.Decode.string)

jsonEncCreateGrupoParams : CreateGrupoParams -> Value
jsonEncCreateGrupoParams  val =
   Json.Encode.string val.grupoName


type alias ParticipanteAddParams  =
   { name: String
   }

jsonDecParticipanteAddParams : Json.Decode.Decoder ( ParticipanteAddParams )
jsonDecParticipanteAddParams =
   Json.Decode.succeed (\pname -> {name = pname}) |> custom (Json.Decode.string)

jsonEncParticipanteAddParams : ParticipanteAddParams -> Value
jsonEncParticipanteAddParams  val =
   Json.Encode.string val.name


type alias Grupo  =
   { grupoId: ULID
   , grupoNombre: String
   , pagos: (List Pago)
   , participantes: (List Participante)
   }

jsonDecGrupo : Json.Decode.Decoder ( Grupo )
jsonDecGrupo =
   Json.Decode.succeed (\pgrupoId pgrupoNombre ppagos pparticipantes -> {grupoId = pgrupoId, grupoNombre = pgrupoNombre, pagos = ppagos, participantes = pparticipantes})
   |> required "grupoId" (jsonDecULID)
   |> required "grupoNombre" (Json.Decode.string)
   |> required "pagos" (Json.Decode.list (jsonDecPago))
   |> required "participantes" (Json.Decode.list (jsonDecParticipante))

jsonEncGrupo : Grupo -> Value
jsonEncGrupo  val =
   Json.Encode.object
   [ ("grupoId", jsonEncULID val.grupoId)
   , ("grupoNombre", Json.Encode.string val.grupoNombre)
   , ("pagos", (Json.Encode.list jsonEncPago) val.pagos)
   , ("participantes", (Json.Encode.list jsonEncParticipante) val.participantes)
   ]



type alias Participante  =
   { participanteId: ULID
   , participanteNombre: String
   }

jsonDecParticipante : Json.Decode.Decoder ( Participante )
jsonDecParticipante =
   Json.Decode.succeed (\pparticipanteId pparticipanteNombre -> {participanteId = pparticipanteId, participanteNombre = pparticipanteNombre})
   |> required "participanteId" (jsonDecULID)
   |> required "participanteNombre" (Json.Decode.string)

jsonEncParticipante : Participante -> Value
jsonEncParticipante  val =
   Json.Encode.object
   [ ("participanteId", jsonEncULID val.participanteId)
   , ("participanteNombre", Json.Encode.string val.participanteNombre)
   ]



type Transaccion  =
    Transaccion ParticipanteId ParticipanteId Monto

jsonDecTransaccion : Json.Decode.Decoder ( Transaccion )
jsonDecTransaccion =
    Json.Decode.lazy (\_ -> Json.Decode.map3 Transaccion (Json.Decode.index 0 (jsonDecParticipanteId)) (Json.Decode.index 1 (jsonDecParticipanteId)) (Json.Decode.index 2 (jsonDecMonto)))


jsonEncTransaccion : Transaccion -> Value
jsonEncTransaccion (Transaccion v1 v2 v3) =
    Json.Encode.list identity [jsonEncParticipanteId v1, jsonEncParticipanteId v2, jsonEncMonto v3]



type alias Netos  =
   { transaccionesParaSaldar: (List Transaccion)
   , netos: (Deudas Monto)
   }

jsonDecNetos : Json.Decode.Decoder ( Netos )
jsonDecNetos =
   Json.Decode.succeed (\ptransaccionesParaSaldar pnetos -> {transaccionesParaSaldar = ptransaccionesParaSaldar, netos = pnetos})
   |> required "transaccionesParaSaldar" (Json.Decode.list (jsonDecTransaccion))
   |> required "netos" (jsonDecDeudas (jsonDecMonto))

jsonEncNetos : Netos -> Value
jsonEncNetos  val =
   Json.Encode.object
   [ ("transaccionesParaSaldar", (Json.Encode.list jsonEncTransaccion) val.transaccionesParaSaldar)
   , ("netos", (jsonEncDeudas (jsonEncMonto)) val.netos)
   ]



type alias Pago  =
   { pagoId: ULID
   , monto: Monto
   , nombre: String
   , deudores: (List Parte)
   , pagadores: (List Parte)
   }

jsonDecPago : Json.Decode.Decoder ( Pago )
jsonDecPago =
   Json.Decode.succeed (\ppagoId pmonto pnombre pdeudores ppagadores -> {pagoId = ppagoId, monto = pmonto, nombre = pnombre, deudores = pdeudores, pagadores = ppagadores})
   |> required "pagoId" (jsonDecULID)
   |> required "monto" (jsonDecMonto)
   |> required "nombre" (Json.Decode.string)
   |> required "deudores" (Json.Decode.list (jsonDecParte))
   |> required "pagadores" (Json.Decode.list (jsonDecParte))

jsonEncPago : Pago -> Value
jsonEncPago  val =
   Json.Encode.object
   [ ("pagoId", jsonEncULID val.pagoId)
   , ("monto", jsonEncMonto val.monto)
   , ("nombre", Json.Encode.string val.nombre)
   , ("deudores", (Json.Encode.list jsonEncParte) val.deudores)
   , ("pagadores", (Json.Encode.list jsonEncParte) val.pagadores)
   ]



type Parte  =
    MontoFijo Monto ParticipanteId
    | Ponderado Int ParticipanteId

jsonDecParte : Json.Decode.Decoder ( Parte )
jsonDecParte =
    let jsonDecDictParte = Dict.fromList
            [ ("MontoFijo", Json.Decode.lazy (\_ -> Json.Decode.map2 MontoFijo (Json.Decode.index 0 (jsonDecMonto)) (Json.Decode.index 1 (jsonDecParticipanteId))))
            , ("Ponderado", Json.Decode.lazy (\_ -> Json.Decode.map2 Ponderado (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (jsonDecParticipanteId))))
            ]
    in  decodeSumObjectWithSingleField  "Parte" jsonDecDictParte

jsonEncParte : Parte -> Value
jsonEncParte  val =
    let keyval v = case v of
                    MontoFijo v1 v2 -> ("MontoFijo", encodeValue (Json.Encode.list identity [jsonEncMonto v1, jsonEncParticipanteId v2]))
                    Ponderado v1 v2 -> ("Ponderado", encodeValue (Json.Encode.list identity [Json.Encode.int v1, jsonEncParticipanteId v2]))
    in encodeSumObjectWithSingleField keyval val



type alias ParticipanteId  = ULID

jsonDecParticipanteId : Json.Decode.Decoder ( ParticipanteId )
jsonDecParticipanteId =
    jsonDecULID

jsonEncParticipanteId : ParticipanteId -> Value
jsonEncParticipanteId  val = jsonEncULID val



type alias ULID  = String

jsonDecULID : Json.Decode.Decoder ( ULID )
jsonDecULID =
    Json.Decode.string

jsonEncULID : ULID -> Value
jsonEncULID  val = Json.Encode.string val



type alias Deudas a = (List (ParticipanteId, a))

jsonDecDeudas : Json.Decode.Decoder a -> Json.Decode.Decoder ( Deudas a )
jsonDecDeudas localDecoder_a =
    Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecParticipanteId)) (Json.Decode.index 1 (localDecoder_a)))

jsonEncDeudas : (a -> Value) -> Deudas a -> Value
jsonEncDeudas localEncoder_a val = (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(jsonEncParticipanteId) t1,(localEncoder_a) t2])) val



type alias Monto  = (String, Int, Int)

jsonDecMonto : Json.Decode.Decoder ( Monto )
jsonDecMonto =
    Json.Decode.map3 tuple3 (Json.Decode.index 0 (Json.Decode.string)) (Json.Decode.index 1 (Json.Decode.int)) (Json.Decode.index 2 (Json.Decode.int))

jsonEncMonto : Monto -> Value
jsonEncMonto  val = (\(t1,t2,t3) -> Json.Encode.list identity [(Json.Encode.string) t1,(Json.Encode.int) t2,(Json.Encode.int) t3]) val


postGrupo : CreateGrupoParams -> (Result Http.Error  (Grupo)  -> msg) -> Cmd msg
postGrupo body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "/api"
                    [ "grupo"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncCreateGrupoParams body)
            , expect =
                Http.expectJson toMsg jsonDecGrupo
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getGrupoById : ULID -> (Result Http.Error  (Grupo)  -> msg) -> Cmd msg
getGrupoById capture_id toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "/api"
                    [ "grupo"
                    , (capture_id)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecGrupo
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getGrupoByIdNetos : ULID -> (Result Http.Error  (Netos)  -> msg) -> Cmd msg
getGrupoByIdNetos capture_id toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "/api"
                    [ "grupo"
                    , (capture_id)
                    , "netos"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecNetos
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postGrupoByIdParticipantes : ULID -> ParticipanteAddParams -> (Result Http.Error  (Participante)  -> msg) -> Cmd msg
postGrupoByIdParticipantes capture_id body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "/api"
                    [ "grupo"
                    , (capture_id)
                    , "participantes"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncParticipanteAddParams body)
            , expect =
                Http.expectJson toMsg jsonDecParticipante
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postGrupoByIdPagos : ULID -> Pago -> (Result Http.Error  (Pago)  -> msg) -> Cmd msg
postGrupoByIdPagos capture_id body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "/api"
                    [ "grupo"
                    , (capture_id)
                    , "pagos"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncPago body)
            , expect =
                Http.expectJson toMsg jsonDecPago
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postPagos : Pago -> (Result Http.Error  (Netos)  -> msg) -> Cmd msg
postPagos body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "/api"
                    [ "pagos"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncPago body)
            , expect =
                Http.expectJson toMsg jsonDecNetos
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

deleteGrupoByIdParticipantesByParticipanteId : ULID -> ULID -> (Result Http.Error  (ULID)  -> msg) -> Cmd msg
deleteGrupoByIdParticipantesByParticipanteId capture_id capture_participanteId toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "/api"
                    [ "grupo"
                    , (capture_id)
                    , "participantes"
                    , (capture_participanteId)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecULID
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

deleteGrupoByIdPagosByPagoId : ULID -> ULID -> (Result Http.Error  (ULID)  -> msg) -> Cmd msg
deleteGrupoByIdPagosByPagoId capture_id capture_pagoId toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "DELETE"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "/api"
                    [ "grupo"
                    , (capture_id)
                    , "pagos"
                    , (capture_pagoId)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecULID
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putGrupoByIdPagosByPagoId : ULID -> ULID -> Pago -> (Result Http.Error  (Pago)  -> msg) -> Cmd msg
putGrupoByIdPagosByPagoId capture_id capture_pagoId body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "PUT"
            , headers =
                []
            , url =
                Url.Builder.crossOrigin "/api"
                    [ "grupo"
                    , (capture_id)
                    , "pagos"
                    , (capture_pagoId)
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncPago body)
            , expect =
                Http.expectJson toMsg jsonDecPago
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
