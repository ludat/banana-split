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
   , grupoParticipante: String
   }

jsonDecCreateGrupoParams : Json.Decode.Decoder ( CreateGrupoParams )
jsonDecCreateGrupoParams =
   Json.Decode.succeed (\pgrupoName pgrupoParticipante -> {grupoName = pgrupoName, grupoParticipante = pgrupoParticipante})
   |> required "grupoName" (Json.Decode.string)
   |> required "grupoParticipante" (Json.Decode.string)

jsonEncCreateGrupoParams : CreateGrupoParams -> Value
jsonEncCreateGrupoParams  val =
   Json.Encode.object
   [ ("grupoName", Json.Encode.string val.grupoName)
   , ("grupoParticipante", Json.Encode.string val.grupoParticipante)
   ]



type alias ReceiptImageRequest  =
   { imageBase64: String
   }

jsonDecReceiptImageRequest : Json.Decode.Decoder ( ReceiptImageRequest )
jsonDecReceiptImageRequest =
   Json.Decode.succeed (\pimageBase64 -> {imageBase64 = pimageBase64}) |> custom (Json.Decode.string)

jsonEncReceiptImageRequest : ReceiptImageRequest -> Value
jsonEncReceiptImageRequest  val =
   Json.Encode.string val.imageBase64


type ReceiptImageResponse  =
    ReceiptImageSuccess {items: (List RepartijaItem)}
    | ReceiptImageError {error: String}

jsonDecReceiptImageResponse : Json.Decode.Decoder ( ReceiptImageResponse )
jsonDecReceiptImageResponse =
    let jsonDecDictReceiptImageResponse = Dict.fromList
            [ ("ReceiptImageSuccess", Json.Decode.lazy (\_ -> Json.Decode.map ReceiptImageSuccess (   Json.Decode.succeed (\pitems -> {items = pitems})    |> required "items" (Json.Decode.list (jsonDecRepartijaItem)))))
            , ("ReceiptImageError", Json.Decode.lazy (\_ -> Json.Decode.map ReceiptImageError (   Json.Decode.succeed (\perror -> {error = perror})    |> required "error" (Json.Decode.string))))
            ]
    in  decodeSumObjectWithSingleField  "ReceiptImageResponse" jsonDecDictReceiptImageResponse

jsonEncReceiptImageResponse : ReceiptImageResponse -> Value
jsonEncReceiptImageResponse  val =
    let keyval v = case v of
                    ReceiptImageSuccess vs -> ("ReceiptImageSuccess", encodeObject [("items", (Json.Encode.list jsonEncRepartijaItem) vs.items)])
                    ReceiptImageError vs -> ("ReceiptImageError", encodeObject [("error", Json.Encode.string vs.error)])
    in encodeSumObjectWithSingleField keyval val



type alias ParticipanteAddParams  =
   { name: String
   }

jsonDecParticipanteAddParams : Json.Decode.Decoder ( ParticipanteAddParams )
jsonDecParticipanteAddParams =
   Json.Decode.succeed (\pname -> {name = pname}) |> custom (Json.Decode.string)

jsonEncParticipanteAddParams : ParticipanteAddParams -> Value
jsonEncParticipanteAddParams  val =
   Json.Encode.string val.name


type alias ResumenGrupo  =
   { transaccionesParaSaldar: (List Transaccion)
   , netos: (Netos Monto)
   , cantidadPagosInvalidos: Int
   , cantidadPagos: Int
   , isFrozen: Bool
   }

jsonDecResumenGrupo : Json.Decode.Decoder ( ResumenGrupo )
jsonDecResumenGrupo =
   Json.Decode.succeed (\ptransaccionesParaSaldar pnetos pcantidadPagosInvalidos pcantidadPagos pisFrozen -> {transaccionesParaSaldar = ptransaccionesParaSaldar, netos = pnetos, cantidadPagosInvalidos = pcantidadPagosInvalidos, cantidadPagos = pcantidadPagos, isFrozen = pisFrozen})
   |> required "transaccionesParaSaldar" (Json.Decode.list (jsonDecTransaccion))
   |> required "netos" (jsonDecNetos (jsonDecMonto))
   |> required "cantidadPagosInvalidos" (Json.Decode.int)
   |> required "cantidadPagos" (Json.Decode.int)
   |> required "isFrozen" (Json.Decode.bool)

jsonEncResumenGrupo : ResumenGrupo -> Value
jsonEncResumenGrupo  val =
   Json.Encode.object
   [ ("transaccionesParaSaldar", (Json.Encode.list jsonEncTransaccion) val.transaccionesParaSaldar)
   , ("netos", (jsonEncNetos (jsonEncMonto)) val.netos)
   , ("cantidadPagosInvalidos", Json.Encode.int val.cantidadPagosInvalidos)
   , ("cantidadPagos", Json.Encode.int val.cantidadPagos)
   , ("isFrozen", Json.Encode.bool val.isFrozen)
   ]



type alias Netos a = (List (ParticipanteId, a))

jsonDecNetos : Json.Decode.Decoder a -> Json.Decode.Decoder ( Netos a )
jsonDecNetos localDecoder_a =
    Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecParticipanteId)) (Json.Decode.index 1 (localDecoder_a)))

jsonEncNetos : (a -> Value) -> Netos a -> Value
jsonEncNetos localEncoder_a val = (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(jsonEncParticipanteId) t1,(localEncoder_a) t2])) val



type alias ResumenPago  =
   { resumen: ResumenNetos
   , resumenPagadores: ResumenNetos
   , resumenDeudores: ResumenNetos
   }

jsonDecResumenPago : Json.Decode.Decoder ( ResumenPago )
jsonDecResumenPago =
   Json.Decode.succeed (\presumen presumenPagadores presumenDeudores -> {resumen = presumen, resumenPagadores = presumenPagadores, resumenDeudores = presumenDeudores})
   |> required "resumen" (jsonDecResumenNetos)
   |> required "resumenPagadores" (jsonDecResumenNetos)
   |> required "resumenDeudores" (jsonDecResumenNetos)

jsonEncResumenPago : ResumenPago -> Value
jsonEncResumenPago  val =
   Json.Encode.object
   [ ("resumen", jsonEncResumenNetos val.resumen)
   , ("resumenPagadores", jsonEncResumenNetos val.resumenPagadores)
   , ("resumenDeudores", jsonEncResumenNetos val.resumenDeudores)
   ]



type alias ResumenNetos  =
   { total: Monto
   , netos: (Netos Monto)
   , errores: (List ErrorResumen)
   }

jsonDecResumenNetos : Json.Decode.Decoder ( ResumenNetos )
jsonDecResumenNetos =
   Json.Decode.succeed (\ptotal pnetos perrores -> {total = ptotal, netos = pnetos, errores = perrores})
   |> required "total" (jsonDecMonto)
   |> required "netos" (jsonDecNetos (jsonDecMonto))
   |> required "errores" (Json.Decode.list (jsonDecErrorResumen))

jsonEncResumenNetos : ResumenNetos -> Value
jsonEncResumenNetos  val =
   Json.Encode.object
   [ ("total", jsonEncMonto val.total)
   , ("netos", (jsonEncNetos (jsonEncMonto)) val.netos)
   , ("errores", (Json.Encode.list jsonEncErrorResumen) val.errores)
   ]



type TipoErrorResumen  =
    ErrorMontosEspecificosVacios 
    | ErrorMontosEspecificosTotalNoCoincide Monto Monto
    | ErrorEquitativoSinParticipantes 
    | ErrorRepartijaSinItems 
    | ErrorRepartijaTotalItemsNoCoincide Monto Monto
    | ErrorRepartijaSinClaims 
    | ErrorRepartijaTotalReclamadoNoCoincide Monto Monto

jsonDecTipoErrorResumen : Json.Decode.Decoder ( TipoErrorResumen )
jsonDecTipoErrorResumen =
    let jsonDecDictTipoErrorResumen = Dict.fromList
            [ ("ErrorMontosEspecificosVacios", Json.Decode.lazy (\_ -> Json.Decode.succeed ErrorMontosEspecificosVacios))
            , ("ErrorMontosEspecificosTotalNoCoincide", Json.Decode.lazy (\_ -> Json.Decode.map2 ErrorMontosEspecificosTotalNoCoincide (Json.Decode.index 0 (jsonDecMonto)) (Json.Decode.index 1 (jsonDecMonto))))
            , ("ErrorEquitativoSinParticipantes", Json.Decode.lazy (\_ -> Json.Decode.succeed ErrorEquitativoSinParticipantes))
            , ("ErrorRepartijaSinItems", Json.Decode.lazy (\_ -> Json.Decode.succeed ErrorRepartijaSinItems))
            , ("ErrorRepartijaTotalItemsNoCoincide", Json.Decode.lazy (\_ -> Json.Decode.map2 ErrorRepartijaTotalItemsNoCoincide (Json.Decode.index 0 (jsonDecMonto)) (Json.Decode.index 1 (jsonDecMonto))))
            , ("ErrorRepartijaSinClaims", Json.Decode.lazy (\_ -> Json.Decode.succeed ErrorRepartijaSinClaims))
            , ("ErrorRepartijaTotalReclamadoNoCoincide", Json.Decode.lazy (\_ -> Json.Decode.map2 ErrorRepartijaTotalReclamadoNoCoincide (Json.Decode.index 0 (jsonDecMonto)) (Json.Decode.index 1 (jsonDecMonto))))
            ]
    in  decodeSumObjectWithSingleField  "TipoErrorResumen" jsonDecDictTipoErrorResumen

jsonEncTipoErrorResumen : TipoErrorResumen -> Value
jsonEncTipoErrorResumen  val =
    let keyval v = case v of
                    ErrorMontosEspecificosVacios  -> ("ErrorMontosEspecificosVacios", encodeValue (Json.Encode.list identity []))
                    ErrorMontosEspecificosTotalNoCoincide v1 v2 -> ("ErrorMontosEspecificosTotalNoCoincide", encodeValue (Json.Encode.list identity [jsonEncMonto v1, jsonEncMonto v2]))
                    ErrorEquitativoSinParticipantes  -> ("ErrorEquitativoSinParticipantes", encodeValue (Json.Encode.list identity []))
                    ErrorRepartijaSinItems  -> ("ErrorRepartijaSinItems", encodeValue (Json.Encode.list identity []))
                    ErrorRepartijaTotalItemsNoCoincide v1 v2 -> ("ErrorRepartijaTotalItemsNoCoincide", encodeValue (Json.Encode.list identity [jsonEncMonto v1, jsonEncMonto v2]))
                    ErrorRepartijaSinClaims  -> ("ErrorRepartijaSinClaims", encodeValue (Json.Encode.list identity []))
                    ErrorRepartijaTotalReclamadoNoCoincide v1 v2 -> ("ErrorRepartijaTotalReclamadoNoCoincide", encodeValue (Json.Encode.list identity [jsonEncMonto v1, jsonEncMonto v2]))
    in encodeSumObjectWithSingleField keyval val



type alias ErrorResumen  =
   { objeto: (List String)
   , tipo: TipoErrorResumen
   }

jsonDecErrorResumen : Json.Decode.Decoder ( ErrorResumen )
jsonDecErrorResumen =
   Json.Decode.succeed (\pobjeto ptipo -> {objeto = pobjeto, tipo = ptipo})
   |> required "objeto" (Json.Decode.list (Json.Decode.string))
   |> required "tipo" (jsonDecTipoErrorResumen)

jsonEncErrorResumen : ErrorResumen -> Value
jsonEncErrorResumen  val =
   Json.Encode.object
   [ ("objeto", (Json.Encode.list Json.Encode.string) val.objeto)
   , ("tipo", jsonEncTipoErrorResumen val.tipo)
   ]



type alias Grupo  =
   { id: ULID
   , nombre: String
   , pagos: (List Pago)
   , participantes: (List Participante)
   }

jsonDecGrupo : Json.Decode.Decoder ( Grupo )
jsonDecGrupo =
   Json.Decode.succeed (\pid pnombre ppagos pparticipantes -> {id = pid, nombre = pnombre, pagos = ppagos, participantes = pparticipantes})
   |> required "id" (jsonDecULID)
   |> required "nombre" (Json.Decode.string)
   |> required "pagos" (Json.Decode.list (jsonDecPago))
   |> required "participantes" (Json.Decode.list (jsonDecParticipante))

jsonEncGrupo : Grupo -> Value
jsonEncGrupo  val =
   Json.Encode.object
   [ ("id", jsonEncULID val.id)
   , ("nombre", Json.Encode.string val.nombre)
   , ("pagos", (Json.Encode.list jsonEncPago) val.pagos)
   , ("participantes", (Json.Encode.list jsonEncParticipante) val.participantes)
   ]



type alias ShallowGrupo  =
   { id: ULID
   , nombre: String
   , participantes: (List Participante)
   , isFrozen: Bool
   }

jsonDecShallowGrupo : Json.Decode.Decoder ( ShallowGrupo )
jsonDecShallowGrupo =
   Json.Decode.succeed (\pid pnombre pparticipantes pisFrozen -> {id = pid, nombre = pnombre, participantes = pparticipantes, isFrozen = pisFrozen})
   |> required "id" (jsonDecULID)
   |> required "nombre" (Json.Decode.string)
   |> required "participantes" (Json.Decode.list (jsonDecParticipante))
   |> required "isFrozen" (Json.Decode.bool)

jsonEncShallowGrupo : ShallowGrupo -> Value
jsonEncShallowGrupo  val =
   Json.Encode.object
   [ ("id", jsonEncULID val.id)
   , ("nombre", Json.Encode.string val.nombre)
   , ("participantes", (Json.Encode.list jsonEncParticipante) val.participantes)
   , ("isFrozen", Json.Encode.bool val.isFrozen)
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



type alias Transaccion  =
   { id: (Maybe ULID)
   , from: ParticipanteId
   , to: ParticipanteId
   , monto: Monto
   }

jsonDecTransaccion : Json.Decode.Decoder ( Transaccion )
jsonDecTransaccion =
   Json.Decode.succeed (\pid pfrom pto pmonto -> {id = pid, from = pfrom, to = pto, monto = pmonto})
   |> fnullable "id" (jsonDecULID)
   |> required "from" (jsonDecParticipanteId)
   |> required "to" (jsonDecParticipanteId)
   |> required "monto" (jsonDecMonto)

jsonEncTransaccion : Transaccion -> Value
jsonEncTransaccion  val =
   Json.Encode.object
   [ ("id", (maybeEncode (jsonEncULID)) val.id)
   , ("from", jsonEncParticipanteId val.from)
   , ("to", jsonEncParticipanteId val.to)
   , ("monto", jsonEncMonto val.monto)
   ]



type alias Pago  =
   { pagoId: ULID
   , monto: Monto
   , isValid: Bool
   , nombre: String
   , pagadores: Distribucion
   , deudores: Distribucion
   }

jsonDecPago : Json.Decode.Decoder ( Pago )
jsonDecPago =
   Json.Decode.succeed (\ppagoId pmonto pisValid pnombre ppagadores pdeudores -> {pagoId = ppagoId, monto = pmonto, isValid = pisValid, nombre = pnombre, pagadores = ppagadores, deudores = pdeudores})
   |> required "pagoId" (jsonDecULID)
   |> required "monto" (jsonDecMonto)
   |> required "isValid" (Json.Decode.bool)
   |> required "nombre" (Json.Decode.string)
   |> required "pagadores" (jsonDecDistribucion)
   |> required "deudores" (jsonDecDistribucion)

jsonEncPago : Pago -> Value
jsonEncPago  val =
   Json.Encode.object
   [ ("pagoId", jsonEncULID val.pagoId)
   , ("monto", jsonEncMonto val.monto)
   , ("isValid", Json.Encode.bool val.isValid)
   , ("nombre", Json.Encode.string val.nombre)
   , ("pagadores", jsonEncDistribucion val.pagadores)
   , ("deudores", jsonEncDistribucion val.deudores)
   ]



type alias ShallowPago  =
   { pagoId: ULID
   , isValid: Bool
   , nombre: String
   , monto: Monto
   }

jsonDecShallowPago : Json.Decode.Decoder ( ShallowPago )
jsonDecShallowPago =
   Json.Decode.succeed (\ppagoId pisValid pnombre pmonto -> {pagoId = ppagoId, isValid = pisValid, nombre = pnombre, monto = pmonto})
   |> required "pagoId" (jsonDecULID)
   |> required "isValid" (Json.Decode.bool)
   |> required "nombre" (Json.Decode.string)
   |> required "monto" (jsonDecMonto)

jsonEncShallowPago : ShallowPago -> Value
jsonEncShallowPago  val =
   Json.Encode.object
   [ ("pagoId", jsonEncULID val.pagoId)
   , ("isValid", Json.Encode.bool val.isValid)
   , ("nombre", Json.Encode.string val.nombre)
   , ("monto", jsonEncMonto val.monto)
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



type alias Monto  =
   { lugaresDespuesDeLaComa: Int
   , valor: Int
   }

jsonDecMonto : Json.Decode.Decoder ( Monto )
jsonDecMonto =
   Json.Decode.succeed (\plugaresDespuesDeLaComa pvalor -> {lugaresDespuesDeLaComa = plugaresDespuesDeLaComa, valor = pvalor})
   |> required "lugaresDespuesDeLaComa" (Json.Decode.int)
   |> required "valor" (Json.Decode.int)

jsonEncMonto : Monto -> Value
jsonEncMonto  val =
   Json.Encode.object
   [ ("lugaresDespuesDeLaComa", Json.Encode.int val.lugaresDespuesDeLaComa)
   , ("valor", Json.Encode.int val.valor)
   ]



type alias Distribucion  =
   { id: ULID
   , tipo: TipoDistribucion
   }

jsonDecDistribucion : Json.Decode.Decoder ( Distribucion )
jsonDecDistribucion =
   Json.Decode.succeed (\pid ptipo -> {id = pid, tipo = ptipo})
   |> required "id" (jsonDecULID)
   |> required "tipo" (jsonDecTipoDistribucion)

jsonEncDistribucion : Distribucion -> Value
jsonEncDistribucion  val =
   Json.Encode.object
   [ ("id", jsonEncULID val.id)
   , ("tipo", jsonEncTipoDistribucion val.tipo)
   ]



type TipoDistribucion  =
    TipoDistribucionMontosEspecificos DistribucionMontosEspecificos
    | TipoDistribucionMontoEquitativo DistribucionMontoEquitativo
    | TipoDistribucionRepartija Repartija

jsonDecTipoDistribucion : Json.Decode.Decoder ( TipoDistribucion )
jsonDecTipoDistribucion =
    let jsonDecDictTipoDistribucion = Dict.fromList
            [ ("TipoDistribucionMontosEspecificos", Json.Decode.lazy (\_ -> Json.Decode.map TipoDistribucionMontosEspecificos (jsonDecDistribucionMontosEspecificos)))
            , ("TipoDistribucionMontoEquitativo", Json.Decode.lazy (\_ -> Json.Decode.map TipoDistribucionMontoEquitativo (jsonDecDistribucionMontoEquitativo)))
            , ("TipoDistribucionRepartija", Json.Decode.lazy (\_ -> Json.Decode.map TipoDistribucionRepartija (jsonDecRepartija)))
            ]
    in  decodeSumObjectWithSingleField  "TipoDistribucion" jsonDecDictTipoDistribucion

jsonEncTipoDistribucion : TipoDistribucion -> Value
jsonEncTipoDistribucion  val =
    let keyval v = case v of
                    TipoDistribucionMontosEspecificos v1 -> ("TipoDistribucionMontosEspecificos", encodeValue (jsonEncDistribucionMontosEspecificos v1))
                    TipoDistribucionMontoEquitativo v1 -> ("TipoDistribucionMontoEquitativo", encodeValue (jsonEncDistribucionMontoEquitativo v1))
                    TipoDistribucionRepartija v1 -> ("TipoDistribucionRepartija", encodeValue (jsonEncRepartija v1))
    in encodeSumObjectWithSingleField keyval val



type alias DistribucionMontosEspecificos  =
   { id: ULID
   , montos: (List MontoEspecifico)
   }

jsonDecDistribucionMontosEspecificos : Json.Decode.Decoder ( DistribucionMontosEspecificos )
jsonDecDistribucionMontosEspecificos =
   Json.Decode.succeed (\pid pmontos -> {id = pid, montos = pmontos})
   |> required "id" (jsonDecULID)
   |> required "montos" (Json.Decode.list (jsonDecMontoEspecifico))

jsonEncDistribucionMontosEspecificos : DistribucionMontosEspecificos -> Value
jsonEncDistribucionMontosEspecificos  val =
   Json.Encode.object
   [ ("id", jsonEncULID val.id)
   , ("montos", (Json.Encode.list jsonEncMontoEspecifico) val.montos)
   ]



type alias MontoEspecifico  =
   { id: ULID
   , participante: ParticipanteId
   , monto: Monto
   }

jsonDecMontoEspecifico : Json.Decode.Decoder ( MontoEspecifico )
jsonDecMontoEspecifico =
   Json.Decode.succeed (\pid pparticipante pmonto -> {id = pid, participante = pparticipante, monto = pmonto})
   |> required "id" (jsonDecULID)
   |> required "participante" (jsonDecParticipanteId)
   |> required "monto" (jsonDecMonto)

jsonEncMontoEspecifico : MontoEspecifico -> Value
jsonEncMontoEspecifico  val =
   Json.Encode.object
   [ ("id", jsonEncULID val.id)
   , ("participante", jsonEncParticipanteId val.participante)
   , ("monto", jsonEncMonto val.monto)
   ]



type alias DistribucionMontoEquitativo  =
   { id: ULID
   , participantes: (List ParticipanteId)
   }

jsonDecDistribucionMontoEquitativo : Json.Decode.Decoder ( DistribucionMontoEquitativo )
jsonDecDistribucionMontoEquitativo =
   Json.Decode.succeed (\pid pparticipantes -> {id = pid, participantes = pparticipantes})
   |> required "id" (jsonDecULID)
   |> required "participantes" (Json.Decode.list (jsonDecParticipanteId))

jsonEncDistribucionMontoEquitativo : DistribucionMontoEquitativo -> Value
jsonEncDistribucionMontoEquitativo  val =
   Json.Encode.object
   [ ("id", jsonEncULID val.id)
   , ("participantes", (Json.Encode.list jsonEncParticipanteId) val.participantes)
   ]



type DistribucionDeSobras  =
    SobrasNoDistribuir 
    | SobrasProporcional 

jsonDecDistribucionDeSobras : Json.Decode.Decoder ( DistribucionDeSobras )
jsonDecDistribucionDeSobras = 
    let jsonDecDictDistribucionDeSobras = Dict.fromList [("SobrasNoDistribuir", SobrasNoDistribuir), ("SobrasProporcional", SobrasProporcional)]
    in  decodeSumUnaries "DistribucionDeSobras" jsonDecDictDistribucionDeSobras

jsonEncDistribucionDeSobras : DistribucionDeSobras -> Value
jsonEncDistribucionDeSobras  val =
    case val of
        SobrasNoDistribuir -> Json.Encode.string "SobrasNoDistribuir"
        SobrasProporcional -> Json.Encode.string "SobrasProporcional"



type alias Repartija  =
   { id: ULID
   , nombre: String
   , extra: Monto
   , distribucionDeSobras: DistribucionDeSobras
   , items: (List RepartijaItem)
   , claims: (List RepartijaClaim)
   }

jsonDecRepartija : Json.Decode.Decoder ( Repartija )
jsonDecRepartija =
   Json.Decode.succeed (\pid pnombre pextra pdistribucionDeSobras pitems pclaims -> {id = pid, nombre = pnombre, extra = pextra, distribucionDeSobras = pdistribucionDeSobras, items = pitems, claims = pclaims})
   |> required "id" (jsonDecULID)
   |> required "nombre" (Json.Decode.string)
   |> required "extra" (jsonDecMonto)
   |> required "distribucionDeSobras" (jsonDecDistribucionDeSobras)
   |> required "items" (Json.Decode.list (jsonDecRepartijaItem))
   |> required "claims" (Json.Decode.list (jsonDecRepartijaClaim))

jsonEncRepartija : Repartija -> Value
jsonEncRepartija  val =
   Json.Encode.object
   [ ("id", jsonEncULID val.id)
   , ("nombre", Json.Encode.string val.nombre)
   , ("extra", jsonEncMonto val.extra)
   , ("distribucionDeSobras", jsonEncDistribucionDeSobras val.distribucionDeSobras)
   , ("items", (Json.Encode.list jsonEncRepartijaItem) val.items)
   , ("claims", (Json.Encode.list jsonEncRepartijaClaim) val.claims)
   ]



type alias RepartijaItem  =
   { id: ULID
   , nombre: String
   , monto: Monto
   , cantidad: Int
   }

jsonDecRepartijaItem : Json.Decode.Decoder ( RepartijaItem )
jsonDecRepartijaItem =
   Json.Decode.succeed (\pid pnombre pmonto pcantidad -> {id = pid, nombre = pnombre, monto = pmonto, cantidad = pcantidad})
   |> required "id" (jsonDecULID)
   |> required "nombre" (Json.Decode.string)
   |> required "monto" (jsonDecMonto)
   |> required "cantidad" (Json.Decode.int)

jsonEncRepartijaItem : RepartijaItem -> Value
jsonEncRepartijaItem  val =
   Json.Encode.object
   [ ("id", jsonEncULID val.id)
   , ("nombre", Json.Encode.string val.nombre)
   , ("monto", jsonEncMonto val.monto)
   , ("cantidad", Json.Encode.int val.cantidad)
   ]



type alias RepartijaClaim  =
   { id: ULID
   , participante: ParticipanteId
   , itemId: ULID
   , cantidad: (Maybe Int)
   }

jsonDecRepartijaClaim : Json.Decode.Decoder ( RepartijaClaim )
jsonDecRepartijaClaim =
   Json.Decode.succeed (\pid pparticipante pitemId pcantidad -> {id = pid, participante = pparticipante, itemId = pitemId, cantidad = pcantidad})
   |> required "id" (jsonDecULID)
   |> required "participante" (jsonDecParticipanteId)
   |> required "itemId" (jsonDecULID)
   |> fnullable "cantidad" (Json.Decode.int)

jsonEncRepartijaClaim : RepartijaClaim -> Value
jsonEncRepartijaClaim  val =
   Json.Encode.object
   [ ("id", jsonEncULID val.id)
   , ("participante", jsonEncParticipanteId val.participante)
   , ("itemId", jsonEncULID val.itemId)
   , ("cantidad", (maybeEncode (Json.Encode.int)) val.cantidad)
   ]



type alias ShallowRepartija  =
   { shallowId: ULID
   , shallowNombre: String
   }

jsonDecShallowRepartija : Json.Decode.Decoder ( ShallowRepartija )
jsonDecShallowRepartija =
   Json.Decode.succeed (\pshallowId pshallowNombre -> {shallowId = pshallowId, shallowNombre = pshallowNombre})
   |> required "shallowId" (jsonDecULID)
   |> required "shallowNombre" (Json.Decode.string)

jsonEncShallowRepartija : ShallowRepartija -> Value
jsonEncShallowRepartija  val =
   Json.Encode.object
   [ ("shallowId", jsonEncULID val.shallowId)
   , ("shallowNombre", Json.Encode.string val.shallowNombre)
   ]



type alias RepartijaForFrontend  =
   { repartija: Repartija
   , pagoId: ULID
   , pagoNombre: String
   }

jsonDecRepartijaForFrontend : Json.Decode.Decoder ( RepartijaForFrontend )
jsonDecRepartijaForFrontend =
   Json.Decode.succeed (\prepartija ppagoId ppagoNombre -> {repartija = prepartija, pagoId = ppagoId, pagoNombre = ppagoNombre})
   |> required "repartija" (jsonDecRepartija)
   |> required "pagoId" (jsonDecULID)
   |> required "pagoNombre" (Json.Decode.string)

jsonEncRepartijaForFrontend : RepartijaForFrontend -> Value
jsonEncRepartijaForFrontend  val =
   Json.Encode.object
   [ ("repartija", jsonEncRepartija val.repartija)
   , ("pagoId", jsonEncULID val.pagoId)
   , ("pagoNombre", Json.Encode.string val.pagoNombre)
   ]


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

getGrupoById : ULID -> (Result Http.Error  (ShallowGrupo)  -> msg) -> Cmd msg
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
                Http.expectJson toMsg jsonDecShallowGrupo
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getGrupoByIdResumen : ULID -> (Result Http.Error  (ResumenGrupo)  -> msg) -> Cmd msg
getGrupoByIdResumen capture_id toMsg =
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
                    , "resumen"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecResumenGrupo
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

getGrupoByIdPagos : ULID -> (Result Http.Error  ((List ShallowPago))  -> msg) -> Cmd msg
getGrupoByIdPagos capture_id toMsg =
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
                    , "pagos"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list (jsonDecShallowPago))
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getGrupoByIdPagosByPagoId : ULID -> ULID -> (Result Http.Error  (Pago)  -> msg) -> Cmd msg
getGrupoByIdPagosByPagoId capture_id capture_pagoId toMsg =
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
                    , "pagos"
                    , (capture_pagoId)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecPago
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postPagosResumen : Pago -> (Result Http.Error  (ResumenPago)  -> msg) -> Cmd msg
postPagosResumen body toMsg =
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
                    , "resumen"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncPago body)
            , expect =
                Http.expectJson toMsg jsonDecResumenPago
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

getRepartijasByRepartijaId : ULID -> (Result Http.Error  (RepartijaForFrontend)  -> msg) -> Cmd msg
getRepartijasByRepartijaId capture_repartijaId toMsg =
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
                    [ "repartijas"
                    , (capture_repartijaId)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecRepartijaForFrontend
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putRepartijasByRepartijaId : ULID -> RepartijaClaim -> (Result Http.Error  (RepartijaClaim)  -> msg) -> Cmd msg
putRepartijasByRepartijaId capture_repartijaId body toMsg =
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
                    [ "repartijas"
                    , (capture_repartijaId)
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncRepartijaClaim body)
            , expect =
                Http.expectJson toMsg jsonDecRepartijaClaim
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

deleteRepartijasClaimsByClaimId : ULID -> (Result Http.Error  (String)  -> msg) -> Cmd msg
deleteRepartijasClaimsByClaimId capture_claimId toMsg =
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
                    [ "repartijas"
                    , "claims"
                    , (capture_claimId)
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg Json.Decode.string
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postGrupoByIdFreeze : ULID -> (Result Http.Error  (ShallowGrupo)  -> msg) -> Cmd msg
postGrupoByIdFreeze capture_id toMsg =
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
                    , "freeze"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecShallowGrupo
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

deleteGrupoByIdFreeze : ULID -> (Result Http.Error  (ShallowGrupo)  -> msg) -> Cmd msg
deleteGrupoByIdFreeze capture_id toMsg =
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
                    , "freeze"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecShallowGrupo
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postGrupoByIdTransaccionescongeladasByTransaccionIdSaldar : ULID -> ULID -> Pago -> (Result Http.Error  (Pago)  -> msg) -> Cmd msg
postGrupoByIdTransaccionescongeladasByTransaccionIdSaldar capture_id capture_transaccionId body toMsg =
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
                    , "transacciones-congeladas"
                    , (capture_transaccionId)
                    , "saldar"
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

postReceiptParseimage : ReceiptImageRequest -> (Result Http.Error  (ReceiptImageResponse)  -> msg) -> Cmd msg
postReceiptParseimage body toMsg =
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
                    [ "receipt"
                    , "parse-image"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncReceiptImageRequest body)
            , expect =
                Http.expectJson toMsg jsonDecReceiptImageResponse
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

getHealth : (Result Http.Error  (String)  -> msg) -> Cmd msg
getHealth toMsg =
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
                    [ "health"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg Json.Decode.string
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
