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

import Utils.Posix exposing (Posix, jsonDecPosix, jsonEncPosix)
import Date
import Utils.Day exposing (Day, jsonDecDay, jsonEncDay)
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



type alias CreateGrupoAsUserParams  =
   { grupoName: String
   }

jsonDecCreateGrupoAsUserParams : Json.Decode.Decoder ( CreateGrupoAsUserParams )
jsonDecCreateGrupoAsUserParams =
   Json.Decode.succeed (\pgrupoName -> {grupoName = pgrupoName}) |> custom (Json.Decode.string)

jsonEncCreateGrupoAsUserParams : CreateGrupoAsUserParams -> Value
jsonEncCreateGrupoAsUserParams  val =
   Json.Encode.string val.grupoName


type alias SigninParams  =
   { email: String
   }

jsonDecSigninParams : Json.Decode.Decoder ( SigninParams )
jsonDecSigninParams =
   Json.Decode.succeed (\pemail -> {email = pemail}) |> custom (Json.Decode.string)

jsonEncSigninParams : SigninParams -> Value
jsonEncSigninParams  val =
   Json.Encode.string val.email


type alias SignupParams  =
   { nombre: String
   , email: String
   }

jsonDecSignupParams : Json.Decode.Decoder ( SignupParams )
jsonDecSignupParams =
   Json.Decode.succeed (\pnombre pemail -> {nombre = pnombre, email = pemail})
   |> required "nombre" (Json.Decode.string)
   |> required "email" (Json.Decode.string)

jsonEncSignupParams : SignupParams -> Value
jsonEncSignupParams  val =
   Json.Encode.object
   [ ("nombre", Json.Encode.string val.nombre)
   , ("email", Json.Encode.string val.email)
   ]



type alias LoginChallenge  =
   { challenge: String
   }

jsonDecLoginChallenge : Json.Decode.Decoder ( LoginChallenge )
jsonDecLoginChallenge =
   Json.Decode.succeed (\pchallenge -> {challenge = pchallenge}) |> custom (Json.Decode.string)

jsonEncLoginChallenge : LoginChallenge -> Value
jsonEncLoginChallenge  val =
   Json.Encode.string val.challenge


type alias VerifyParams  =
   { challenge: String
   , code: String
   }

jsonDecVerifyParams : Json.Decode.Decoder ( VerifyParams )
jsonDecVerifyParams =
   Json.Decode.succeed (\pchallenge pcode -> {challenge = pchallenge, code = pcode})
   |> required "challenge" (Json.Decode.string)
   |> required "code" (Json.Decode.string)

jsonEncVerifyParams : VerifyParams -> Value
jsonEncVerifyParams  val =
   Json.Encode.object
   [ ("challenge", Json.Encode.string val.challenge)
   , ("code", Json.Encode.string val.code)
   ]



type alias UpdateMeParams  =
   { nombre: String
   }

jsonDecUpdateMeParams : Json.Decode.Decoder ( UpdateMeParams )
jsonDecUpdateMeParams =
   Json.Decode.succeed (\pnombre -> {nombre = pnombre}) |> custom (Json.Decode.string)

jsonEncUpdateMeParams : UpdateMeParams -> Value
jsonEncUpdateMeParams  val =
   Json.Encode.string val.nombre


type alias User  =
   { id: ULID
   , email: String
   , nombre: String
   }

jsonDecUser : Json.Decode.Decoder ( User )
jsonDecUser =
   Json.Decode.succeed (\pid pemail pnombre -> {id = pid, email = pemail, nombre = pnombre})
   |> required "id" (jsonDecULID)
   |> required "email" (Json.Decode.string)
   |> required "nombre" (Json.Decode.string)

jsonEncUser : User -> Value
jsonEncUser  val =
   Json.Encode.object
   [ ("id", jsonEncULID val.id)
   , ("email", Json.Encode.string val.email)
   , ("nombre", Json.Encode.string val.nombre)
   ]



type alias UpdateGrupoParams  =
   { nombre: String
   , monedaPorDefecto: Moneda
   }

jsonDecUpdateGrupoParams : Json.Decode.Decoder ( UpdateGrupoParams )
jsonDecUpdateGrupoParams =
   Json.Decode.succeed (\pnombre pmonedaPorDefecto -> {nombre = pnombre, monedaPorDefecto = pmonedaPorDefecto})
   |> required "nombre" (Json.Decode.string)
   |> required "monedaPorDefecto" (jsonDecMoneda)

jsonEncUpdateGrupoParams : UpdateGrupoParams -> Value
jsonEncUpdateGrupoParams  val =
   Json.Encode.object
   [ ("nombre", Json.Encode.string val.nombre)
   , ("monedaPorDefecto", jsonEncMoneda val.monedaPorDefecto)
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


type ClaimRejection  =
    ClaimedByOtherUser 
    | AlreadyOwnAnotherParticipante 
    | ParticipanteNotFound 

jsonDecClaimRejection : Json.Decode.Decoder ( ClaimRejection )
jsonDecClaimRejection = 
    let jsonDecDictClaimRejection = Dict.fromList [("ClaimedByOtherUser", ClaimedByOtherUser), ("AlreadyOwnAnotherParticipante", AlreadyOwnAnotherParticipante), ("ParticipanteNotFound", ParticipanteNotFound)]
    in  decodeSumUnaries "ClaimRejection" jsonDecDictClaimRejection

jsonEncClaimRejection : ClaimRejection -> Value
jsonEncClaimRejection  val =
    case val of
        ClaimedByOtherUser -> Json.Encode.string "ClaimedByOtherUser"
        AlreadyOwnAnotherParticipante -> Json.Encode.string "AlreadyOwnAnotherParticipante"
        ParticipanteNotFound -> Json.Encode.string "ParticipanteNotFound"



type ClaimParticipanteResult  =
    ClaimAccepted Participante
    | ClaimRejected ClaimRejection

jsonDecClaimParticipanteResult : Json.Decode.Decoder ( ClaimParticipanteResult )
jsonDecClaimParticipanteResult =
    let jsonDecDictClaimParticipanteResult = Dict.fromList
            [ ("ClaimAccepted", Json.Decode.lazy (\_ -> Json.Decode.map ClaimAccepted (jsonDecParticipante)))
            , ("ClaimRejected", Json.Decode.lazy (\_ -> Json.Decode.map ClaimRejected (jsonDecClaimRejection)))
            ]
    in  decodeSumObjectWithSingleField  "ClaimParticipanteResult" jsonDecDictClaimParticipanteResult

jsonEncClaimParticipanteResult : ClaimParticipanteResult -> Value
jsonEncClaimParticipanteResult  val =
    let keyval v = case v of
                    ClaimAccepted v1 -> ("ClaimAccepted", encodeValue (jsonEncParticipante v1))
                    ClaimRejected v1 -> ("ClaimRejected", encodeValue (jsonEncClaimRejection v1))
    in encodeSumObjectWithSingleField keyval val



type alias ResumenGrupo  =
   { transaccionesParaSaldar: (PorMoneda (List Transaccion))
   , netos: (PorMoneda (Netos Monto))
   , cantidadPagosInvalidos: Int
   , cantidadPagos: Int
   , isFrozen: Bool
   }

jsonDecResumenGrupo : Json.Decode.Decoder ( ResumenGrupo )
jsonDecResumenGrupo =
   Json.Decode.succeed (\ptransaccionesParaSaldar pnetos pcantidadPagosInvalidos pcantidadPagos pisFrozen -> {transaccionesParaSaldar = ptransaccionesParaSaldar, netos = pnetos, cantidadPagosInvalidos = pcantidadPagosInvalidos, cantidadPagos = pcantidadPagos, isFrozen = pisFrozen})
   |> required "transaccionesParaSaldar" (jsonDecPorMoneda (Json.Decode.list (jsonDecTransaccion)))
   |> required "netos" (jsonDecPorMoneda (jsonDecNetos (jsonDecMonto)))
   |> required "cantidadPagosInvalidos" (Json.Decode.int)
   |> required "cantidadPagos" (Json.Decode.int)
   |> required "isFrozen" (Json.Decode.bool)

jsonEncResumenGrupo : ResumenGrupo -> Value
jsonEncResumenGrupo  val =
   Json.Encode.object
   [ ("transaccionesParaSaldar", (jsonEncPorMoneda ((Json.Encode.list jsonEncTransaccion))) val.transaccionesParaSaldar)
   , ("netos", (jsonEncPorMoneda ((jsonEncNetos (jsonEncMonto)))) val.netos)
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



type alias PorMoneda a = (List (Moneda, a))

jsonDecPorMoneda : Json.Decode.Decoder a -> Json.Decode.Decoder ( PorMoneda a )
jsonDecPorMoneda localDecoder_a =
    Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 (jsonDecMoneda)) (Json.Decode.index 1 (localDecoder_a)))

jsonEncPorMoneda : (a -> Value) -> PorMoneda a -> Value
jsonEncPorMoneda localEncoder_a val = (Json.Encode.list (\(t1,t2) -> Json.Encode.list identity [(jsonEncMoneda) t1,(localEncoder_a) t2])) val



type Moneda  =
    ARS 
    | USD 
    | EUR 
    | BRL 
    | UYU 
    | CLP 
    | GBP 

jsonDecMoneda : Json.Decode.Decoder ( Moneda )
jsonDecMoneda = 
    let jsonDecDictMoneda = Dict.fromList [("ARS", ARS), ("USD", USD), ("EUR", EUR), ("BRL", BRL), ("UYU", UYU), ("CLP", CLP), ("GBP", GBP)]
    in  decodeSumUnaries "Moneda" jsonDecDictMoneda

jsonEncMoneda : Moneda -> Value
jsonEncMoneda  val =
    case val of
        ARS -> Json.Encode.string "ARS"
        USD -> Json.Encode.string "USD"
        EUR -> Json.Encode.string "EUR"
        BRL -> Json.Encode.string "BRL"
        UYU -> Json.Encode.string "UYU"
        CLP -> Json.Encode.string "CLP"
        GBP -> Json.Encode.string "GBP"



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
    ErrorRepartijaSinItems 
    | ErrorRepartijaTotalItemsNoCoincide Monto Monto
    | ErrorRepartijaSinClaims 
    | ErrorRepartijaTotalReclamadoNoCoincide Monto Monto
    | ErrorPartesVacias 
    | ErrorPartesMontoFijoSuperaTotal Monto Monto
    | ErrorPartesTotalNoCoincide Monto Monto

jsonDecTipoErrorResumen : Json.Decode.Decoder ( TipoErrorResumen )
jsonDecTipoErrorResumen =
    let jsonDecDictTipoErrorResumen = Dict.fromList
            [ ("ErrorRepartijaSinItems", Json.Decode.lazy (\_ -> Json.Decode.succeed ErrorRepartijaSinItems))
            , ("ErrorRepartijaTotalItemsNoCoincide", Json.Decode.lazy (\_ -> Json.Decode.map2 ErrorRepartijaTotalItemsNoCoincide (Json.Decode.index 0 (jsonDecMonto)) (Json.Decode.index 1 (jsonDecMonto))))
            , ("ErrorRepartijaSinClaims", Json.Decode.lazy (\_ -> Json.Decode.succeed ErrorRepartijaSinClaims))
            , ("ErrorRepartijaTotalReclamadoNoCoincide", Json.Decode.lazy (\_ -> Json.Decode.map2 ErrorRepartijaTotalReclamadoNoCoincide (Json.Decode.index 0 (jsonDecMonto)) (Json.Decode.index 1 (jsonDecMonto))))
            , ("ErrorPartesVacias", Json.Decode.lazy (\_ -> Json.Decode.succeed ErrorPartesVacias))
            , ("ErrorPartesMontoFijoSuperaTotal", Json.Decode.lazy (\_ -> Json.Decode.map2 ErrorPartesMontoFijoSuperaTotal (Json.Decode.index 0 (jsonDecMonto)) (Json.Decode.index 1 (jsonDecMonto))))
            , ("ErrorPartesTotalNoCoincide", Json.Decode.lazy (\_ -> Json.Decode.map2 ErrorPartesTotalNoCoincide (Json.Decode.index 0 (jsonDecMonto)) (Json.Decode.index 1 (jsonDecMonto))))
            ]
    in  decodeSumObjectWithSingleField  "TipoErrorResumen" jsonDecDictTipoErrorResumen

jsonEncTipoErrorResumen : TipoErrorResumen -> Value
jsonEncTipoErrorResumen  val =
    let keyval v = case v of
                    ErrorRepartijaSinItems  -> ("ErrorRepartijaSinItems", encodeValue (Json.Encode.list identity []))
                    ErrorRepartijaTotalItemsNoCoincide v1 v2 -> ("ErrorRepartijaTotalItemsNoCoincide", encodeValue (Json.Encode.list identity [jsonEncMonto v1, jsonEncMonto v2]))
                    ErrorRepartijaSinClaims  -> ("ErrorRepartijaSinClaims", encodeValue (Json.Encode.list identity []))
                    ErrorRepartijaTotalReclamadoNoCoincide v1 v2 -> ("ErrorRepartijaTotalReclamadoNoCoincide", encodeValue (Json.Encode.list identity [jsonEncMonto v1, jsonEncMonto v2]))
                    ErrorPartesVacias  -> ("ErrorPartesVacias", encodeValue (Json.Encode.list identity []))
                    ErrorPartesMontoFijoSuperaTotal v1 v2 -> ("ErrorPartesMontoFijoSuperaTotal", encodeValue (Json.Encode.list identity [jsonEncMonto v1, jsonEncMonto v2]))
                    ErrorPartesTotalNoCoincide v1 v2 -> ("ErrorPartesTotalNoCoincide", encodeValue (Json.Encode.list identity [jsonEncMonto v1, jsonEncMonto v2]))
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
   , monedaPorDefecto: Moneda
   }

jsonDecGrupo : Json.Decode.Decoder ( Grupo )
jsonDecGrupo =
   Json.Decode.succeed (\pid pnombre ppagos pparticipantes pmonedaPorDefecto -> {id = pid, nombre = pnombre, pagos = ppagos, participantes = pparticipantes, monedaPorDefecto = pmonedaPorDefecto})
   |> required "id" (jsonDecULID)
   |> required "nombre" (Json.Decode.string)
   |> required "pagos" (Json.Decode.list (jsonDecPago))
   |> required "participantes" (Json.Decode.list (jsonDecParticipante))
   |> required "monedaPorDefecto" (jsonDecMoneda)

jsonEncGrupo : Grupo -> Value
jsonEncGrupo  val =
   Json.Encode.object
   [ ("id", jsonEncULID val.id)
   , ("nombre", Json.Encode.string val.nombre)
   , ("pagos", (Json.Encode.list jsonEncPago) val.pagos)
   , ("participantes", (Json.Encode.list jsonEncParticipante) val.participantes)
   , ("monedaPorDefecto", jsonEncMoneda val.monedaPorDefecto)
   ]



type alias ShallowGrupo  =
   { id: ULID
   , nombre: String
   , participantes: (List Participante)
   , isFrozen: Bool
   , monedaPorDefecto: Moneda
   }

jsonDecShallowGrupo : Json.Decode.Decoder ( ShallowGrupo )
jsonDecShallowGrupo =
   Json.Decode.succeed (\pid pnombre pparticipantes pisFrozen pmonedaPorDefecto -> {id = pid, nombre = pnombre, participantes = pparticipantes, isFrozen = pisFrozen, monedaPorDefecto = pmonedaPorDefecto})
   |> required "id" (jsonDecULID)
   |> required "nombre" (Json.Decode.string)
   |> required "participantes" (Json.Decode.list (jsonDecParticipante))
   |> required "isFrozen" (Json.Decode.bool)
   |> required "monedaPorDefecto" (jsonDecMoneda)

jsonEncShallowGrupo : ShallowGrupo -> Value
jsonEncShallowGrupo  val =
   Json.Encode.object
   [ ("id", jsonEncULID val.id)
   , ("nombre", Json.Encode.string val.nombre)
   , ("participantes", (Json.Encode.list jsonEncParticipante) val.participantes)
   , ("isFrozen", Json.Encode.bool val.isFrozen)
   , ("monedaPorDefecto", jsonEncMoneda val.monedaPorDefecto)
   ]



type alias Participante  =
   { id: ULID
   , nombre: String
   , user: (Maybe User)
   }

jsonDecParticipante : Json.Decode.Decoder ( Participante )
jsonDecParticipante =
   Json.Decode.succeed (\pid pnombre puser -> {id = pid, nombre = pnombre, user = puser})
   |> required "id" (jsonDecULID)
   |> required "nombre" (Json.Decode.string)
   |> fnullable "user" (jsonDecUser)

jsonEncParticipante : Participante -> Value
jsonEncParticipante  val =
   Json.Encode.object
   [ ("id", jsonEncULID val.id)
   , ("nombre", Json.Encode.string val.nombre)
   , ("user", (maybeEncode (jsonEncUser)) val.user)
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
   , moneda: Moneda
   , isValid: Bool
   , nombre: String
   , fecha: Day
   , pagadores: Distribucion
   , deudores: Distribucion
   }

jsonDecPago : Json.Decode.Decoder ( Pago )
jsonDecPago =
   Json.Decode.succeed (\ppagoId pmonto pmoneda pisValid pnombre pfecha ppagadores pdeudores -> {pagoId = ppagoId, monto = pmonto, moneda = pmoneda, isValid = pisValid, nombre = pnombre, fecha = pfecha, pagadores = ppagadores, deudores = pdeudores})
   |> required "pagoId" (jsonDecULID)
   |> required "monto" (jsonDecMonto)
   |> required "moneda" (jsonDecMoneda)
   |> required "isValid" (Json.Decode.bool)
   |> required "nombre" (Json.Decode.string)
   |> required "fecha" (jsonDecDay)
   |> required "pagadores" (jsonDecDistribucion)
   |> required "deudores" (jsonDecDistribucion)

jsonEncPago : Pago -> Value
jsonEncPago  val =
   Json.Encode.object
   [ ("pagoId", jsonEncULID val.pagoId)
   , ("monto", jsonEncMonto val.monto)
   , ("moneda", jsonEncMoneda val.moneda)
   , ("isValid", Json.Encode.bool val.isValid)
   , ("nombre", Json.Encode.string val.nombre)
   , ("fecha", jsonEncDay val.fecha)
   , ("pagadores", jsonEncDistribucion val.pagadores)
   , ("deudores", jsonEncDistribucion val.deudores)
   ]



type alias ShallowPago  =
   { pagoId: ULID
   , isValid: Bool
   , nombre: String
   , monto: Monto
   , moneda: Moneda
   , fecha: Day
   }

jsonDecShallowPago : Json.Decode.Decoder ( ShallowPago )
jsonDecShallowPago =
   Json.Decode.succeed (\ppagoId pisValid pnombre pmonto pmoneda pfecha -> {pagoId = ppagoId, isValid = pisValid, nombre = pnombre, monto = pmonto, moneda = pmoneda, fecha = pfecha})
   |> required "pagoId" (jsonDecULID)
   |> required "isValid" (Json.Decode.bool)
   |> required "nombre" (Json.Decode.string)
   |> required "monto" (jsonDecMonto)
   |> required "moneda" (jsonDecMoneda)
   |> required "fecha" (jsonDecDay)

jsonEncShallowPago : ShallowPago -> Value
jsonEncShallowPago  val =
   Json.Encode.object
   [ ("pagoId", jsonEncULID val.pagoId)
   , ("isValid", Json.Encode.bool val.isValid)
   , ("nombre", Json.Encode.string val.nombre)
   , ("monto", jsonEncMonto val.monto)
   , ("moneda", jsonEncMoneda val.moneda)
   , ("fecha", jsonEncDay val.fecha)
   ]



type Parte  =
    MontoFijo Monto ParticipanteId
    | Ponderado Int ParticipanteId
    | PonderadoYMontoFijo Monto Int ParticipanteId

jsonDecParte : Json.Decode.Decoder ( Parte )
jsonDecParte =
    let jsonDecDictParte = Dict.fromList
            [ ("MontoFijo", Json.Decode.lazy (\_ -> Json.Decode.map2 MontoFijo (Json.Decode.index 0 (jsonDecMonto)) (Json.Decode.index 1 (jsonDecParticipanteId))))
            , ("Ponderado", Json.Decode.lazy (\_ -> Json.Decode.map2 Ponderado (Json.Decode.index 0 (Json.Decode.int)) (Json.Decode.index 1 (jsonDecParticipanteId))))
            , ("PonderadoYMontoFijo", Json.Decode.lazy (\_ -> Json.Decode.map3 PonderadoYMontoFijo (Json.Decode.index 0 (jsonDecMonto)) (Json.Decode.index 1 (Json.Decode.int)) (Json.Decode.index 2 (jsonDecParticipanteId))))
            ]
    in  decodeSumObjectWithSingleField  "Parte" jsonDecDictParte

jsonEncParte : Parte -> Value
jsonEncParte  val =
    let keyval v = case v of
                    MontoFijo v1 v2 -> ("MontoFijo", encodeValue (Json.Encode.list identity [jsonEncMonto v1, jsonEncParticipanteId v2]))
                    Ponderado v1 v2 -> ("Ponderado", encodeValue (Json.Encode.list identity [Json.Encode.int v1, jsonEncParticipanteId v2]))
                    PonderadoYMontoFijo v1 v2 v3 -> ("PonderadoYMontoFijo", encodeValue (Json.Encode.list identity [jsonEncMonto v1, Json.Encode.int v2, jsonEncParticipanteId v3]))
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
    TipoDistribucionRepartija Repartija
    | TipoDistribucionPartes DistribucionPartes

jsonDecTipoDistribucion : Json.Decode.Decoder ( TipoDistribucion )
jsonDecTipoDistribucion =
    let jsonDecDictTipoDistribucion = Dict.fromList
            [ ("TipoDistribucionRepartija", Json.Decode.lazy (\_ -> Json.Decode.map TipoDistribucionRepartija (jsonDecRepartija)))
            , ("TipoDistribucionPartes", Json.Decode.lazy (\_ -> Json.Decode.map TipoDistribucionPartes (jsonDecDistribucionPartes)))
            ]
    in  decodeSumObjectWithSingleField  "TipoDistribucion" jsonDecDictTipoDistribucion

jsonEncTipoDistribucion : TipoDistribucion -> Value
jsonEncTipoDistribucion  val =
    let keyval v = case v of
                    TipoDistribucionRepartija v1 -> ("TipoDistribucionRepartija", encodeValue (jsonEncRepartija v1))
                    TipoDistribucionPartes v1 -> ("TipoDistribucionPartes", encodeValue (jsonEncDistribucionPartes v1))
    in encodeSumObjectWithSingleField keyval val



type alias DistribucionPartes  =
   { id: ULID
   , partes: (List Parte)
   }

jsonDecDistribucionPartes : Json.Decode.Decoder ( DistribucionPartes )
jsonDecDistribucionPartes =
   Json.Decode.succeed (\pid ppartes -> {id = pid, partes = ppartes})
   |> required "id" (jsonDecULID)
   |> required "partes" (Json.Decode.list (jsonDecParte))

jsonEncDistribucionPartes : DistribucionPartes -> Value
jsonEncDistribucionPartes  val =
   Json.Encode.object
   [ ("id", jsonEncULID val.id)
   , ("partes", (Json.Encode.list jsonEncParte) val.partes)
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

putGrupoById : ULID -> UpdateGrupoParams -> (Result Http.Error  (ShallowGrupo)  -> msg) -> Cmd msg
putGrupoById capture_id body toMsg =
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
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncUpdateGrupoParams body)
            , expect =
                Http.expectJson toMsg jsonDecShallowGrupo
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

postAuthSignup : SignupParams -> (Result Http.Error  (LoginChallenge)  -> msg) -> Cmd msg
postAuthSignup body toMsg =
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
                    [ "auth"
                    , "signup"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncSignupParams body)
            , expect =
                Http.expectJson toMsg jsonDecLoginChallenge
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postAuthSignin : SigninParams -> (Result Http.Error  (LoginChallenge)  -> msg) -> Cmd msg
postAuthSignin body toMsg =
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
                    [ "auth"
                    , "signin"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncSigninParams body)
            , expect =
                Http.expectJson toMsg jsonDecLoginChallenge
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postAuthVerify : VerifyParams -> (Result Http.Error  (User)  -> msg) -> Cmd msg
postAuthVerify body toMsg =
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
                    [ "auth"
                    , "verify"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncVerifyParams body)
            , expect =
                Http.expectJson toMsg jsonDecUser
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postAuthLogout : (Result Http.Error  (String)  -> msg) -> Cmd msg
postAuthLogout toMsg =
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
                    [ "auth"
                    , "logout"
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

getMe : (Result Http.Error  (User)  -> msg) -> Cmd msg
getMe toMsg =
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
                    [ "me"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecUser
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putMe : UpdateMeParams -> (Result Http.Error  (User)  -> msg) -> Cmd msg
putMe body toMsg =
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
                    [ "me"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncUpdateMeParams body)
            , expect =
                Http.expectJson toMsg jsonDecUser
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postMeGrupos : CreateGrupoAsUserParams -> (Result Http.Error  (Grupo)  -> msg) -> Cmd msg
postMeGrupos body toMsg =
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
                    [ "me"
                    , "grupos"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncCreateGrupoAsUserParams body)
            , expect =
                Http.expectJson toMsg jsonDecGrupo
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

putGrupoByIdParticipantesByParticipanteIdClaim : ULID -> ULID -> (Result Http.Error  (ClaimParticipanteResult)  -> msg) -> Cmd msg
putGrupoByIdParticipantesByParticipanteIdClaim capture_id capture_participanteId toMsg =
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
                    , "participantes"
                    , (capture_participanteId)
                    , "claim"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecClaimParticipanteResult
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

deleteGrupoByIdParticipantesByParticipanteIdClaim : ULID -> ULID -> (Result Http.Error  (Participante)  -> msg) -> Cmd msg
deleteGrupoByIdParticipantesByParticipanteIdClaim capture_id capture_participanteId toMsg =
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
                    , "claim"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg jsonDecParticipante
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
