module Models.ResumenNetos exposing (errorAccionableEn, errorMensaje, getDeudasFromResumen, getTotalFromResumen)

import Generated.Api exposing (Monto, Netos, ResumenNetos, TipoErrorResumen(..))
import Models.LugarAccionable exposing (LugarParaAccionar(..))
import Models.Monto as Monto


getTotalFromResumen : ResumenNetos -> Monto
getTotalFromResumen resumen =
    resumen.total


errorMensaje : TipoErrorResumen -> String
errorMensaje tipo =
    case tipo of
        ErrorMontosEspecificosVacios ->
            "No hay montos especificados"

        ErrorMontosEspecificosTotalNoCoincide actual esperado ->
            "El total (" ++ Monto.toString actual ++ ") debería ser igual al monto del pago (" ++ Monto.toString esperado ++ "), " ++ Monto.diffText actual esperado

        ErrorEquitativoSinParticipantes ->
            "No hay participantes especificados"

        ErrorRepartijaSinItems ->
            "No hay items para repartir."

        ErrorRepartijaTotalItemsNoCoincide totalItems totalPago ->
            "El total de items (" ++ Monto.toString totalItems ++ ") debería ser igual al monto del pago (" ++ Monto.toString totalPago ++ "), " ++ Monto.diffText totalItems totalPago

        ErrorRepartijaSinClaims ->
            "Nadie reclamo ningun item."

        ErrorRepartijaTotalReclamadoNoCoincide totalReclamado totalPago ->
            "El total reclamado (" ++ Monto.toString totalReclamado ++ ") no coincide con el monto del pago (" ++ Monto.toString totalPago ++ "), " ++ Monto.diffText totalReclamado totalPago


errorAccionableEn : TipoErrorResumen -> List LugarParaAccionar
errorAccionableEn tipo =
    case tipo of
        ErrorMontosEspecificosVacios ->
            [ Lugar_CreacionPago ]

        ErrorMontosEspecificosTotalNoCoincide _ _ ->
            [ Lugar_CreacionPago ]

        ErrorEquitativoSinParticipantes ->
            [ Lugar_CreacionPago ]

        ErrorRepartijaSinItems ->
            [ Lugar_CreacionPago ]

        ErrorRepartijaTotalItemsNoCoincide _ _ ->
            [ Lugar_CreacionPago ]

        ErrorRepartijaSinClaims ->
            [ Lugar_PaginaRepartija ]

        ErrorRepartijaTotalReclamadoNoCoincide _ _ ->
            [ Lugar_PaginaRepartija ]


getDeudasFromResumen : ResumenNetos -> Maybe (Netos Monto)
getDeudasFromResumen resumen =
    Just resumen.netos
