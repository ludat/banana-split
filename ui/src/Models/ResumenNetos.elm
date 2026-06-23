module Models.ResumenNetos exposing (errorAccionableEn, errorMensaje, getDeudasFromResumen)

import Generated.Api exposing (Monto, Netos, ResumenNetos, TipoErrorResumen(..))
import Models.LugarAccionable exposing (LugarParaAccionar(..))
import Models.Monto as Monto


errorMensaje : TipoErrorResumen -> String
errorMensaje tipo =
    case tipo of
        ErrorRepartijaSinItems ->
            "No hay items para repartir."

        ErrorRepartijaTotalItemsNoCoincide totalItems totalPago ->
            "El total de items (" ++ Monto.toString totalItems ++ ") debería ser igual al monto del pago (" ++ Monto.toString totalPago ++ "), " ++ Monto.diffText totalItems totalPago

        ErrorRepartijaSinClaims ->
            "Nadie reclamo ningun item."

        ErrorRepartijaTotalReclamadoNoCoincide totalReclamado totalPago ->
            "El total reclamado (" ++ Monto.toString totalReclamado ++ ") no coincide con el monto del pago (" ++ Monto.toString totalPago ++ "), " ++ Monto.diffText totalReclamado totalPago

        ErrorPartesVacias ->
            "No hay participantes en el reparto"

        ErrorPartesMontoFijoSuperaTotal totalFijos totalPago ->
            "Los montos fijos (" ++ Monto.toString totalFijos ++ ") superan el monto del pago (" ++ Monto.toString totalPago ++ ")"

        ErrorPartesTotalNoCoincide totalFijos totalPago ->
            "El total de los montos fijos (" ++ Monto.toString totalFijos ++ ") debería ser igual al monto del pago (" ++ Monto.toString totalPago ++ "), " ++ Monto.diffText totalFijos totalPago


errorAccionableEn : TipoErrorResumen -> List LugarParaAccionar
errorAccionableEn tipo =
    case tipo of
        ErrorRepartijaSinItems ->
            [ Lugar_CreacionPago ]

        ErrorRepartijaTotalItemsNoCoincide _ _ ->
            [ Lugar_CreacionPago ]

        ErrorRepartijaSinClaims ->
            [ Lugar_PaginaRepartija ]

        ErrorRepartijaTotalReclamadoNoCoincide _ _ ->
            [ Lugar_PaginaRepartija ]

        ErrorPartesVacias ->
            [ Lugar_CreacionPago ]

        ErrorPartesMontoFijoSuperaTotal _ _ ->
            [ Lugar_CreacionPago ]

        ErrorPartesTotalNoCoincide _ _ ->
            [ Lugar_CreacionPago ]


getDeudasFromResumen : ResumenNetos -> Maybe (Netos Monto)
getDeudasFromResumen resumen =
    Just resumen.netos
