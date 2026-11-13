module Models.ResumenNetos exposing (EstadoDelGasto(..), errorAccionableEn, errorMensaje, estadoDelGasto, getDeudasFromResumen, seArreglaReclamando)

import Generated.Api exposing (ErrorResumen, Monto, Netos, ResumenNetos, TipoErrorResumen(..))
import Models.LugarAccionable exposing (LugarParaAccionar(..))
import Models.Monto as Monto


errorMensaje : TipoErrorResumen -> String
errorMensaje tipo =
    case tipo of
        ErrorRepartijaSinItems ->
            "No hay items para repartir."

        ErrorRepartijaTotalItemsNoCoincide totalItems totalPago ->
            "Los items cargados suman "
                ++ Monto.toString totalItems
                ++ " pero el gasto es de "
                ++ Monto.toString totalPago
                ++ ": "
                ++ (if (Monto.sub totalItems totalPago).valor > 0 then
                        "sobran " ++ Monto.toString (Monto.sub totalItems totalPago)

                    else
                        "faltan cargar " ++ Monto.toString (Monto.sub totalPago totalItems)
                   )
                ++ ". Corregí los items o el monto del gasto."

        ErrorRepartijaSinClaims ->
            "Nadie reclamó ningún item todavía."

        ErrorRepartijaItemsSinReclamar nombres montoSinReclamar ->
            "Falta que alguien reclame "
                ++ listarItems nombres
                ++ ": "
                ++ Monto.toString montoSinReclamar
                ++ " que todavía no se le cobran a nadie."

        ErrorRepartijaItemsReclamadosDeFormaMixta nombres ->
            (if List.length nombres == 1 then
                listarItems nombres ++ " tiene"

             else
                listarItems nombres ++ " tienen"
            )
                ++ " reclamos por cantidad y en partes iguales a la vez, así que no se le cobran a nadie. Reclamalos de una sola forma."

        ErrorRepartijaTotalReclamadoNoCoincide totalReclamado totalPago ->
            "Lo reclamado (" ++ Monto.toString totalReclamado ++ ") no coincide con el monto del gasto (" ++ Monto.toString totalPago ++ "), " ++ Monto.diffText totalReclamado totalPago

        ErrorPartesVacias ->
            "No hay participantes en el reparto"

        ErrorPartesMontoFijoSuperaTotal totalFijos totalPago ->
            "Los montos fijos (" ++ Monto.toString totalFijos ++ ") superan el monto del gasto (" ++ Monto.toString totalPago ++ ")"

        ErrorPartesTotalNoCoincide totalFijos totalPago ->
            "El total de los montos fijos (" ++ Monto.toString totalFijos ++ ") debería ser igual al monto del gasto (" ++ Monto.toString totalPago ++ "), " ++ Monto.diffText totalFijos totalPago

        ErrorNoCalculado ->
            "Estamos recalculando este gasto. En un rato vuelve a estar disponible."


listarItems : List String -> String
listarItems nombres =
    let
        mostrados =
            3

        entrecomillados =
            List.map (\nombre -> "\"" ++ nombre ++ "\"") nombres

        restantes =
            List.length nombres - mostrados
    in
    if restantes > 0 then
        String.join ", " (List.take mostrados entrecomillados)
            ++ " y "
            ++ String.fromInt restantes
            ++ (if restantes == 1 then
                    " item más"

                else
                    " items más"
               )

    else
        case List.reverse entrecomillados of
            [] ->
                "algunos items"

            [ unico ] ->
                unico

            ultimo :: anteriores ->
                String.join ", " (List.reverse anteriores) ++ " y " ++ ultimo


errorAccionableEn : TipoErrorResumen -> List LugarParaAccionar
errorAccionableEn tipo =
    case tipo of
        ErrorRepartijaSinItems ->
            [ Lugar_CreacionPago ]

        ErrorRepartijaTotalItemsNoCoincide _ _ ->
            [ Lugar_CreacionPago ]

        ErrorRepartijaSinClaims ->
            [ Lugar_PaginaRepartija ]

        ErrorRepartijaItemsSinReclamar _ _ ->
            [ Lugar_PaginaRepartija ]

        ErrorRepartijaItemsReclamadosDeFormaMixta _ ->
            [ Lugar_PaginaRepartija ]

        ErrorRepartijaTotalReclamadoNoCoincide _ _ ->
            [ Lugar_PaginaRepartija ]

        ErrorPartesVacias ->
            [ Lugar_CreacionPago ]

        ErrorPartesMontoFijoSuperaTotal _ _ ->
            [ Lugar_CreacionPago ]

        ErrorPartesTotalNoCoincide _ _ ->
            [ Lugar_CreacionPago ]

        -- No hay nada que el usuario pueda hacer: lo resuelve el backend solo.
        ErrorNoCalculado ->
            []


type EstadoDelGasto
    = Valido
    | Incompleto
    | Roto


estadoQueDeja : TipoErrorResumen -> EstadoDelGasto
estadoQueDeja tipo =
    case tipo of
        ErrorRepartijaSinItems ->
            Roto

        ErrorRepartijaTotalItemsNoCoincide _ _ ->
            Roto

        ErrorRepartijaSinClaims ->
            Incompleto

        ErrorRepartijaItemsSinReclamar _ _ ->
            Incompleto

        ErrorRepartijaItemsReclamadosDeFormaMixta _ ->
            Roto

        ErrorRepartijaTotalReclamadoNoCoincide _ _ ->
            Roto

        ErrorPartesVacias ->
            Roto

        ErrorPartesMontoFijoSuperaTotal _ _ ->
            Roto

        ErrorPartesTotalNoCoincide _ _ ->
            Roto

        ErrorNoCalculado ->
            Incompleto


seArreglaReclamando : TipoErrorResumen -> Bool
seArreglaReclamando tipo =
    case tipo of
        ErrorRepartijaSinItems ->
            False

        ErrorRepartijaTotalItemsNoCoincide _ _ ->
            False

        ErrorRepartijaSinClaims ->
            True

        ErrorRepartijaItemsSinReclamar _ _ ->
            True

        ErrorRepartijaItemsReclamadosDeFormaMixta _ ->
            False

        ErrorRepartijaTotalReclamadoNoCoincide _ _ ->
            False

        ErrorPartesVacias ->
            False

        ErrorPartesMontoFijoSuperaTotal _ _ ->
            False

        ErrorPartesTotalNoCoincide _ _ ->
            False

        ErrorNoCalculado ->
            False


estadoDelGasto : List ErrorResumen -> EstadoDelGasto
estadoDelGasto errores =
    let
        estados =
            List.map (\error -> estadoQueDeja error.tipo) errores
    in
    if List.member Roto estados then
        Roto

    else if List.isEmpty estados then
        Valido

    else
        Incompleto


getDeudasFromResumen : ResumenNetos -> Maybe (Netos Monto)
getDeudasFromResumen resumen =
    Just resumen.netos
