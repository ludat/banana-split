module Models.Parte exposing (participanteId)

import Generated.Api exposing (Parte(..), ParticipanteId)


participanteId : Parte -> ParticipanteId
participanteId parte =
    case parte of
        MontoFijo _ p ->
            p

        Ponderado _ p ->
            p

        PonderadoYMontoFijo _ _ p ->
            p
