module Models.Grupo exposing (lookupParticipante)

import Generated.Api exposing (Grupo, Participante, ParticipanteId)


lookupParticipante : Grupo -> ParticipanteId -> Participante
lookupParticipante grupo participanteId =
    grupo.participantes
        |> List.filter (\p -> p.participanteId == participanteId)
        |> List.head
        |> Maybe.withDefault { participanteId = participanteId, participanteNombre = "Desconocido" }
