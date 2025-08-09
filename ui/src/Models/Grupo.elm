module Models.Grupo exposing (GrupoLike, lookupNombreParticipante, lookupParticipante, lookupParticipantes)

import Generated.Api exposing (Grupo, Participante, ParticipanteAddParams, ParticipanteId)


type alias GrupoLike r =
    { r | participantes : List Participante }


lookupParticipantes : List Participante -> ParticipanteId -> Participante
lookupParticipantes participantes participanteId =
    participantes
        |> List.filter (\p -> p.participanteId == participanteId)
        |> List.head
        |> Maybe.withDefault { participanteId = participanteId, participanteNombre = "Desconocido" }


lookupParticipante : GrupoLike g -> ParticipanteId -> Participante
lookupParticipante grupo participanteId =
    grupo.participantes
        |> List.filter (\p -> p.participanteId == participanteId)
        |> List.head
        |> Maybe.withDefault { participanteId = participanteId, participanteNombre = "Desconocido" }


lookupNombreParticipante : GrupoLike g -> ParticipanteId -> String
lookupNombreParticipante grupo participanteId =
    lookupParticipante grupo participanteId |> .participanteNombre
