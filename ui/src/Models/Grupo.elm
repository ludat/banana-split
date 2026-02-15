module Models.Grupo exposing (GrupoLike, lookupNombreParticipante, lookupParticipante)

import Generated.Api exposing (Participante, ParticipanteId, ULID)


type alias GrupoLike r =
    { r
        | id : ULID
        , participantes : List Participante
    }


lookupParticipante : GrupoLike g -> ParticipanteId -> Participante
lookupParticipante grupo participanteId =
    grupo.participantes
        |> List.filter (\p -> p.participanteId == participanteId)
        |> List.head
        |> Maybe.withDefault { participanteId = participanteId, participanteNombre = "Desconocido" }


lookupNombreParticipante : GrupoLike g -> ParticipanteId -> String
lookupNombreParticipante grupo participanteId =
    lookupParticipante grupo participanteId |> .participanteNombre
