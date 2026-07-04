module Models.Grupo exposing (GrupoLike, grupoIdFromPath, lookupNombreParticipante, lookupParticipante)

import Generated.Api exposing (Participante, ParticipanteId, ULID)
import Route.Path as Path


type alias GrupoLike r =
    { r
        | id : ULID
        , participantes : List Participante
    }


{-| Recover the grupo id embedded in a route path, when there is one. Pages name
that route param differently (`grupoId` vs `id`), so callers that only have a
`Path` use this instead of reaching into route params.
-}
grupoIdFromPath : Path.Path -> Maybe ULID
grupoIdFromPath path =
    case path of
        Path.Grupos_Id_ params ->
            Just params.id

        Path.Grupos_GrupoId__Pagos params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Pagos_New params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Pagos_PagoId_ params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Liquidaciones params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Participantes params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Settings params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Repartijas_RepartijaId_ params ->
            Just params.grupoId

        Path.Home_ ->
            Nothing

        Path.NotFound_ ->
            Nothing

        Path.Login ->
            Nothing

        Path.Signup ->
            Nothing


lookupParticipante : GrupoLike g -> ParticipanteId -> Participante
lookupParticipante grupo participanteId =
    grupo.participantes
        |> List.filter (\p -> p.id == participanteId)
        |> List.head
        |> Maybe.withDefault { id = participanteId, nombre = "Desconocido" }


lookupNombreParticipante : GrupoLike g -> ParticipanteId -> String
lookupNombreParticipante grupo participanteId =
    lookupParticipante grupo participanteId |> .nombre
