module Models.Grupo exposing (GrupoLike, grupoIdFromPath, isOwnedBy, lookupNombreParticipante, lookupParticipante, ownedParticipante)

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

        Path.Cuenta ->
            Nothing


lookupParticipante : GrupoLike g -> ParticipanteId -> Participante
lookupParticipante grupo participanteId =
    grupo.participantes
        |> List.filter (\p -> p.id == participanteId)
        |> List.head
        |> Maybe.withDefault { id = participanteId, nombre = "Desconocido", user = Nothing }


lookupNombreParticipante : GrupoLike g -> ParticipanteId -> String
lookupNombreParticipante grupo participanteId =
    lookupParticipante grupo participanteId |> .nombre


{-| Whether a participante is claimed by the given account (its `user`).
-}
isOwnedBy : ULID -> Participante -> Bool
isOwnedBy accountUserId participante =
    (participante.user |> Maybe.map .id) == Just accountUserId


{-| The participante the given account has claimed in this grupo, if any. A user
owns at most one participante per grupo.
-}
ownedParticipante : ULID -> GrupoLike r -> Maybe Participante
ownedParticipante accountUserId grupo =
    grupo.participantes
        |> List.filter (isOwnedBy accountUserId)
        |> List.head
