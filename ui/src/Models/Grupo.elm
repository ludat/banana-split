module Models.Grupo exposing (GrupoLike, claimErrorMessage, datosDeCobro, estaCongelado, findParticipante, grupoIdFromPath, isOwnedBy, lookupNombreParticipante, lookupParticipante, ownedParticipante, participanteErrorFromHttp, participanteErrorMessage)

import Generated.Api exposing (Participante, ParticipanteClaimError(..), ParticipanteError(..), ParticipanteId, ParticipanteUsage(..), ULID)
import Http
import Route.Path as Path
import Time


{-| Un grupo está congelado mientras tenga fecha de congelamiento: la fecha es
lo único que se guarda, y sirve además para separar las transferencias hechas
durante este congelamiento de las de los anteriores.
-}
estaCongelado : { r | congeladoAt : Maybe Time.Posix } -> Bool
estaCongelado grupo =
    grupo.congeladoAt /= Nothing


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

        Path.Grupos_GrupoId__Gastos params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Gastos_New params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Gastos_GastoId_ params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Pagos params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Pagos_New params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Pagos_PagoId_ params ->
            Just params.grupoId

        Path.Grupos_GrupoId__Transferencias params ->
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

        Path.Landing ->
            Nothing


lookupParticipante : GrupoLike g -> ParticipanteId -> Participante
lookupParticipante grupo participanteId =
    findParticipante grupo participanteId
        |> Maybe.withDefault { id = participanteId, nombre = "Desconocido", user = Nothing, datosDeCobro = Nothing }


findParticipante : GrupoLike g -> ParticipanteId -> Maybe Participante
findParticipante grupo participanteId =
    grupo.participantes
        |> List.filter (\p -> p.id == participanteId)
        |> List.head


{-| Cómo quiere recibir la plata: lo que se cargó en el participante y, si no
tiene nada, lo de la cuenta que lo reclamó. Así alcanza con cargarlo una vez en
la cuenta en vez de en cada grupo.
-}
datosDeCobro : Participante -> Maybe String
datosDeCobro participante =
    case participante.datosDeCobro of
        Just datos ->
            Just datos

        Nothing ->
            participante.user |> Maybe.andThen .datosDeCobro


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


{-| A user-facing message for each way a claim can be refused.
-}
claimErrorMessage : ParticipanteClaimError -> String
claimErrorMessage reason =
    case reason of
        ClaimedByOtherUser ->
            "Ese participante ya fue reclamado por otra persona"

        AlreadyOwnAnotherParticipante ->
            "Ya reclamaste otro participante en este grupo. Dejá de reclamarlo primero."

        ParticipanteError NameAlreadyTaken ->
            "Ya hay un participante con ese nombre. Si sos vos, elegilo de la lista."

        ParticipanteError error ->
            participanteErrorMessage error


{-| A user-facing message for each way creating, editing or deleting a
participante can be refused.
-}
participanteErrorMessage : ParticipanteError -> String
participanteErrorMessage error =
    case error of
        ParticipanteNotFound ->
            "No encontramos ese participante"

        EmptyName ->
            "Poné un nombre"

        NameAlreadyTaken ->
            "Ya hay un participante con ese nombre"

        ParticipanteInUse UsedInGasto ->
            "No se puede borrar: aparece en algún gasto. Sacalo de los gastos primero."

        ParticipanteInUse UsedInRepartija ->
            "No se puede borrar: reclamó items de una repartija. Sacale esos items primero."

        ParticipanteInUse UsedInTransferencia ->
            "No se puede borrar: tiene transferencias de cuando se congeló el grupo."


{-| Crear y editar un participante no devuelven un resultado tipado: rechazan
con un status, y este es el 'ParticipanteError' que corresponde a cada uno.
-}
participanteErrorFromHttp : Http.Error -> Maybe ParticipanteError
participanteErrorFromHttp error =
    case error of
        Http.BadStatus 400 ->
            Just EmptyName

        Http.BadStatus 404 ->
            Just ParticipanteNotFound

        Http.BadStatus 409 ->
            Just NameAlreadyTaken

        _ ->
            Nothing
