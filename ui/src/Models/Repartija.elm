module Models.Repartija exposing
    ( claimsDeItem
    , consumoDeItem
    , detalleDeItem
    , hayItemsNoResueltos
    , itemsNoResueltos
    , parteDe
    , total
    )

{-| Lo que la página de la repartija y el header del layout leen de una
repartija.

Acá no se interpreta nada: cuánto le toca a cada participante de cada item, de
la propina y de lo que nadie reclamó, cómo se está repartiendo cada item y si
deja la repartija lista para repartir vienen resueltos del backend, en el
`detalle` de la repartija (`BananaSplit.Deudas.detalleRepartija`). Este módulo
solo lo busca, porque tener las reglas escritas dos veces es tenerlas mal en
una.

-}

import Generated.Api exposing (DetalleDeItem, DetalleRepartija, Monto, ParticipanteId, Repartija, RepartijaClaim, RepartijaItem, ResolucionDeItem(..), ULID)
import List.Extra
import Models.Monto as Monto


claimsDeItem : Repartija -> RepartijaItem -> List RepartijaClaim
claimsDeItem repartija item =
    repartija.claims
        |> List.filter (\claim -> claim.itemId == item.id)


{-| Lo que el backend calculó de un item: cuánto le toca a cada uno, cómo se
está repartiendo y si deja la repartija lista para repartir.
-}
detalleDeItem : DetalleRepartija -> ULID -> Maybe DetalleDeItem
detalleDeItem detalle itemId =
    detalle.porItem
        |> List.Extra.find (\deItem -> deItem.itemId == itemId)


hayItemsNoResueltos : DetalleRepartija -> Bool
hayItemsNoResueltos detalle =
    itemsNoResueltos detalle > 0


{-| Los items que hay que arreglar para que la repartija sea válida. Los que
tienen algo raro pero se reparten igual (`ItemConAdvertencia`) no cuentan.
-}
itemsNoResueltos : DetalleRepartija -> Int
itemsNoResueltos detalle =
    detalle.porItem
        |> List.filter (\deItem -> deItem.resolucion == ItemNoResuelto)
        |> List.length


total : Repartija -> Monto
total repartija =
    repartija.items
        |> List.map .monto
        |> List.foldl Monto.add Monto.zero
        |> Monto.add repartija.extra


{-| Lo que le toca a un participante de un reparto que ya vino calculado del
backend. Sin participante elegido —o si a esa persona no le toca nada— es cero.
-}
parteDe : Maybe ParticipanteId -> List ( ParticipanteId, Monto ) -> Monto
parteDe participanteId reparto =
    case participanteId of
        Nothing ->
            Monto.zero

        Just unId ->
            reparto
                |> List.Extra.find (\( p, _ ) -> p == unId)
                |> Maybe.map Tuple.second
                |> Maybe.withDefault Monto.zero


{-| Cuánto consumió un participante de un item puntual.
-}
consumoDeItem : DetalleRepartija -> ULID -> Maybe ParticipanteId -> Monto
consumoDeItem detalle itemId participanteId =
    detalleDeItem detalle itemId
        |> Maybe.map (.consumos >> parteDe participanteId)
        |> Maybe.withDefault Monto.zero
