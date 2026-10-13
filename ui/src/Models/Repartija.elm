module Models.Repartija exposing
    ( Delta(..)
    , ItemRepartidoState(..)
    , claimsDeItem
    , compararConCero
    , consumoDeItem
    , estaResuelto
    , estadoDeItem
    , hayItemsNoResueltos
    , itemsNoResueltos
    , parteDe
    , total
    )

{-| En qué estado quedó cada item de una repartija según sus claims, que es lo
que decide qué botones y qué etiquetas mostrar.

La plata no se calcula acá: cuánto le toca a cada participante de cada item, de
la propina y de lo que nadie reclamó viene resuelto del backend, en el `detalle`
de la repartija (`BananaSplit.Deudas.detalleRepartija`). Este módulo solo lo lee,
porque tener la cuenta escrita dos veces es tenerla mal en una.

Lo usan la página de la repartija y el header del layout, que muestra una alerta
en el título cuando todavía hay items sin resolver.

-}

import Generated.Api exposing (DetalleRepartija, Monto, ParticipanteId, Repartija, RepartijaClaim, RepartijaItem, ULID)
import List.Extra
import Models.Monto as Monto


type ItemClaimsState
    = MixedClaims
    | OnlyExactClaims (List ( Int, RepartijaClaim ))
    | OnlyParticipationClaims (List RepartijaClaim)
    | NoClaims


type ItemRepartidoState
    = SinRepartir
    | RepartidoIncorrectamente
    | RepartidoExactamente { cantidadReclamada : Int, deltaDeCantidad : Int }
    | RepartidoEquitativamenteEntre { cantidadDeParticipantes : Int }


type Delta
    = SePasaPor Int
    | QuedaCortoPor Int
    | ExactamenteCero


compararConCero : Int -> Delta
compararConCero n =
    case compare n 0 of
        GT ->
            SePasaPor n

        EQ ->
            ExactamenteCero

        LT ->
            QuedaCortoPor <| negate n


claimsDeItem : Repartija -> RepartijaItem -> List RepartijaClaim
claimsDeItem repartija item =
    repartija.claims
        |> List.filter (\claim -> claim.itemId == item.id)


interpretClaims : List RepartijaClaim -> ItemClaimsState
interpretClaims originalClaims =
    let
        folder : RepartijaClaim -> ItemClaimsState -> ItemClaimsState
        folder claim claimsState =
            case ( claimsState, claim.cantidad ) of
                ( NoClaims, Just n ) ->
                    OnlyExactClaims [ ( n, claim ) ]

                ( NoClaims, Nothing ) ->
                    OnlyParticipationClaims [ claim ]

                ( MixedClaims, _ ) ->
                    MixedClaims

                ( OnlyExactClaims _, Nothing ) ->
                    MixedClaims

                ( OnlyExactClaims claims, Just n ) ->
                    OnlyExactClaims (claims ++ [ ( n, claim ) ])

                ( OnlyParticipationClaims claims, Nothing ) ->
                    OnlyParticipationClaims <| claims ++ [ claim ]

                ( OnlyParticipationClaims _, Just _ ) ->
                    MixedClaims
    in
    List.foldl folder NoClaims originalClaims


estadoDeItem : Repartija -> RepartijaItem -> ItemRepartidoState
estadoDeItem repartija item =
    case interpretClaims (claimsDeItem repartija item) of
        MixedClaims ->
            RepartidoIncorrectamente

        OnlyExactClaims exactClaims ->
            let
                cantidadReclamada =
                    exactClaims |> List.map Tuple.first |> List.sum
            in
            RepartidoExactamente
                { cantidadReclamada = cantidadReclamada
                , deltaDeCantidad = cantidadReclamada - item.cantidad
                }

        OnlyParticipationClaims repartijaClaims ->
            RepartidoEquitativamenteEntre { cantidadDeParticipantes = List.length repartijaClaims }

        NoClaims ->
            SinRepartir


{-| Un item está repartido cuando toda su plata tiene dueño. Lo que no llega a
eso lleva la etiqueta "No resuelto" y entra en la cuenta de la alerta de arriba.

Reclamar de más no deja plata sin dueño —el backend la reparte en proporción—,
así que cuenta como repartido; que sobren unidades se avisa aparte. Un item
repartido entre cantidades y partes iguales a la vez, en cambio, el backend no
se lo cobra a nadie.

-}
estaResuelto : ItemRepartidoState -> Bool
estaResuelto estado =
    case estado of
        SinRepartir ->
            False

        RepartidoIncorrectamente ->
            False

        RepartidoExactamente { deltaDeCantidad } ->
            deltaDeCantidad >= 0

        RepartidoEquitativamenteEntre { cantidadDeParticipantes } ->
            cantidadDeParticipantes >= 1


hayItemsNoResueltos : Repartija -> Bool
hayItemsNoResueltos repartija =
    itemsNoResueltos repartija > 0


itemsNoResueltos : Repartija -> Int
itemsNoResueltos repartija =
    repartija.items
        |> List.filter (\item -> not (estaResuelto (estadoDeItem repartija item)))
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
    detalle.porItem
        |> List.Extra.find (\detalleDeItem -> detalleDeItem.itemId == itemId)
        |> Maybe.map (.consumos >> parteDe participanteId)
        |> Maybe.withDefault Monto.zero
