module Models.Repartija exposing
    ( Delta(..)
    , ItemRepartidoState(..)
    , ParteDeParticipante
    , claimsDeItem
    , compararConCero
    , estaResuelto
    , estadoDeItem
    , hayItemsNoResueltos
    , parteDeParticipante
    , total
    )

{-| Lo que se puede saber de una repartija sin mirar la pantalla: en qué estado
quedó cada item según sus claims, y cuánto le toca pagar a cada participante.

Lo usan la página de la repartija y el header del layout, que muestra una alerta
en el título cuando todavía hay items sin resolver.

-}

import Generated.Api exposing (DistribucionDeSobras(..), Monto, ParticipanteId, Repartija, RepartijaClaim, RepartijaItem)
import List.Extra
import Models.Monto as Monto


type ItemClaimsState
    = MixedClaims
    | OnlyExactClaims (List ( Int, RepartijaClaim ))
    | OnlyParticipationClaims (List RepartijaClaim)
    | NoClaims


type ItemRepartidoState
    = SinRepartir
    | RepartidoIncorrectamente { cantidadDeParticipantes : Int }
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
    let
        claims =
            claimsDeItem repartija item
    in
    case interpretClaims claims of
        MixedClaims ->
            RepartidoIncorrectamente { cantidadDeParticipantes = List.length claims }

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


{-| Un item está resuelto cuando no queda nada que hacer con él: por cantidad se
reclamaron exactamente las unidades que hay, o equitativo entre al menos dos.
Todo lo demás lleva la etiqueta de alerta en la página.
-}
estaResuelto : ItemRepartidoState -> Bool
estaResuelto estado =
    case estado of
        SinRepartir ->
            False

        RepartidoIncorrectamente _ ->
            False

        RepartidoExactamente { deltaDeCantidad } ->
            deltaDeCantidad == 0

        RepartidoEquitativamenteEntre { cantidadDeParticipantes } ->
            cantidadDeParticipantes >= 2


hayItemsNoResueltos : Repartija -> Bool
hayItemsNoResueltos repartija =
    repartija.items
        |> List.any (\item -> not (estaResuelto (estadoDeItem repartija item)))


total : Repartija -> Monto
total repartija =
    repartija.items
        |> List.map .monto
        |> List.foldl Monto.add Monto.zero
        |> Monto.add repartija.extra


type alias ParteDeParticipante =
    { consumoPorItem : List ( RepartijaItem, Monto )
    , propina : Monto
    , total : Monto
    }


{-| Cuánto le toca a un participante. Replica `calcularNetosRepartija` del
backend: cada item se reparte según sus claims, lo que nadie reclamó queda sin
dueño, y la propina (más lo no reclamado, si la repartija lo distribuye
proporcionalmente) se reparte en proporción a lo que consumió cada uno.

Es para mostrar: redondea cada parte por separado, así que puede diferir en
algún centavo de lo que termina calculando el backend.

-}
parteDeParticipante : Repartija -> ParticipanteId -> ParteDeParticipante
parteDeParticipante repartija participanteId =
    let
        -- Para cada item: lo que le toca a cada participante y lo que quedó
        -- sin repartir, como fracciones del monto del item.
        fracciones : RepartijaItem -> { de : ParticipanteId -> Float, sinRepartir : Float }
        fracciones item =
            let
                claims =
                    claimsDeItem repartija item
            in
            case interpretClaims claims of
                OnlyExactClaims exactClaims ->
                    let
                        reclamado =
                            exactClaims |> List.map Tuple.first |> List.sum

                        sobrante =
                            max 0 (item.cantidad - reclamado)

                        divisor =
                            Basics.toFloat (reclamado + sobrante)
                    in
                    if divisor == 0 then
                        { de = always 0, sinRepartir = 0 }

                    else
                        { de =
                            \p ->
                                exactClaims
                                    |> List.filter (\( _, c ) -> c.participante == p)
                                    |> List.map Tuple.first
                                    |> List.sum
                                    |> (\n -> Basics.toFloat n / divisor)
                        , sinRepartir = Basics.toFloat sobrante / divisor
                        }

                OnlyParticipationClaims participationClaims ->
                    let
                        cantidad =
                            Basics.toFloat (List.length participationClaims)
                    in
                    { de =
                        \p ->
                            if List.any (\c -> c.participante == p) participationClaims then
                                1 / cantidad

                            else
                                0
                    , sinRepartir = 0
                    }

                NoClaims ->
                    { de = always 0, sinRepartir = 1 }

                -- El backend no reparte los items mal repartidos: no le cuentan
                -- a nadie, ni siquiera como sobras.
                MixedClaims ->
                    { de = always 0, sinRepartir = 0 }

        consumoDe : ParticipanteId -> Float
        consumoDe p =
            repartija.items
                |> List.map (\item -> (fracciones item).de p * Monto.toFloat item.monto)
                |> List.sum

        participantes =
            repartija.claims
                |> List.map .participante
                |> List.Extra.unique

        consumoTotal =
            participantes |> List.map consumoDe |> List.sum

        proporcion =
            if consumoTotal == 0 then
                0

            else
                consumoDe participanteId / consumoTotal

        consumoPorItem =
            repartija.items
                |> List.filterMap
                    (\item ->
                        let
                            f =
                                (fracciones item).de participanteId
                        in
                        if f == 0 then
                            Nothing

                        else
                            Just ( item, escalar f item.monto )
                    )

        propina =
            escalar proporcion repartija.extra

        sobras =
            case repartija.distribucionDeSobras of
                SobrasNoDistribuir ->
                    Monto.zero

                SobrasProporcional ->
                    let
                        sinRepartir =
                            repartija.items
                                |> List.map (\item -> (fracciones item).sinRepartir * Monto.toFloat item.monto)
                                |> List.sum
                    in
                    escalar proporcion (deFloat repartija.extra.lugaresDespuesDeLaComa sinRepartir)
    in
    { consumoPorItem = consumoPorItem
    , propina = propina
    , total =
        consumoPorItem
            |> List.map Tuple.second
            |> List.foldl Monto.add Monto.zero
            |> Monto.add propina
            |> Monto.add sobras
    }


escalar : Float -> Monto -> Monto
escalar factor monto =
    { monto | valor = round (Basics.toFloat monto.valor * factor) }


deFloat : Int -> Float -> Monto
deFloat lugaresDespuesDeLaComa n =
    { lugaresDespuesDeLaComa = lugaresDespuesDeLaComa
    , valor = round (n * Basics.toFloat (10 ^ lugaresDespuesDeLaComa))
    }
