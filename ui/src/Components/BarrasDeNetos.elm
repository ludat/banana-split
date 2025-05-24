module Components.BarrasDeNetos exposing (..)

import Css
import Generated.Api exposing (Grupo, Netos)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Models.Grupo exposing (lookupParticipante)
import Numeric.Decimal as Decimal
import Numeric.Decimal.Rounding as Decimal
import Numeric.Nat as Nat
import Numeric.Rational as Rational


viewNetosBarras : Grupo -> Netos -> Html msg
viewNetosBarras grupo netos =
    let
        maximo =
            netos.netos
                |> List.map
                    (\( _, ( _, numerador, denominador ) ) ->
                        Decimal.fromRational Decimal.RoundTowardsZero Nat.nat2 (Rational.ratio numerador denominador)
                            |> Result.withDefault (Decimal.fromInt Decimal.RoundTowardsZero Nat.nat2 0)
                            |> Decimal.abs
                    )
                |> List.map Decimal.toFloat
                |> List.maximum
                |> Maybe.withDefault 0
    in
    div [ class "fixed-grid" ]
        [ div [ class "grid", Css.barras_precio ]
            (netos.netos
                |> List.sortBy
                    (\( _, ( _, numerador, denominador ) ) ->
                        Decimal.fromRational Decimal.RoundTowardsZero Nat.nat2 (Rational.ratio numerador denominador)
                            |> Result.withDefault (Decimal.fromInt Decimal.RoundTowardsZero Nat.nat2 0)
                            |> Decimal.toFloat
                    )
                |> List.reverse
                |> List.concatMap
                    (\( participanteId, ( _, numerador, denominador ) ) ->
                        let
                            monto =
                                Decimal.fromRational Decimal.RoundTowardsZero Nat.nat2 (Rational.ratio numerador denominador)
                                    |> Result.withDefault (Decimal.fromInt Decimal.RoundTowardsZero Nat.nat2 0)

                            participante =
                                lookupParticipante grupo participanteId

                            nombreDerecha =
                                div [ class "cell nombre derecha ml-2" ] [ text participante.participanteNombre ]

                            nombreIzquierda =
                                div [ class "cell nombre izquierda mr-2" ] [ text participante.participanteNombre ]

                            barraIzquierda =
                                div [ class "cell monto izquierda" ]
                                    [ p
                                        [ class "mr-2" ]
                                        [ text <| Decimal.toString monto ]
                                    , div
                                        [ style "width" <| String.fromFloat (abs (Decimal.toFloat monto) * 100 / maximo) ++ "%"
                                        , class "barra has-background-danger"
                                        ]
                                        []
                                    ]

                            barraDerecha =
                                div [ class "cell monto derecha" ]
                                    [ p
                                        [ class "ml-2" ]
                                        [ text <| Decimal.toString monto ]
                                    , div
                                        [ style "width" <| String.fromFloat (Decimal.toFloat monto * 100 / maximo) ++ "%"
                                        , class "barra has-background-success"
                                        ]
                                        []
                                    ]
                        in
                        case compare numerador 0 of
                            LT ->
                                [ barraIzquierda
                                , nombreDerecha
                                ]

                            EQ ->
                                [ nombreIzquierda
                                , barraDerecha
                                ]

                            GT ->
                                [ nombreIzquierda
                                , barraDerecha
                                ]
                    )
                |> List.append [ div [ class "eje-vertical" ] [] ]
            )
        ]
