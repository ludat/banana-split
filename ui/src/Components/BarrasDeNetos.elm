module Components.BarrasDeNetos exposing (..)

import Css
import Generated.Api exposing (Grupo, Monto, Netos)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Models.Grupo exposing (GrupoLike, lookupParticipante)
import Models.Monto as Monto
import Numeric.Decimal as Decimal


viewNetosBarras : GrupoLike g -> Netos Monto -> Html msg
viewNetosBarras grupo netos =
    let
        maximo =
            netos
                |> List.map
                    (\( _, m ) ->
                        Monto.abs m
                    )
                |> List.map (Monto.toDecimal >> Decimal.toFloat)
                |> List.maximum
                |> Maybe.withDefault 0
    in
    div [ class "fixed-grid" ]
        [ div [ class "grid", Css.barras_precio ]
            (netos
                |> List.sortBy
                    (\( _, m ) -> m.valor)
                |> List.reverse
                |> List.concatMap
                    (\( participanteId, m ) ->
                        let
                            monto =
                                Monto.toDecimal m

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
                        case compare m.valor 0 of
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
