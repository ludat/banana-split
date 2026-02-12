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
    div [ Css.barras_precio ]
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
                            div [ class "nombre derecha", style "margin-left" "0.5rem" ] [ text participante.participanteNombre ]

                        nombreIzquierda =
                            div [ class "nombre izquierda", style "margin-right" "0.5rem" ] [ text participante.participanteNombre ]

                        barraIzquierda =
                            div [ class "monto izquierda" ]
                                [ p
                                    [ style "margin-right" "0.5rem" ]
                                    [ text <| Decimal.toString monto ]
                                , div
                                    [ style "width" <| String.fromFloat (abs (Decimal.toFloat monto) * 100 / maximo) ++ "%"
                                    , class "barra"
                                    , style "background-color" "var(--sapErrorColor, rgb(72, 199, 142))"
                                    ]
                                    []
                                ]

                        barraDerecha =
                            div [ class "monto derecha" ]
                                [ p
                                    [ style "margin-left" "0.5rem" ]
                                    [ text <| Decimal.toString monto ]
                                , div
                                    [ style "width" <| String.fromFloat (Decimal.toFloat monto * 100 / maximo) ++ "%"
                                    , class "barra"
                                    , style "background-color" "var(--sapPositiveColor, rgb(255, 102, 133))"
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
            |> List.append [ div [ Css.eje_vertical ] [] ]
        )
