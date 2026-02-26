module Components.BarrasDeNetos exposing (viewNetosBarras)

import Components.Ui5 as Ui5
import Css
import Generated.Api exposing (Monto, Netos)
import Html exposing (Html, div, p)
import Html.Attributes exposing (class, style)
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

                        nombreIzquierda =
                            div [ class "nombre izquierda", style "margin-right" "0.5rem" ] [ Ui5.text participante.participanteNombre ]

                        barraDerecha =
                            div [ class "monto derecha" ]
                                [ p
                                    [ style "margin-left" "0.5rem" ]
                                    [ Ui5.text <| Decimal.toString monto ]
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
                            let
                                nombreDerecha =
                                    div [ class "nombre derecha", style "margin-left" "0.5rem" ] [ Ui5.text participante.participanteNombre ]

                                barraIzquierda =
                                    div [ class "monto izquierda" ]
                                        [ p
                                            [ style "margin-right" "0.5rem" ]
                                            [ Ui5.text <| Decimal.toString monto ]
                                        , div
                                            [ style "width" <| String.fromFloat (abs (Decimal.toFloat monto) * 100 / maximo) ++ "%"
                                            , class "barra"
                                            , style "background-color" "var(--sapErrorColor, rgb(72, 199, 142))"
                                            ]
                                            []
                                        ]
                            in
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
