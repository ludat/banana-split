module Components.BarrasDeNetos exposing (viewNetosBarras)

import Css
import Generated.Api exposing (Monto, Netos)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, style)
import Models.Grupo exposing (GrupoLike, lookupParticipante)
import Models.Monto as Monto


viewNetosBarras : GrupoLike g -> Netos Monto -> Html msg
viewNetosBarras grupo netos =
    let
        maximo =
            netos
                |> List.map
                    (\( _, m ) ->
                        Monto.abs m
                    )
                |> List.map Monto.toFloat
                |> List.maximum
                |> Maybe.withDefault 0
    in
    div [ Css.barras_precio ]
        (netos
            |> List.sortBy
                (\( _, m ) -> m.valor)
            |> List.reverse
            |> List.concatMap
                (\( participanteId, monto ) ->
                    let
                        participante =
                            lookupParticipante grupo participanteId

                        nombreIzquierda =
                            div [ class "nombre izquierda", style "margin-right" "0.5rem" ] [ text participante.nombre ]

                        barraDerecha =
                            div [ class "monto derecha" ]
                                [ p
                                    [ style "margin-left" "0.5rem" ]
                                    [ text <| Monto.toString monto ]
                                , div
                                    [ style "width" <| String.fromFloat (Monto.toFloat monto * 100 / maximo) ++ "%"
                                    , class "barra"
                                    , style "background-color" "var(--bs-success)"
                                    ]
                                    []
                                ]
                    in
                    case compare monto.valor 0 of
                        LT ->
                            let
                                nombreDerecha =
                                    div [ class "nombre derecha", style "margin-left" "0.5rem" ] [ text participante.nombre ]

                                barraIzquierda =
                                    div [ class "monto izquierda" ]
                                        [ p
                                            [ style "margin-right" "0.5rem" ]
                                            [ text <| Monto.toString monto ]
                                        , div
                                            [ style "width" <| String.fromFloat (abs (Monto.toFloat monto) * 100 / maximo) ++ "%"
                                            , class "barra"
                                            , style "background-color" "var(--bs-danger)"
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
