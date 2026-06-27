module Components.BarrasDeNetos exposing (viewNetosBarras, viewNetosBarrasMini)

import Css
import Generated.Api exposing (Monto, Netos)
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class, style)
import Models.Grupo exposing (GrupoLike, lookupParticipante)
import Models.Monto as Monto
import Svg
import Svg.Attributes as SvgAttr


viewNetosBarrasMini : Netos Monto -> Html msg
viewNetosBarrasMini netos =
    let
        maximo =
            netos
                |> List.map (\( _, m ) -> abs (Monto.toFloat m))
                |> List.maximum
                |> Maybe.withDefault 0

        ordenados =
            netos
                |> List.sortBy (\( _, m ) -> m.valor)
                |> List.reverse

        cantidad =
            List.length ordenados

        centro =
            50

        altoFila =
            if cantidad > 0 then
                100 / toFloat cantidad

            else
                100

        altoBarra =
            altoFila * 0.6

        viewBarra indice ( _, monto ) =
            let
                valor =
                    Monto.toFloat monto

                largo =
                    if maximo > 0 then
                        abs valor / maximo * centro

                    else
                        0

                y =
                    toFloat indice * altoFila + (altoFila - altoBarra) / 2

                ( x, color ) =
                    if valor < 0 then
                        ( centro - largo, "var(--bs-danger)" )

                    else
                        ( centro, "var(--bs-success)" )
            in
            Svg.rect
                [ SvgAttr.x (String.fromFloat x)
                , SvgAttr.y (String.fromFloat y)
                , SvgAttr.width (String.fromFloat largo)
                , SvgAttr.height (String.fromFloat altoBarra)
                , SvgAttr.fill color
                ]
                []
    in
    div
        [ class "border rounded d-flex align-items-center justify-content-center bg-body p-1"
        , style "width" "3.5rem"
        , style "height" "3.5rem"
        ]
        [ Svg.svg
            [ SvgAttr.viewBox "0 0 100 100"
            , SvgAttr.width "100%"
            , SvgAttr.height "100%"
            ]
            (Svg.line
                [ SvgAttr.x1 (String.fromFloat centro)
                , SvgAttr.y1 "0"
                , SvgAttr.x2 (String.fromFloat centro)
                , SvgAttr.y2 "100"
                , SvgAttr.stroke "var(--bs-border-color)"
                , SvgAttr.strokeWidth "1"
                ]
                []
                :: List.indexedMap viewBarra ordenados
            )
        ]


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
