module Components.Cotizaciones exposing (view)

import Generated.Api as Api exposing (Moneda, PorMoneda)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Models.Moneda as Moneda
import Models.Monto as Monto


{-| Las cotizaciones usadas para consolidar, p.ej. "1 U$D = $ 1.200,00".
-}
view : Moneda -> PorMoneda Api.Monto -> Html msg
view monedaDestino cotizaciones =
    div [ class "text-muted small" ]
        (cotizaciones
            |> List.filter (\( moneda, _ ) -> moneda /= monedaDestino)
            |> List.map
                (\( moneda, valor ) ->
                    span [ class "me-3 text-nowrap" ]
                        [ text <|
                            "1 "
                                ++ Moneda.simboloUnico moneda
                                ++ " = "
                                ++ Moneda.simbolo monedaDestino monedaDestino
                                ++ " "
                                ++ Monto.toString valor
                        ]
                )
        )
