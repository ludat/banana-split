module Components.ResumenGasto exposing (esValido, viewErrores, viewMiParte)

{-| Lo que la lista de gastos muestra del resumen que trae cada `ShallowPago`:
por qué un gasto es inválido, y cuánto puso y consumió el participante que está
mirando.

El `resumen` viene en `Maybe` porque puede faltar si el cache está frío, pero
los endpoints reparan antes de listar, así que en la práctica siempre está.

-}

import Generated.Api exposing (Moneda, Monto, Netos, ParticipanteId, ShallowPago, ULID)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class)
import Models.Moneda as Moneda
import Models.Monto as Monto
import Models.ResumenNetos exposing (errorMensaje)


esValido : ShallowPago -> Bool
esValido pago =
    case pago.resumen of
        Just resumen ->
            List.isEmpty resumen.errores

        Nothing ->
            False


{-| Los motivos por los que el gasto es inválido, uno por línea. Antes de tener
el resumen guardado esto era sólo un ícono de alerta sin explicación.
-}
viewErrores : ShallowPago -> Html msg
viewErrores pago =
    case pago.resumen of
        Just resumen ->
            if List.isEmpty resumen.errores then
                text ""

            else
                div [ class "text-warning small" ]
                    (resumen.errores |> List.map (\error -> div [] [ text (errorMensaje error.tipo) ]))

        Nothing ->
            text ""


{-| Cuánto puso y cuánto consumió en este gasto el participante que está
mirando. Se omite si no hay participante elegido o si no participó del gasto.
-}
viewMiParte : Maybe ULID -> Moneda -> ShallowPago -> Html msg
viewMiParte participanteId monedaPorDefecto pago =
    case ( participanteId, pago.resumen ) of
        ( Just yo, Just resumen ) ->
            let
                puso =
                    montoDe yo resumen.pagado

                consumio =
                    montoDe yo resumen.consumido
            in
            case ( puso, consumio ) of
                ( Nothing, Nothing ) ->
                    text ""

                _ ->
                    let
                        simbolo =
                            Moneda.simbolo monedaPorDefecto pago.moneda
                    in
                    div [ class "text-muted small d-flex gap-2" ]
                        [ viewLado "Pusiste" simbolo puso
                        , viewLado "Consumiste" simbolo consumio
                        ]

        _ ->
            text ""


viewLado : String -> String -> Maybe Monto -> Html msg
viewLado etiqueta simbolo monto =
    case monto of
        Just m ->
            span [] [ text (etiqueta ++ " " ++ simbolo ++ " " ++ Monto.toString m) ]

        Nothing ->
            text ""


montoDe : ParticipanteId -> Netos Monto -> Maybe Monto
montoDe participanteId netos =
    netos
        |> List.filter (\( unId, _ ) -> unId == participanteId)
        |> List.head
        |> Maybe.map Tuple.second
