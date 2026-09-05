module Components.ResumenGasto exposing (viewBadgeRepartija, viewFila, viewIconoInvalido)

{-| La fila de un gasto en la lista, con lo que el resumen cacheado agrega:
cuánto consumió y cuánto pagó el participante que está mirando, por qué el gasto
es inválido, y cuánta gente reclamó si es una repartija.

El `resumen` viene en `Maybe` porque puede faltar si el cache está frío, pero
los endpoints reparan antes de listar, así que en la práctica siempre está.

-}

import Generated.Api exposing (Moneda, Monto, Netos, ParticipanteId, ShallowPago, ULID)
import Html exposing (Html, div, i, span, text)
import Html.Attributes exposing (class, title)
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


{-| El cuerpo de la fila: nombre, monto total y la línea personal. Los tres
tamaños bajan escalonadamente para que el nombre mande y el resumen no compita.
-}
viewFila : Maybe ULID -> Moneda -> ShallowPago -> Html msg
viewFila participanteId monedaPorDefecto pago =
    div [ class "flex-grow-1 text-truncate" ]
        [ div [ class "fw-semibold text-truncate", Html.Attributes.style "font-size" "1.05rem" ]
            [ text pago.nombre ]
        , div [ class "text-body-secondary", Html.Attributes.style "font-size" "0.85rem" ]
            [ text (Moneda.simbolo monedaPorDefecto pago.moneda ++ " " ++ Monto.toString pago.monto) ]
        , viewMiParte participanteId monedaPorDefecto pago
        ]


{-| El triángulo de alerta, con los motivos como tooltip. Antes de tener el
resumen guardado el ícono no podía explicar nada.
-}
viewIconoInvalido : ShallowPago -> Html msg
viewIconoInvalido pago =
    if esValido pago then
        text ""

    else
        i
            [ class "bi bi-exclamation-triangle-fill text-warning flex-shrink-0"
            , title (motivos pago)
            ]
            []


motivos : ShallowPago -> String
motivos pago =
    case pago.resumen of
        Just resumen ->
            resumen.errores
                |> List.map (\error -> errorMensaje error.tipo)
                |> String.join " "

        Nothing ->
            "Todavía no se calculó el estado de este gasto."


{-| Cuánta gente reclamó en la repartija del gasto. No aparece si el gasto no se
reparte por repartija.
-}
viewBadgeRepartija : ShallowPago -> Html msg
viewBadgeRepartija pago =
    case pago.resumen |> Maybe.andThen .participantesEnRepartija of
        Just cuantos ->
            span
                [ class "badge bg-body-secondary text-body-secondary d-flex align-items-center gap-1 flex-shrink-0"
                , title
                    (if cuantos == 1 then
                        "1 persona reclamó items"

                     else
                        String.fromInt cuantos ++ " personas reclamaron items"
                    )
                ]
                [ text (String.fromInt cuantos)
                , i [ class "bi bi-people-fill" ] []
                ]

        Nothing ->
            text ""


{-| "Consumí X | Pagué Y" para el participante que está mirando. Lo consumido va
en rojo y lo pagado en verde, que es la misma convención que los netos. Se omite
si no hay participante elegido o si no participó del gasto.
-}
viewMiParte : Maybe ULID -> Moneda -> ShallowPago -> Html msg
viewMiParte participanteId monedaPorDefecto pago =
    case ( participanteId, pago.resumen ) of
        ( Just yo, Just resumen ) ->
            let
                simbolo =
                    Moneda.simbolo monedaPorDefecto pago.moneda

                lados =
                    List.filterMap identity
                        [ viewLado "Pagué" "text-success" simbolo (montoDe yo resumen.pagado)
                        , viewLado "Consumí" "text-danger" simbolo (montoDe yo resumen.consumido)
                        ]
            in
            if List.isEmpty lados then
                text ""

            else
                div
                    [ class "d-flex align-items-center gap-2 text-body-secondary"
                    , Html.Attributes.style "font-size" "0.75rem"
                    ]
                    (List.intersperse separador lados)

        _ ->
            text ""


separador : Html msg
separador =
    span [ class "text-body-tertiary" ] [ text "|" ]


{-| Un cero no se muestra. El cache guarda una fila por cada participante que
aparece de alguno de los dos lados, con cero en el que no le toca, así que
"Consumí 0" es la forma que tiene de decir que no consumió nada.
-}
viewLado : String -> String -> String -> Maybe Monto -> Maybe (Html msg)
viewLado etiqueta colorDelMonto simbolo monto =
    monto
        |> Maybe.andThen
            (\m ->
                if m.valor == 0 then
                    Nothing

                else
                    Just m
            )
        |> Maybe.map
            (\m ->
                span [ class "d-flex gap-1" ]
                    [ text etiqueta
                    , span [ class colorDelMonto ] [ text (simbolo ++ " " ++ Monto.toString m) ]
                    ]
            )


montoDe : ParticipanteId -> Netos Monto -> Maybe Monto
montoDe participanteId netos =
    netos
        |> List.filter (\( unId, _ ) -> unId == participanteId)
        |> List.head
        |> Maybe.map Tuple.second
