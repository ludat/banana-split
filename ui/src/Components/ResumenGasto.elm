module Components.ResumenGasto exposing (esRepartijaActiva, leFaltaReclamar, viewBadgeRepartija, viewFila, viewIconoInvalido)

{-| La fila de un gasto en la lista, con lo que el resumen cacheado agrega:
cuánto consumió y cuánto pagó el participante que está mirando, por qué el gasto
es inválido, y cuánta gente reclamó si es una repartija.
-}

import Generated.Api exposing (Moneda, Monto, PagadoYConsumido, ShallowPago, ULID)
import Html exposing (Html, div, i, span, text)
import Html.Attributes exposing (class, title)
import Models.Moneda as Moneda
import Models.Monto as Monto
import Models.ResumenNetos exposing (EstadoDelGasto(..), errorMensaje, estadoDelGasto, seArreglaReclamando)


{-| El cuerpo de la fila: nombre, monto total y la línea personal. Los tres
tamaños bajan escalonadamente para que el nombre mande y el resumen no compita.
-}
viewFila : Maybe PagadoYConsumido -> Moneda -> ShallowPago -> Html msg
viewFila pagadoYConsumido monedaPorDefecto pago =
    div [ class "flex-grow-1 text-truncate" ]
        [ div [ class "fw-semibold text-truncate", Html.Attributes.style "font-size" "1.05rem" ]
            [ text pago.nombre ]
        , div [ class "text-body-secondary", Html.Attributes.style "font-size" "0.85rem" ]
            [ text (Moneda.simbolo monedaPorDefecto pago.moneda ++ " " ++ Monto.toString pago.monto) ]
        , viewMiParte pagadoYConsumido monedaPorDefecto pago
        ]


{-| El icono de un gasto que no cierra, con los motivos como tooltip.
-}
viewIconoInvalido : ShallowPago -> Html msg
viewIconoInvalido pago =
    case estadoDelGasto pago.resumen.errores of
        Valido ->
            text ""

        Roto ->
            i
                [ class "bi bi-exclamation-triangle-fill text-warning flex-shrink-0"
                , title (motivos pago)
                ]
                []

        Incompleto ->
            i
                [ class "bi bi-hourglass-split text-body-secondary flex-shrink-0"
                , title (motivos pago)
                ]
                []


esRepartijaActiva : ShallowPago -> Bool
esRepartijaActiva pago =
    List.any (\error -> seArreglaReclamando error.tipo) pago.resumen.errores


leFaltaReclamar : Maybe ULID -> ShallowPago -> Bool
leFaltaReclamar participanteId pago =
    Maybe.map2
        (\yo reclamaron ->
            esRepartijaActiva pago
                && not (List.member yo reclamaron)
                && not (List.member yo pago.resumen.noConsumieronEnRepartija)
        )
        participanteId
        pago.resumen.participantesEnRepartija
        |> Maybe.withDefault False


motivos : ShallowPago -> String
motivos pago =
    pago.resumen.errores
        |> List.map (\error -> errorMensaje error.tipo)
        |> String.join " "


{-| Cuánta gente reclamó en la repartija del gasto. No aparece si el gasto no se
reparte por repartija.
-}
viewBadgeRepartija : ShallowPago -> Html msg
viewBadgeRepartija pago =
    case Maybe.map List.length pago.resumen.participantesEnRepartija of
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
si no participó del gasto, o mientras no llegó.
-}
viewMiParte : Maybe PagadoYConsumido -> Moneda -> ShallowPago -> Html msg
viewMiParte pagadoYConsumido monedaPorDefecto pago =
    case pagadoYConsumido of
        Just mio ->
            let
                simbolo =
                    Moneda.simbolo monedaPorDefecto pago.moneda

                lados =
                    List.filterMap identity
                        [ viewLado "Pagué" "text-success" simbolo mio.pagado
                        , viewLado "Consumí" "text-danger" simbolo mio.consumido
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

        Nothing ->
            text ""


separador : Html msg
separador =
    span [ class "text-body-tertiary" ] [ text "|" ]


{-| Un cero no se muestra. El cache guarda una fila por cada participante que
aparece de alguno de los dos lados, con cero en el que no le toca, así que
"Consumí 0" es la forma que tiene de decir que no consumió nada.
-}
viewLado : String -> String -> String -> Monto -> Maybe (Html msg)
viewLado etiqueta colorDelMonto simbolo monto =
    if monto.valor == 0 then
        Nothing

    else
        Just <|
            span [ class "d-flex gap-1" ]
                [ text etiqueta
                , span [ class colorDelMonto ] [ text (simbolo ++ " " ++ Monto.toString monto) ]
                ]
