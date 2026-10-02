module Models.Transferencia exposing (Estado(..), conFlechas, esPropuesta, estaHecha, estado, frase, monto, participante, progresoDelPlan)

import Generated.Api exposing (Grupo, Moneda, ParticipanteId, Transferencia)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Models.Grupo exposing (lookupNombreParticipante)
import Models.Moneda as Moneda
import Models.Monto as Monto
import Time exposing (Posix)


{-| Una transferencia guarda `saldadaAt` —cuándo se movió la plata, si se
movió—, que es lo que la db tiene. Para las vistas conviene el mismo dato como
dos casos, así el `case` es exhaustivo y la fecha viene de la mano del estado
que la tiene.
-}
type Estado
    = Pendiente
    | Hecha Posix


estado : Transferencia -> Estado
estado transferencia =
    case transferencia.saldadaAt of
        Just saldadaAt ->
            Hecha saldadaAt

        Nothing ->
            Pendiente


estaHecha : Transferencia -> Bool
estaHecha transferencia =
    transferencia.saldadaAt /= Nothing


{-| Las transferencias de un grupo congelado vienen de dos lados: el plan que
propuso la app al congelar, y las que alguien registró a mano porque la plata ya
se había movido. El backend las guarda en la misma tabla y no las marca, pero se
distinguen igual: las del plan nacen pendientes, así que una `saldadaAt` anterior
al congelamiento sólo puede ser de una registrada a mano de antes.

El caso que esto NO distingue es una registrada a mano _después_ de congelar:
queda contada como propuesta, porque tiene la misma forma que una del plan que
alguien marcó. Hoy no se puede crear una así —el botón de registrar a mano sólo
aparece con el grupo abierto—, pero si eso cambia hace falta que el backend diga
de dónde viene cada una.

-}
esPropuesta : { r | congeladoAt : Maybe Posix } -> Transferencia -> Bool
esPropuesta grupo transferencia =
    case ( grupo.congeladoAt, transferencia.saldadaAt ) of
        ( Just congeladoAt, Just saldadaAt ) ->
            Time.posixToMillis saldadaAt >= Time.posixToMillis congeladoAt

        _ ->
            True


{-| Cómo viene el plan: cuántas de las que propuso la app ya se marcaron, sobre
cuántas propuso. Las registradas a mano quedan afuera de los dos números —no son
algo que el grupo tenga pendiente hacer, y contarlas hacía que el progreso
arrancara adelantado.
-}
progresoDelPlan :
    { r | congeladoAt : Maybe Posix }
    -> List Transferencia
    -> { completadas : Int, total : Int }
progresoDelPlan grupo transferencias =
    let
        plan =
            transferencias |> List.filter (esPropuesta grupo)
    in
    { completadas = plan |> List.filter estaHecha |> List.length
    , total = List.length plan
    }


frase : Grupo -> Transferencia -> List (Html msg)
frase grupo t =
    [ participante grupo t.from
    , text " transfiere "
    , monto grupo.monedaPorDefecto t
    , text " a "
    , participante grupo t.to
    ]


conFlechas : Grupo -> Transferencia -> List (Html msg)
conFlechas grupo t =
    [ participante grupo t.from
    , flecha
    , monto grupo.monedaPorDefecto t
    , flecha
    , participante grupo t.to
    ]


flecha : Html msg
flecha =
    span
        [ class "mx-2 text-body-secondary"
        , Html.Attributes.attribute "aria-hidden" "true"
        ]
        [ text "→" ]


participante : Grupo -> ParticipanteId -> Html msg
participante grupo participanteId =
    span [ class "fw-semibold" ]
        [ text <| lookupNombreParticipante grupo participanteId ]


monto : Moneda -> Transferencia -> Html msg
monto monedaPorDefecto t =
    span [ class "fw-semibold text-nowrap" ]
        [ text <| Moneda.simbolo monedaPorDefecto t.moneda
        , text " "
        , text <| Monto.toString t.monto
        ]
