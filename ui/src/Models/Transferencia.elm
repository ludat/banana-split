module Models.Transferencia exposing (Estado(..), estaHecha, estado, frase, monto, participante)

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


frase : Grupo -> Transferencia -> List (Html msg)
frase grupo t =
    [ participante grupo t.from
    , text " le transfiere "
    , monto grupo.monedaPorDefecto t
    , text " a "
    , participante grupo t.to
    ]


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
