module Models.Transferencia exposing (Estado(..), frase, monto, participante)

import Generated.Api exposing (Moneda, ParticipanteId, ShallowGrupo, Transferencia)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Models.Grupo exposing (lookupNombreParticipante)
import Models.Moneda as Moneda
import Models.Monto as Monto
import Time exposing (Posix)


type Estado
    = Pendiente
    | Hecha Posix


frase : ShallowGrupo -> Moneda -> Transferencia -> List (Html msg)
frase grupo moneda t =
    [ participante grupo t.from
    , text " le transfiere "
    , monto grupo.monedaPorDefecto moneda t
    , text " a "
    , participante grupo t.to
    ]


participante : ShallowGrupo -> ParticipanteId -> Html msg
participante grupo participanteId =
    span [ class "fw-semibold" ]
        [ text <| lookupNombreParticipante grupo participanteId ]


monto : Moneda -> Moneda -> Transferencia -> Html msg
monto monedaPorDefecto moneda t =
    span [ class "fw-semibold text-nowrap" ]
        [ text <| Moneda.simbolo monedaPorDefecto moneda
        , text " "
        , text <| Monto.toString t.monto
        ]
