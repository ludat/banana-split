module Models.Transaccion exposing (frase, monto, participante)

import Generated.Api exposing (Moneda, ParticipanteId, ShallowGrupo, Transaccion)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Models.Grupo exposing (lookupNombreParticipante)
import Models.Moneda as Moneda
import Models.Monto as Monto


{-| La transferencia dicha en una oración: "Lucas le transfiere AR$ 10.000 a
Juan". Se lee sola, sin tener que interpretar una flecha entre dos nombres, y
resalta lo único que hay que buscar de un vistazo: los dos nombres y el monto.
-}
frase : ShallowGrupo -> Moneda -> Transaccion -> List (Html msg)
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


monto : Moneda -> Moneda -> Transaccion -> Html msg
monto monedaPorDefecto moneda t =
    span [ class "fw-semibold text-nowrap" ]
        [ text <| Moneda.simbolo monedaPorDefecto moneda
        , text " "
        , text <| Monto.toString t.monto
        ]
