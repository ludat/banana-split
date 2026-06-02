module Components.MonedaSelector exposing (MonedaSeleccionada(..), resolve, view)

import Generated.Api exposing (Moneda, jsonDecMoneda)
import Html exposing (Html, option, select, text)
import Html.Attributes exposing (class, selected, value)
import Html.Events exposing (on)
import Json.Decode
import Models.Moneda as Moneda


type MonedaSeleccionada
    = MonedaDefaultDelGrupo
    | MonedaSeleccionadaPorUsuario Moneda


resolve : MonedaSeleccionada -> Moneda -> Moneda
resolve sel default =
    case sel of
        MonedaDefaultDelGrupo ->
            default

        MonedaSeleccionadaPorUsuario moneda ->
            moneda


view : List Moneda -> Moneda -> (Moneda -> msg) -> Html msg
view monedas monedaSeleccionada onSelect =
    select
        [ class "form-select form-select-sm mb-3"
        , on "change"
            (Json.Decode.at [ "target", "value" ] jsonDecMoneda
                |> Json.Decode.map onSelect
            )
        ]
        (monedas
            |> List.map
                (\m ->
                    option
                        [ value (Moneda.toString m)
                        , selected (monedaSeleccionada == m)
                        ]
                        [ text (Moneda.nombre m) ]
                )
        )
