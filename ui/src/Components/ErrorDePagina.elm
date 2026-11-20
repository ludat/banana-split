module Components.ErrorDePagina exposing (Tipo(..), view)

import Html exposing (Html, div, h4, i, text)
import Html.Attributes exposing (class, style)


type Tipo
    = NoExiste
    | Fallo


view : { tipo : Tipo, titulo : String, detalle : List (Html msg) } -> Html msg
view { tipo, titulo, detalle } =
    div
        [ class "container py-5 text-center d-flex flex-column align-items-center justify-content-center"
        , style "min-height" "60vh"
        ]
        [ i
            [ class "display-4 mb-3"
            , class
                (case tipo of
                    NoExiste ->
                        "bi bi-search text-body-secondary"

                    Fallo ->
                        "bi bi-exclamation-triangle text-danger"
                )
            ]
            []
        , h4 [ class "fw-semibold mb-2" ] [ text titulo ]
        , div [ class "text-body-secondary", style "max-width" "32rem" ] detalle
        ]
