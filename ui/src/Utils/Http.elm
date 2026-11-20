module Utils.Http exposing (viewHttpError)

import Html exposing (Html, details, pre, summary, text)
import Html.Attributes exposing (class, style)
import Http


viewHttpError : Http.Error -> Html a
viewHttpError e =
    details [ class "small text-body-secondary mt-2" ]
        [ summary [] [ text "Ver detalles" ]
        , pre [ class "text-start mb-0 mt-1", style "white-space" "pre-wrap" ] [ text (describir e) ]
        ]


describir : Http.Error -> String
describir e =
    case e of
        Http.BadUrl url ->
            "URL inválida: " ++ url

        Http.Timeout ->
            "El servidor tardó demasiado en responder."

        Http.NetworkError ->
            "No pudimos conectarnos con el servidor. Revisá tu conexión."

        Http.BadStatus status ->
            "El servidor respondió con status " ++ String.fromInt status ++ "."

        Http.BadBody mensaje ->
            "La respuesta del servidor no tiene el formato esperado:\n\n" ++ mensaje
