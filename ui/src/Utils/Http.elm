module Utils.Http exposing (viewHttpError)

import Html exposing (Html, text)
import Http


viewHttpError : Http.Error -> Html a
viewHttpError e =
    case e of
        Http.BadUrl string ->
            text <| "Bad url: '" ++ string ++ "'."

        Http.Timeout ->
            text <| "Server timed out."

        Http.NetworkError ->
            text <| "Network error."

        Http.BadStatus status ->
            text <| "Bad status: '" ++ String.fromInt status ++ "'."

        Http.BadBody body ->
            text <| "Bad status: '" ++ body ++ "'."
