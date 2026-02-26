module Utils.Http exposing (viewHttpError)

import Components.Ui5 as Ui5
import Html exposing (Html)
import Http


viewHttpError : Http.Error -> Html a
viewHttpError e =
    case e of
        Http.BadUrl string ->
            Ui5.text <| "Bad url: '" ++ string ++ "'."

        Http.Timeout ->
            Ui5.text "Server timed out."

        Http.NetworkError ->
            Ui5.text "Network error."

        Http.BadStatus status ->
            Ui5.text <| "Bad status: '" ++ String.fromInt status ++ "'."

        Http.BadBody body ->
            Ui5.text <| "Bad status: '" ++ body ++ "'."
