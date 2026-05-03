module Utils.Date exposing (Day, jsonDecDay, jsonEncDay)

import Date exposing (Date)
import Json.Decode
import Json.Encode


type alias Day =
    Date


jsonDecDay : Json.Decode.Decoder Day
jsonDecDay =
    Json.Decode.string
        |> Json.Decode.andThen
            (\s ->
                case Date.fromIsoString s of
                    Ok date ->
                        Json.Decode.succeed date

                    Err e ->
                        Json.Decode.fail e
            )


jsonEncDay : Day -> Json.Encode.Value
jsonEncDay date =
    Json.Encode.string (Date.toIsoString date)
