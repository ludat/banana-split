module Utils.Day exposing (Day, jsonDecDay, jsonEncDay, validateDay)

import Date exposing (Date)
import Form.Validate as V exposing (Validation)
import Json.Decode
import Json.Encode
import Utils.Form exposing (CustomFormError(..))


type alias Day =
    Date


validateDay : Validation CustomFormError Day
validateDay =
    V.string
        |> V.andThen
            (\s ->
                case Date.fromIsoString s of
                    Ok date ->
                        V.succeed date

                    Err _ ->
                        V.fail (V.customError InvalidDate)
            )


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
