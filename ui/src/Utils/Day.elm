module Utils.Day exposing (Day, jsonDecDay, jsonEncDay, mesAbreviado, toString, validateDay)

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


mesAbreviado : Date -> String
mesAbreviado date =
    case Date.monthNumber date of
        1 ->
            "ENE"

        2 ->
            "FEB"

        3 ->
            "MAR"

        4 ->
            "ABR"

        5 ->
            "MAY"

        6 ->
            "JUN"

        7 ->
            "JUL"

        8 ->
            "AGO"

        9 ->
            "SEP"

        10 ->
            "OCT"

        11 ->
            "NOV"

        12 ->
            "DIC"

        _ ->
            ""


toString : Day -> String
toString fecha =
    let
        pad n =
            String.padLeft 2 '0' (String.fromInt n)
    in
    pad (Date.day fecha)
        ++ "-"
        ++ pad (Date.monthNumber fecha)
        ++ "-"
        ++ String.fromInt (Date.year fecha)
