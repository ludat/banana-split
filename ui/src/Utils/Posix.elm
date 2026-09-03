module Utils.Posix exposing (Posix, jsonDecPosix, jsonEncPosix, relativo, toString)

import DateFormat
import DateFormat.Language
import DateFormat.Relative
import Iso8601
import Json.Decode
import Json.Encode
import Time


type alias Posix =
    Time.Posix


jsonDecPosix : Json.Decode.Decoder Posix
jsonDecPosix =
    Iso8601.decoder


jsonEncPosix : Posix -> Json.Encode.Value
jsonEncPosix =
    Iso8601.encode


{-| Un instante en la zona del navegador: "3 sep 2026, 14:32". Acá para que el
formato y el idioma se decidan una vez y no en cada pantalla.
-}
toString : Time.Zone -> Posix -> String
toString =
    DateFormat.formatWithLanguage DateFormat.Language.spanish
        [ DateFormat.dayOfMonthNumber
        , DateFormat.text " "
        , DateFormat.monthNameAbbreviated
        , DateFormat.text " "
        , DateFormat.yearNumber
        , DateFormat.text ", "
        , DateFormat.hourMilitaryFixed
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        ]


{-| Hace cuánto pasó algo, contado desde `ahora`: "hace 2 días". Para una lista
de cosas que fueron pasando importa más el orden y la cercanía que la fecha
exacta, que igual queda a mano en el `title`.
-}
relativo : Posix -> Posix -> String
relativo ahora cuando =
    DateFormat.Relative.relativeTimeWithOptions opcionesEnEspanol ahora cuando


opcionesEnEspanol : DateFormat.Relative.RelativeTimeOptions
opcionesEnEspanol =
    let
        hace cantidad singular plural =
            if cantidad < 2 then
                "hace " ++ singular

            else
                "hace " ++ String.fromInt cantidad ++ " " ++ plural

        dentroDe cantidad singular plural =
            if cantidad < 2 then
                "en " ++ singular

            else
                "en " ++ String.fromInt cantidad ++ " " ++ plural
    in
    { rightNow = "recién"
    , someSecondsAgo =
        \segundos ->
            if segundos < 30 then
                "recién"

            else
                "hace " ++ String.fromInt segundos ++ " segundos"
    , someMinutesAgo = \n -> hace n "un minuto" "minutos"
    , someHoursAgo = \n -> hace n "una hora" "horas"
    , someDaysAgo =
        \dias ->
            if dias < 2 then
                "ayer"

            else
                "hace " ++ String.fromInt dias ++ " días"
    , someMonthsAgo = \n -> hace n "un mes" "meses"
    , someYearsAgo = \n -> hace n "un año" "años"
    , inSomeSeconds = \n -> dentroDe n "unos segundos" "segundos"
    , inSomeMinutes = \n -> dentroDe n "un minuto" "minutos"
    , inSomeHours = \n -> dentroDe n "una hora" "horas"
    , inSomeDays = \n -> dentroDe n "un día" "días"
    , inSomeMonths = \n -> dentroDe n "un mes" "meses"
    , inSomeYears = \n -> dentroDe n "un año" "años"
    }
