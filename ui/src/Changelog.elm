module Changelog exposing (Entry, recentChangelog)

import Date exposing (Date)
import Time exposing (Month(..))


changelog : List Entry
changelog =
    [ { date = Date.fromCalendarDate 2026 Feb 28
      , title = "Popup de novedades"
      , description = "Ahora las novedades se muestran en un popup. Podés marcarlas como leídas para que no aparezcan de nuevo."
      }
    , { date = Date.fromCalendarDate 2026 Feb 28
      , title = "Nueva interfaz con UI5"
      , description = "Se migró la interfaz de Bulma CSS a UI5 Web Components para una experiencia más moderna y consistente."
      }
    ]


recentChangelog : Maybe Date -> Date -> List Entry
recentChangelog lastRead now =
    let
        oneMonthAgo =
            Date.add Date.Months -1 now
    in
    changelog
        |> List.filter (\entry -> Date.compare entry.date oneMonthAgo /= LT)
        |> List.filter
            (\entry ->
                case lastRead of
                    Nothing ->
                        True

                    Just readDate ->
                        Date.compare entry.date readDate == GT
            )


type alias Entry =
    { date : Date
    , description : String
    , title : String
    }
