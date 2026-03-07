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
    , { date = Date.fromCalendarDate 2026 Mar 3
      , title = "Instalá como un app"
      , description = "Ahora podés instalar un grupo en tu celu como si fuera un app nativa. Adios a perder el enlace de un grupo."
      }
    , { date = Date.fromCalendarDate 2026 Mar 7
      , title = "Congelar grupos"
      , description = "Ahora podés congelar un grupo para fijar las deudas. Una vez congelado, no se pueden agregar, editar ni eliminar pagos."
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
