module Models.Opinion exposing (Opinion(..), icono, label, toString)

{-| La respuesta a las encuestas de "¿mejor, igual o peor?" (el rediseño de la
repartija, las novedades).
-}


type Opinion
    = Mejor
    | Igual
    | Peor


{-| Lo que va en el atributo `app.feedback.opinion` de los eventos.
-}
toString : Opinion -> String
toString opinion =
    case opinion of
        Mejor ->
            "mejor"

        Igual ->
            "igual"

        Peor ->
            "peor"


icono : Opinion -> String
icono opinion =
    case opinion of
        Mejor ->
            "bi-emoji-smile"

        Igual ->
            "bi-emoji-neutral"

        Peor ->
            "bi-emoji-frown"


label : Opinion -> String
label opinion =
    case opinion of
        Mejor ->
            "Mejor"

        Igual ->
            "Igual"

        Peor ->
            "Peor"
