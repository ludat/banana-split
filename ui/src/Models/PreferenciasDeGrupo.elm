module Models.PreferenciasDeGrupo exposing (CambioDePreferencia(..), PreferenciasDeGrupo, aplicar, decoder, encodeCambio, vacias)

{-| Lo que cada persona configura para un grupo en este navegador. Cada campo
es una clave aparte de localStorage con el valor como string (ver
`interop.js`), y llegan todos en los flags.

Lo guardado puede venir de otra versión de la app, así que cada campo se lee
por su cuenta: uno que falta o no se entiende queda como en `vacias`, sin
afectar a los demás.

-}

import Dict exposing (Dict)
import Generated.Api exposing (ULID)
import Json.Decode
import Json.Encode


type alias PreferenciasDeGrupo =
    { participanteId : Maybe ULID
    }


vacias : PreferenciasDeGrupo
vacias =
    { participanteId = Nothing
    }


{-| `{ grupoId: { campo: valor } }`.
-}
decoder : Json.Decode.Decoder (Dict ULID PreferenciasDeGrupo)
decoder =
    Json.Decode.value
        |> Json.Decode.map
            (\todas ->
                objeto todas
                    |> Dict.map (\_ campos -> desdeCampos (objeto campos))
            )


{-| Lo que no es un objeto cuenta como uno vacío.
-}
objeto : Json.Decode.Value -> Dict String Json.Decode.Value
objeto =
    Json.Decode.decodeValue (Json.Decode.dict Json.Decode.value)
        >> Result.withDefault Dict.empty


desdeCampos : Dict String Json.Decode.Value -> PreferenciasDeGrupo
desdeCampos campos =
    { participanteId = campo "participanteId" Json.Decode.string campos
    }


campo : String -> Json.Decode.Decoder a -> Dict String Json.Decode.Value -> Maybe a
campo nombre decoderDelValor campos =
    Dict.get nombre campos
        |> Maybe.andThen (Json.Decode.decodeValue decoderDelValor >> Result.toMaybe)


{-| Se cambia de a un campo, así lo que se guarda es sólo esa clave y no se
pisan las demás con una copia vieja (por ejemplo, desde otra pestaña).
-}
type CambioDePreferencia
    = ParticipanteId (Maybe ULID)


aplicar : CambioDePreferencia -> PreferenciasDeGrupo -> PreferenciasDeGrupo
aplicar cambio preferencias =
    case cambio of
        ParticipanteId participanteId ->
            { preferencias | participanteId = participanteId }


{-| `null` borra la clave.
-}
encodeCambio : CambioDePreferencia -> { campo : String, valor : Json.Encode.Value }
encodeCambio cambio =
    case cambio of
        ParticipanteId participanteId ->
            { campo = "participanteId"
            , valor =
                participanteId
                    |> Maybe.map Json.Encode.string
                    |> Maybe.withDefault Json.Encode.null
            }
