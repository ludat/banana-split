module Models.PreferenciasDeGrupo exposing (CambioDePreferencia(..), Leidas, PreferenciasDeGrupo, aplicar, decoder, encodeCambio, vacias)

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


{-| `ilegibles` es qué no se pudo leer, para reportarlo: el nombre de cada campo
que no se entendió, `"grupo"` por cada grupo que no era un objeto y
`"preferencias"` si no lo era el total. Los campos desconocidos no cuentan:
pueden ser de una versión más nueva.
-}
type alias Leidas =
    { preferencias : Dict ULID PreferenciasDeGrupo
    , ilegibles : List String
    }


{-| `{ grupoId: { campo: valor } }`. Nunca falla.
-}
decoder : Json.Decode.Decoder Leidas
decoder =
    Json.Decode.map leer Json.Decode.value


leer : Json.Decode.Value -> Leidas
leer todas =
    case objeto todas of
        Just grupos ->
            let
                leidos =
                    Dict.map (\_ -> leerGrupo) grupos
            in
            { preferencias = Dict.map (\_ -> Tuple.first) leidos
            , ilegibles = leidos |> Dict.values |> List.concatMap Tuple.second
            }

        Nothing ->
            { preferencias = Dict.empty, ilegibles = [ "preferencias" ] }


leerGrupo : Json.Decode.Value -> ( PreferenciasDeGrupo, List String )
leerGrupo valor =
    case objeto valor of
        Just campos ->
            let
                ( participanteId, ilegiblesDeParticipanteId ) =
                    campo "participanteId" Json.Decode.string campos
            in
            ( { participanteId = participanteId }, ilegiblesDeParticipanteId )

        Nothing ->
            ( vacias, [ "grupo" ] )


objeto : Json.Decode.Value -> Maybe (Dict String Json.Decode.Value)
objeto =
    Json.Decode.decodeValue (Json.Decode.dict Json.Decode.value) >> Result.toMaybe


campo : String -> Json.Decode.Decoder a -> Dict String Json.Decode.Value -> ( Maybe a, List String )
campo nombre decoderDelValor campos =
    case Dict.get nombre campos of
        Just valor ->
            case Json.Decode.decodeValue decoderDelValor valor of
                Ok leido ->
                    ( Just leido, [] )

                Err _ ->
                    ( Nothing, [ nombre ] )

        Nothing ->
            ( Nothing, [] )


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
