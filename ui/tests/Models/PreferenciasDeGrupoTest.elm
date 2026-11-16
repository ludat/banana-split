module Models.PreferenciasDeGrupoTest exposing (suite)

import Dict
import Expect
import Json.Decode
import Json.Encode
import Models.PreferenciasDeGrupo as PreferenciasDeGrupo exposing (CambioDePreferencia(..))
import Test exposing (Test, describe, test)


decodificar : String -> Result Json.Decode.Error (List ( String, Maybe String ))
decodificar json =
    Json.Decode.decodeString PreferenciasDeGrupo.decoder json
        |> Result.map (Dict.toList >> List.map (Tuple.mapSecond .participanteId))


suite : Test
suite =
    describe "PreferenciasDeGrupo"
        [ decoder
        , cambios
        ]


cambios : Test
cambios =
    describe "CambioDePreferencia"
        [ test "aplicar cambia sólo ese campo" <|
            \_ ->
                PreferenciasDeGrupo.aplicar (ParticipanteId (Just "p1")) PreferenciasDeGrupo.vacias
                    |> Expect.equal { participanteId = Just "p1" }
        , test "se guarda en la clave del campo" <|
            \_ ->
                PreferenciasDeGrupo.encodeCambio (ParticipanteId (Just "p1"))
                    |> (\{ campo, valor } -> ( campo, Json.Encode.encode 0 valor ))
                    |> Expect.equal ( "participanteId", "\"p1\"" )
        , test "sacar el participante borra la clave" <|
            \_ ->
                PreferenciasDeGrupo.encodeCambio (ParticipanteId Nothing)
                    |> (\{ campo, valor } -> ( campo, Json.Encode.encode 0 valor ))
                    |> Expect.equal ( "participanteId", "null" )
        ]


decoder : Test
decoder =
    describe "decoder"
        [ test "lee el participante de cada grupo" <|
            \_ ->
                decodificar """{ "a": { "participanteId": "p1" }, "b": {} }"""
                    |> Expect.equal (Ok [ ( "a", Just "p1" ), ( "b", Nothing ) ])
        , test "un campo que no se entiende queda en su valor por defecto" <|
            \_ ->
                decodificar """{ "a": { "participanteId": 42 } }"""
                    |> Expect.equal (Ok [ ( "a", Nothing ) ])
        , test "los campos desconocidos no molestan" <|
            \_ ->
                decodificar """{ "a": { "participanteId": "p1", "otraCosa": "x" } }"""
                    |> Expect.equal (Ok [ ( "a", Just "p1" ) ])
        , test "un grupo que no es un objeto no afecta a los demás" <|
            \_ ->
                decodificar """{ "a": "basura", "b": { "participanteId": "p1" } }"""
                    |> Expect.equal (Ok [ ( "a", Nothing ), ( "b", Just "p1" ) ])
        , test "si no es un objeto no hay preferencias" <|
            \_ ->
                decodificar "null"
                    |> Expect.equal (Ok [])
        ]
