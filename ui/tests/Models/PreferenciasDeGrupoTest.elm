module Models.PreferenciasDeGrupoTest exposing (suite)

import Dict
import Expect
import Json.Decode
import Json.Encode
import Models.PreferenciasDeGrupo as PreferenciasDeGrupo exposing (CambioDePreferencia(..))
import Test exposing (Test, describe, test)


decodificar : String -> Result Json.Decode.Error { preferencias : List ( String, Maybe String ), ilegibles : List String }
decodificar json =
    Json.Decode.decodeString PreferenciasDeGrupo.decoder json
        |> Result.map
            (\leidas ->
                { preferencias = leidas.preferencias |> Dict.toList |> List.map (Tuple.mapSecond .participanteId)
                , ilegibles = leidas.ilegibles
                }
            )


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
                    |> Expect.equal (Ok { preferencias = [ ( "a", Just "p1" ), ( "b", Nothing ) ], ilegibles = [] })
        , test "un campo que no se entiende queda en su valor por defecto y se reporta" <|
            \_ ->
                decodificar """{ "a": { "participanteId": 42 } }"""
                    |> Expect.equal (Ok { preferencias = [ ( "a", Nothing ) ], ilegibles = [ "participanteId" ] })
        , test "los campos desconocidos no molestan ni se reportan" <|
            \_ ->
                decodificar """{ "a": { "participanteId": "p1", "otraCosa": "x" } }"""
                    |> Expect.equal (Ok { preferencias = [ ( "a", Just "p1" ) ], ilegibles = [] })
        , test "un grupo que no es un objeto no afecta a los demás" <|
            \_ ->
                decodificar """{ "a": "basura", "b": { "participanteId": "p1" } }"""
                    |> Expect.equal (Ok { preferencias = [ ( "a", Nothing ), ( "b", Just "p1" ) ], ilegibles = [ "grupo" ] })
        , test "si no es un objeto no hay preferencias" <|
            \_ ->
                decodificar "null"
                    |> Expect.equal (Ok { preferencias = [], ilegibles = [ "preferencias" ] })
        ]
