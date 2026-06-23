module Models.Moneda exposing (nombre, perEach, simbolo, simboloUnico, toString, todas, validate)

import Form.Error as FormError
import Form.Validate as V exposing (Validation)
import Generated.Api exposing (Moneda(..), PorMoneda, jsonDecMoneda, jsonEncMoneda)
import Json.Decode
import Json.Encode
import Utils.Form exposing (CustomFormError)


perEach : (Moneda -> a -> b) -> PorMoneda a -> List b
perEach f monedas =
    monedas |> List.map (\( moneda, a ) -> f moneda a)


toString : Moneda -> String
toString moneda =
    Json.Decode.decodeValue Json.Decode.string (jsonEncMoneda moneda)
        |> Result.withDefault ""


fromString : String -> Maybe Moneda
fromString s =
    Json.Decode.decodeValue jsonDecMoneda (Json.Encode.string s)
        |> Result.toMaybe


{-| Conseguir un nombre para el usuario de una moneda el particular
por ejemplo "Pesos Argentinos" ó "Dólares".
-}
nombre : Moneda -> String
nombre moneda =
    case moneda of
        ARS ->
            "Pesos Argentinos"

        USD ->
            "Dólares"

        EUR ->
            "Euros"

        BRL ->
            "Reales"

        UYU ->
            "Pesos Uruguayos"

        CLP ->
            "Pesos Chilenos"

        GBP ->
            "Libras"


todas : List Moneda
todas =
    [ ARS, USD, EUR, BRL, UYU, CLP, GBP ]


{-| Conseguir un simbolo de una moneda el particular. Si la moneda es la default del grupo
usamos un simbolo más corto para esa moneda.
por ejemplo "$" como simbolo corto ó "AR$" como simbolo largo.
-}
simbolo : Moneda -> Moneda -> String
simbolo monedaPorDefecto moneda =
    if moneda == monedaPorDefecto then
        simboloSimple moneda

    else
        simboloUnico moneda


simboloSimple : Moneda -> String
simboloSimple moneda =
    case moneda of
        ARS ->
            "$"

        USD ->
            "$"

        EUR ->
            "€"

        BRL ->
            "R$"

        UYU ->
            "$"

        CLP ->
            "$"

        GBP ->
            "£"


simboloUnico : Moneda -> String
simboloUnico moneda =
    case moneda of
        ARS ->
            "AR$"

        USD ->
            "U$D"

        EUR ->
            "€"

        BRL ->
            "R$"

        UYU ->
            "$U"

        CLP ->
            "CL$"

        GBP ->
            "£"


validate : Validation CustomFormError Moneda
validate =
    V.customValidation V.string
        (\s ->
            case fromString s of
                Just m ->
                    Ok m

                Nothing ->
                    Err <| FormError.value FormError.InvalidString
        )
