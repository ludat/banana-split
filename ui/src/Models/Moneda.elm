module Models.Moneda exposing (fromString, perEach, simbolo, toString, todas, validate)

import Form.Error as FormError
import Form.Validate as V exposing (Validation)
import Generated.Api exposing (Moneda(..), PorMoneda)
import Utils.Form exposing (CustomFormError)


perEach : (Moneda -> a -> b) -> PorMoneda a -> List b
perEach f monedas =
    monedas |> List.map (\( moneda, a ) -> f moneda a)


toString : Moneda -> String
toString moneda =
    case moneda of
        ARS ->
            "ARS"

        USD ->
            "USD"

        EUR ->
            "EUR"

        BRL ->
            "BRL"

        UYU ->
            "UYU"

        CLP ->
            "CLP"

        GBP ->
            "GBP"


fromString : String -> Maybe Moneda
fromString s =
    case s of
        "ARS" ->
            Just ARS

        "USD" ->
            Just USD

        "EUR" ->
            Just EUR

        "BRL" ->
            Just BRL

        "UYU" ->
            Just UYU

        "CLP" ->
            Just CLP

        "GBP" ->
            Just GBP

        _ ->
            Nothing


todas : List Moneda
todas =
    [ ARS, USD, EUR, BRL, UYU, CLP, GBP ]


simbolo : List Moneda -> Moneda -> String
simbolo contexto moneda =
    let
        propio =
            simboloSimple moneda

        hayColision =
            contexto
                |> List.any (\m -> m /= moneda && simboloSimple m == propio)
    in
    if hayColision then
        simboloUnico moneda

    else
        propio


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
    V.customValidation (V.defaultValue (toString ARS) V.string)
        (\s ->
            case fromString s of
                Just m ->
                    Ok m

                Nothing ->
                    Err <| FormError.value FormError.InvalidString
        )
