module Models.Monto exposing
    ( abs
    , add
    , asDeltaHtml
    , diffText
    , sub
    , toFloat
    , toRawString
    , toSignedString
    , toString
    , validateMonto
    , zero
    )

import Form.Error as FormError
import Form.Validate as V exposing (Validation)
import Generated.Api exposing (Monto)
import Html exposing (Html, span, text)
import Html.Attributes exposing (class)
import Numeric.Decimal as Decimal
import Numeric.Decimal.Rounding as Decimal
import Numeric.Nat as Nat
import Utils.Form exposing (CustomFormError(..))


{-| Form validation that accepts display-format input ("1.000,50") and produces a Monto.
-}
validateMonto : Validation CustomFormError Monto
validateMonto =
    V.string
        |> V.andThen
            (\t ->
                case Decimal.fromString Decimal.HalfUp (Nat.fromIntOrZero 2) t of
                    Ok n ->
                        let
                            numerator =
                                Decimal.toInt n

                            precision =
                                Decimal.getPrecision n
                        in
                        V.succeed (Monto (Nat.toInt precision) numerator)

                    Err e ->
                        V.fail <| FormError.value <| FormError.CustomError <| DecimalError e
            )


{-| Lift a Monto into the Decimal type for arithmetic.
-}
toDecimal : Monto -> Decimal.Decimal s Int
toDecimal monto =
    Decimal.succeed Decimal.RoundTowardsZero (Nat.fromIntAbs monto.lugaresDespuesDeLaComa) monto.valor


{-| Convert a Decimal result back to a Monto. Useful after arithmetic to get back to display.
-}
fromDecimal : Decimal.Decimal s Int -> Monto
fromDecimal d =
    Monto (Nat.toInt (Decimal.getPrecision d)) (Decimal.toInt d)


toFloat : Monto -> Float
toFloat monto =
    monto |> toDecimal |> Decimal.toFloat


zero : Monto
zero =
    Monto 0 0


{-| Add two Montos, scaling both to the larger precision so no decimal places are lost.
-}
add : Monto -> Monto -> Monto
add a b =
    let
        precision =
            max a.lugaresDespuesDeLaComa b.lugaresDespuesDeLaComa
    in
    { lugaresDespuesDeLaComa = precision
    , valor = scaleTo precision a + scaleTo precision b
    }


{-| Subtract `b` from `a`, scaling both to the larger precision so no decimal places are lost.
-}
sub : Monto -> Monto -> Monto
sub a b =
    let
        precision =
            max a.lugaresDespuesDeLaComa b.lugaresDespuesDeLaComa
    in
    { lugaresDespuesDeLaComa = precision
    , valor = scaleTo precision a - scaleTo precision b
    }


{-| Format string for programatic usage, as 1000.00. Don't use this for the ui
-}
toRawString : Monto -> String
toRawString monto =
    monto |> toDecimal |> Decimal.toString


{-| Display-formatted string, e.g. "1.000,50". Period = thousands separator, comma = decimal.
-}
toString : Monto -> String
toString { lugaresDespuesDeLaComa, valor } =
    let
        absValor =
            Basics.abs valor

        factor =
            10 ^ lugaresDespuesDeLaComa

        sign =
            if valor < 0 then
                "-"

            else
                ""

        intPart =
            groupThousands (String.fromInt (absValor // factor))

        decimalPart =
            if lugaresDespuesDeLaComa == 0 then
                ""

            else
                "," ++ String.padLeft lugaresDespuesDeLaComa '0' (String.fromInt (modBy factor absValor))
    in
    sign ++ intPart ++ decimalPart


{-| Like `toString`, but also prepends a `+` for positive values so the number
reads as a delta rather than a total (e.g. "+1.000,50"). Negatives keep their
`-` and zero stays unsigned.
-}
toSignedString : Monto -> String
toSignedString monto =
    if monto.valor > 0 then
        "+" ++ toString monto

    else
        toString monto


{-| Render a Monto as a signed delta (see `toSignedString`) coloured green when
positive and red when negative, via Bootstrap's `text-success` / `text-danger`.
Only the number is coloured; wrap it with an uncoloured currency symbol outside.
-}
asDeltaHtml : Monto -> Html msg
asDeltaHtml monto =
    span
        [ class
            (if monto.valor < 0 then
                "text-danger"

             else
                "text-success"
            )
        ]
        [ text (toSignedString monto) ]


abs : Monto -> Monto
abs monto =
    { lugaresDespuesDeLaComa = monto.lugaresDespuesDeLaComa, valor = Basics.abs monto.valor }


{-| Human-readable description of how `actual` differs from `expected`,
e.g. "es 50,00 de más".
-}
diffText : Monto -> Monto -> String
diffText actual expected =
    let
        diff =
            Decimal.subtract (toDecimal actual) (toDecimal expected)
    in
    if Decimal.toInt diff > 0 then
        "es " ++ toString (abs (fromDecimal diff)) ++ " de más"

    else
        "es " ++ toString (abs (fromDecimal diff)) ++ " de menos"



-- Internal


scaleTo : Int -> Monto -> Int
scaleTo precision { lugaresDespuesDeLaComa, valor } =
    valor * 10 ^ (precision - lugaresDespuesDeLaComa)


groupThousands : String -> String
groupThousands s =
    let
        len =
            String.length s

        firstLen =
            if modBy 3 len == 0 then
                3

            else
                modBy 3 len

        firstGroup =
            String.left firstLen s

        rest =
            String.dropLeft firstLen s
    in
    if String.isEmpty rest then
        firstGroup

    else
        firstGroup ++ "." ++ groupThousands rest
