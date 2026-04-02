module Models.Monto exposing
    ( abs
    , diffText
    , negate
    , toDecimal
    , toString
    , validateMonto
    , zero
    )

import Form.Error as FormError
import Form.Validate as V exposing (Validation)
import Generated.Api exposing (Monto)
import Numeric.Decimal as Decimal
import Numeric.Decimal.Rounding as Decimal
import Numeric.Nat as Nat
import Utils.Form exposing (CustomFormError(..))


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


toDecimal : Monto -> Decimal.Decimal s Int
toDecimal monto =
    Decimal.succeed Decimal.RoundTowardsZero (Nat.fromIntAbs monto.lugaresDespuesDeLaComa) monto.valor


zero : Monto
zero =
    Monto 0 0


negate : Monto -> Monto
negate monto =
    { lugaresDespuesDeLaComa = monto.lugaresDespuesDeLaComa, valor = monto.valor * -1 }


toString : Monto -> String
toString monto =
    Decimal.toString (toDecimal monto)


abs : Monto -> Monto
abs monto =
    { monto | valor = Basics.abs monto.valor }


diffText : Monto -> Monto -> String
diffText actual expected =
    let
        diff =
            Decimal.subtract (toDecimal actual) (toDecimal expected)

        absDiff =
            Decimal.abs diff
    in
    if Decimal.toInt diff > 0 then
        "es " ++ Decimal.toString absDiff ++ " de más"

    else
        "es " ++ Decimal.toString absDiff ++ " de menos"
