module Models.Monto exposing
    ( monto2Decimal
    , validateMonto
    )

import Form.Error as FormError
import Form.Validate as V exposing (Validation)
import Generated.Api exposing (Monto)
import Numeric.Decimal as Decimal
import Numeric.Decimal.Rounding as Decimal
import Numeric.Nat as Nat
import Numeric.Rational as Rational
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
                                Decimal.toNumerator n

                            denominator =
                                Decimal.toDenominator n
                        in
                        V.succeed ( "ARS", numerator, denominator )

                    Err e ->
                        V.fail <| FormError.value <| FormError.CustomError <| DecimalError e
            )


monto2Decimal : Monto -> Decimal.Decimal s Int
monto2Decimal montoRaw =
    case montoRaw of
        ( _, numerador, denominador ) ->
            Decimal.fromRational Decimal.RoundTowardsZero Nat.nat2 (Rational.ratio numerador denominador)
                |> Result.withDefault (Decimal.fromInt Decimal.RoundTowardsZero Nat.nat2 0)
