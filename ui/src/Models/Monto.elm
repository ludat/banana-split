module Models.Monto exposing
    ( abs
    , toDecimal
    , validateMonto
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


abs : Monto -> Monto
abs monto =
    { monto | valor = Basics.abs monto.valor }
