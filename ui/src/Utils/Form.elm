module Utils.Form exposing (..)

import Numeric.ArithmeticError as DecimalError


type CustomFormError
    = DecimalError DecimalError.ArithmeticError
    | StringError String
