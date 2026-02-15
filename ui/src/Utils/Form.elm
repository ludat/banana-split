module Utils.Form exposing (CustomFormError(..), errorForField, hasErrorField)

import Form
import Form.Error as FormError
import Html exposing (Html, text)
import Numeric.ArithmeticError as DecimalError


type CustomFormError
    = DecimalError DecimalError.ArithmeticError


hasErrorField : Form.FieldState CustomFormError String -> Bool
hasErrorField field =
    case field.liveError of
        Just _ ->
            True

        Nothing ->
            False


errorTextForField : Form.FieldState CustomFormError String -> Maybe String
errorTextForField field =
    field.liveError
        |> Maybe.map
            (\liveError ->
                case liveError of
                    FormError.Empty ->
                        "No puede ser vacio"

                    FormError.InvalidString ->
                        "String invalido"

                    FormError.InvalidEmail ->
                        "Email invalido"

                    FormError.InvalidFormat ->
                        "Formato invalido"

                    FormError.InvalidInt ->
                        "Entero invalido"

                    FormError.InvalidFloat ->
                        "Numero con coma invalido"

                    FormError.InvalidBool ->
                        "Booleano invalido"

                    FormError.SmallerIntThan _ ->
                        "Mas chico que"

                    FormError.GreaterIntThan _ ->
                        "Mas grande que"

                    FormError.SmallerFloatThan _ ->
                        "Mas chico que"

                    FormError.GreaterFloatThan _ ->
                        "Mas grande que"

                    FormError.ShorterStringThan _ ->
                        "Mas corto que"

                    FormError.LongerStringThan _ ->
                        "Longer than"

                    FormError.NotIncludedIn ->
                        "Not included in"

                    FormError.CustomError customError ->
                        case customError of
                            DecimalError arithmeticError ->
                                DecimalError.toString arithmeticError
            )


errorForField : Form.FieldState CustomFormError String -> Html msg
errorForField field =
    errorTextForField field
        |> Maybe.map text
        |> Maybe.withDefault (text "")
