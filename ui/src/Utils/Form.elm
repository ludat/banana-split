module Utils.Form exposing (CustomFormError(..), anyString, errorForField, hasErrorField, isDataModifyingEvent, textInput)

import Form
import Form.Error as FormError
import Form.Field
import Form.Validate as Validate exposing (Validation)
import Html exposing (Html, input, text)
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (on, onInput)
import Json.Decode
import Numeric.ArithmeticError as DecimalError


type CustomFormError
    = DecimalError DecimalError.ArithmeticError
    | InvalidDate


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

                    FormError.GreaterFloatThan n ->
                        "Tiene que ser mayor que " ++ String.fromFloat n

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

                            InvalidDate ->
                                "Fecha inválida"
            )


isDataModifyingEvent : Form.Msg -> Bool
isDataModifyingEvent formMsg =
    case formMsg of
        Form.Input _ _ _ ->
            True

        Form.Append _ ->
            True

        Form.RemoveItem _ _ ->
            True

        Form.Reset _ ->
            True

        Form.NoOp ->
            False

        Form.Focus _ ->
            False

        Form.Blur _ ->
            False

        Form.Submit ->
            False

        Form.Validate ->
            False


errorForField : Form.FieldState CustomFormError String -> Html msg
errorForField field =
    errorTextForField field
        |> Maybe.map text
        |> Maybe.withDefault (text "")


{-| Cualquier texto, vacío incluido (`Form.Validate.string` rechaza el vacío).
Para los campos que valida el backend y no el form.
-}
anyString : Validation CustomFormError String
anyString =
    Validate.oneOf [ Validate.string, Validate.emptyString ]


{-| Un input de texto atado a un campo del form, con los atributos que hagan
falta (id, placeholder...).
-}
textInput : List (Html.Attribute Form.Msg) -> Form.FieldState CustomFormError String -> Html Form.Msg
textInput attributes field =
    input
        ([ type_ "text"
         , class "form-control"
         , value (Maybe.withDefault "" field.value)
         , onInput (\v -> Form.Input field.path Form.Text (Form.Field.String v))
         , on "focus" (Json.Decode.succeed (Form.Focus field.path))
         , on "blur" (Json.Decode.succeed (Form.Blur field.path))
         ]
            ++ attributes
        )
        []
