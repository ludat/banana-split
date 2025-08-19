module Utils.Form exposing (CustomFormError(..), errorForField, hasErrorField)

import Form
import Form.Error as FormError
import Html exposing (..)
import Html.Attributes exposing (..)
import Numeric.ArithmeticError as DecimalError


type CustomFormError
    = DecimalError DecimalError.ArithmeticError
    | StringError String


hasErrorField : Form.FieldState CustomFormError String -> Bool
hasErrorField field =
    case field.liveError of
        Just _ ->
            True

        Nothing ->
            False


errorForField : Form.FieldState CustomFormError String -> Html a
errorForField field =
    case field.liveError of
        Just FormError.Empty ->
            p [ class "help is-danger" ] [ text "No puede ser vacio" ]

        Just FormError.InvalidString ->
            p [ class "help is-danger" ] [ text "String invalido" ]

        Just FormError.InvalidEmail ->
            p [ class "help is-danger" ] [ text "Email invalido" ]

        Just FormError.InvalidFormat ->
            p [ class "help is-danger" ] [ text "Formato invalido" ]

        Just FormError.InvalidInt ->
            p [ class "help is-danger" ] [ text "Entero invalido" ]

        Just FormError.InvalidFloat ->
            p [ class "help is-danger" ] [ text "Numero con coma invalido" ]

        Just FormError.InvalidBool ->
            p [ class "help is-danger" ] [ text "Booleano invalido" ]

        Just (FormError.SmallerIntThan _) ->
            p [ class "help is-danger" ] [ text "Mas chico que" ]

        Just (FormError.GreaterIntThan _) ->
            p [ class "help is-danger" ] [ text "Mas grande que" ]

        Just (FormError.SmallerFloatThan _) ->
            p [ class "help is-danger" ] [ text "Mas chico que" ]

        Just (FormError.GreaterFloatThan _) ->
            p [ class "help is-danger" ] [ text "Mas grande que" ]

        Just (FormError.ShorterStringThan _) ->
            p [ class "help is-danger" ] [ text "Mas corto que" ]

        Just (FormError.LongerStringThan _) ->
            p [ class "help is-danger" ] [ text "Longer than" ]

        Just FormError.NotIncludedIn ->
            p [ class "help is-danger" ] [ text "Not included in" ]

        Just (FormError.CustomError _) ->
            p [ class "help is-danger" ] [ text "Jajan't" ]

        Nothing ->
            text ""
