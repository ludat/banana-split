module Components.Ui5 exposing (slot, ui5Button, ui5Form, ui5FormItem, ui5Label, ui5TextFormItem, ui5TextFormItemOptions, ui5TextInput)

import Form exposing (Msg(..))
import Form.Field
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Utils.Form exposing (CustomFormError, errorForField, hasErrorField)


ui5TextInput : Form.FieldState CustomFormError String -> List (Attribute Form.Msg) -> Html Form.Msg
ui5TextInput state attrs =
    Html.node "ui5-input"
        ([ case state.value of
            Just v ->
                value v

            Nothing ->
                class ""
         , onInput (\v -> Input state.path Form.Text (Form.Field.String v))
         , on "focusin" (Json.Decode.succeed (Focus state.path))
         , on "focusout" (Json.Decode.succeed (Blur state.path))
         , id state.path
         , Attr.attribute "value-state"
            (if hasErrorField state then
                "Negative"

             else
                "None"
            )
         ]
            ++ attrs
        )
        [ div [ slot "valueStateMessage" ] [ errorForField state ] ]


slot : String -> Attribute msg
slot name =
    Attr.attribute "slot" name


ui5FormItem : List (Attribute m) -> List (Html m) -> Html m
ui5FormItem attrs children =
    Html.node "ui5-form-item" attrs children


ui5Label : List (Attribute m) -> List (Html m) -> Html m
ui5Label attrs children =
    Html.node "ui5-label" attrs children


ui5Form : (Form.Msg -> m) -> List (Attribute m) -> List (Html m) -> Html m
ui5Form f attrs children =
    Html.form [ onSubmit <| f Form.Submit ]
        [ Html.node "ui5-form" attrs children
        ]


ui5TextFormItemOptions : { placeholder : Maybe String, required : Bool, label : String }
ui5TextFormItemOptions =
    { placeholder = Nothing, required = False, label = "" }


ui5Button : List (Attribute m) -> List (Html m) -> Html m
ui5Button attrs children =
    Html.node "ui5-label" attrs children


ui5TextFormItem : Form.FieldState CustomFormError String -> { placeholder : Maybe String, required : Bool, label : String } -> Html Form.Msg
ui5TextFormItem field options =
    ui5FormItem []
        [ ui5Label
            [ slot "labelContent"
            , for field.path
            , required options.required
            , Attr.attribute "show-colon" ""
            ]
            [ text options.label ]
        , ui5TextInput
            field
            [ case options.placeholder of
                Just p ->
                    placeholder p

                Nothing ->
                    class ""
            , required options.required
            ]
        ]
