module Components.Ui5 exposing (bar, busyIndicator, button, dialog, fileUploader, form, formCheckbox, formSelect, label, li, link, list, messageStrip, option, page, responsivePopover, segmentedButton, segmentedButtonItem, select, slot, table, tableCell, tableHeaderCell, tableHeaderRow, tableRow, tableRowAction, textFormItem, textInput, wizard, wizardStep)

import Form exposing (Msg(..))
import Form.Field
import Html exposing (Attribute, Html, div, text)
import Html.Attributes as Attr exposing (checked, class, for, id, placeholder, required, selected, value)
import Html.Events exposing (on, onInput, onSubmit)
import Json.Decode
import Utils.Form exposing (CustomFormError, errorForField, hasErrorField)


textInput : Form.FieldState CustomFormError String -> List (Attribute Form.Msg) -> Html Form.Msg
textInput state attrs =
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


textFormItem : Form.FieldState CustomFormError String -> { placeholder : Maybe String, required : Bool, label : String } -> Html Form.Msg
textFormItem field options =
    formItem []
        [ label
            [ slot "labelContent"
            , for field.path
            , required options.required
            , Attr.attribute "show-colon" ""
            ]
            [ text options.label ]
        , textInput
            field
            [ case options.placeholder of
                Just p ->
                    placeholder p

                Nothing ->
                    class ""
            , required options.required
            ]
        ]


formSelect : List ( String, String ) -> Form.FieldState CustomFormError String -> List (Attribute Form.Msg) -> Html Form.Msg
formSelect options state attrs =
    select
        ([ on "change"
            (Json.Decode.at [ "detail", "selectedOption", "dataset", "id" ] Json.Decode.string
                |> Json.Decode.map (\v -> Input state.path Form.Select (Form.Field.String v))
            )
         , on "focusin" (Json.Decode.succeed (Focus state.path))
         , on "focusout" (Json.Decode.succeed (Blur state.path))
         , id state.path
         ]
            ++ attrs
        )
        (options
            |> List.map
                (\( val, labelText ) ->
                    option
                        [ Attr.attribute "data-id" val
                        , selected (state.value == Just val)
                        ]
                        [ text labelText ]
                )
        )


formCheckbox : Form.FieldState CustomFormError Bool -> List (Attribute Form.Msg) -> Html Form.Msg
formCheckbox state attrs =
    checkbox
        ([ checked (state.value == Just True)
         , on "change"
            (Json.Decode.at [ "target", "checked" ] Json.Decode.bool
                |> Json.Decode.map (\v -> Input state.path Form.Checkbox (Form.Field.Bool v))
            )
         , on "focusin" (Json.Decode.succeed (Focus state.path))
         , on "focusout" (Json.Decode.succeed (Blur state.path))
         ]
            ++ attrs
        )
        []



-- Simple wrappers


option : List (Attribute m) -> List (Html m) -> Html m
option attrs children =
    Html.node "ui5-option" attrs children


page : List (Attribute m) -> List (Html m) -> Html m
page attrs children =
    Html.node "ui5-page" attrs children


bar : List (Attribute m) -> List (Html m) -> Html m
bar attrs children =
    Html.node "ui5-bar" attrs children


messageStrip : List (Attribute m) -> List (Html m) -> Html m
messageStrip attrs children =
    Html.node "ui5-message-strip" attrs children


list : List (Attribute m) -> List (Html m) -> Html m
list attrs children =
    Html.node "ui5-list" attrs children


li : List (Attribute m) -> List (Html m) -> Html m
li attrs children =
    Html.node "ui5-li" attrs children


dialog : List (Attribute m) -> List (Html m) -> Html m
dialog attrs children =
    Html.node "ui5-dialog" attrs children


table : List (Attribute m) -> List (Html m) -> Html m
table attrs children =
    Html.node "ui5-table" attrs children


tableHeaderRow : List (Attribute m) -> List (Html m) -> Html m
tableHeaderRow attrs children =
    Html.node "ui5-table-header-row" attrs children


tableHeaderCell : List (Attribute m) -> List (Html m) -> Html m
tableHeaderCell attrs children =
    Html.node "ui5-table-header-cell" attrs children


tableRow : List (Attribute m) -> List (Html m) -> Html m
tableRow attrs children =
    Html.node "ui5-table-row" attrs children


tableCell : List (Attribute m) -> List (Html m) -> Html m
tableCell attrs children =
    Html.node "ui5-table-cell" attrs children


tableRowAction : List (Attribute m) -> List (Html m) -> Html m
tableRowAction attrs children =
    Html.node "ui5-table-row-action" attrs children


responsivePopover : List (Attribute m) -> List (Html m) -> Html m
responsivePopover attrs children =
    Html.node "ui5-responsive-popover" attrs children


wizard : List (Attribute m) -> List (Html m) -> Html m
wizard attrs children =
    Html.node "ui5-wizard" attrs children


wizardStep : List (Attribute m) -> List (Html m) -> Html m
wizardStep attrs children =
    Html.node "ui5-wizard-step" attrs children


segmentedButton : List (Attribute m) -> List (Html m) -> Html m
segmentedButton attrs children =
    Html.node "ui5-segmented-button" attrs children


segmentedButtonItem : List (Attribute m) -> List (Html m) -> Html m
segmentedButtonItem attrs children =
    Html.node "ui5-segmented-button-item" attrs children


fileUploader : List (Attribute m) -> List (Html m) -> Html m
fileUploader attrs children =
    Html.node "ui5-file-uploader" attrs children


busyIndicator : List (Attribute m) -> List (Html m) -> Html m
busyIndicator attrs children =
    Html.node "ui5-busy-indicator" attrs children


link : List (Attribute m) -> List (Html m) -> Html m
link attrs children =
    Html.node "ui5-link" attrs children


select : List (Attribute m) -> List (Html m) -> Html m
select attrs children =
    Html.node "ui5-select" attrs children


checkbox : List (Attribute m) -> List (Html m) -> Html m
checkbox attrs children =
    Html.node "ui5-checkbox" attrs children


formItem : List (Attribute m) -> List (Html m) -> Html m
formItem attrs children =
    Html.node "ui5-form-item" attrs children


label : List (Attribute m) -> List (Html m) -> Html m
label attrs children =
    Html.node "ui5-label" attrs children


form : (Form.Msg -> m) -> List (Attribute m) -> List (Html m) -> Html m
form f attrs children =
    Html.form [ onSubmit <| f Form.Submit ]
        [ Html.node "ui5-form" attrs children
        ]


button : List (Attribute m) -> List (Html m) -> Html m
button attrs children =
    Html.node "ui5-button" attrs children
