module Components.Bootstrap exposing
    ( AlertVariant(..)
    , BtnVariant(..)
    , alert
    , badge
    , btn
    , card
    , cardBody
    , cardHeader
    , checkbox
    , dateFormItem
    , fileInput
    , listGroup
    , listGroupItem
    , modal
    , montoFormItem
    , montoInput
    , navTab
    , navTabs
    , navbar
    , navbarBrand
    , segmentedButton
    , segmentedButtonItem
    , selectFormItem
    , selectInput
    , spinner
    , textFormItem
    , textInput
    )

import Form exposing (Msg(..))
import Form.Field as FormField
import Html exposing (Attribute, Html, button, div, h5, li, nav, span, ul)
import Html.Attributes as Attr exposing (attribute, checked, class, classList, for, id, placeholder, selected, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Decode
import Utils.Form exposing (CustomFormError, errorForField, hasErrorField)


navbar : List (Attribute msg) -> List (Html msg) -> Html msg
navbar attrs children =
    nav (class "navbar bg-body-tertiary" :: attrs)
        [ div [ class "container-fluid" ] children ]


navbarBrand : List (Attribute msg) -> List (Html msg) -> Html msg
navbarBrand attrs children =
    Html.a (class "navbar-brand" :: attrs) children


type BtnVariant
    = Primary
    | Secondary
    | Danger
    | Transparent


btn : BtnVariant -> List (Attribute msg) -> List (Html msg) -> Html msg
btn variant attrs children =
    let
        variantClass =
            case variant of
                Primary ->
                    "btn-primary"

                Secondary ->
                    "btn-outline-secondary"

                Danger ->
                    "btn-danger"

                Transparent ->
                    "btn-outline-secondary border-0"
    in
    button ([ type_ "button", class ("btn " ++ variantClass) ] ++ attrs) children


spinner : List (Attribute msg) -> Html msg
spinner attrs =
    div
        (class "spinner-border text-primary"
            :: attribute "role" "status"
            :: attrs
        )
        [ span [ class "visually-hidden" ] [ Html.text "Cargando..." ] ]


card : List (Attribute msg) -> List (Html msg) -> Html msg
card attrs children =
    div (class "card" :: attrs) children


cardHeader : List (Attribute msg) -> List (Html msg) -> Html msg
cardHeader attrs children =
    div (class "card-header" :: attrs) children


cardBody : List (Attribute msg) -> List (Html msg) -> Html msg
cardBody attrs children =
    div (class "card-body" :: attrs) children


navTabs : List (Attribute msg) -> List (Html msg) -> Html msg
navTabs attrs children =
    ul (class "nav nav-underline" :: attrs) children


navTab : { active : Bool, attrs : List (Attribute msg) } -> List (Html msg) -> Html msg
navTab { active, attrs } children =
    li [ class "nav-item" ]
        [ Html.a
            ([ classList [ ( "nav-link", True ), ( "active", active ) ]
             , attribute "aria-current"
                (if active then
                    "page"

                 else
                    ""
                )
             ]
                ++ attrs
            )
            children
        ]


modal :
    { isOpen : Bool
    , onClose : msg
    , title : String
    , body : List (Html msg)
    , footer : List (Html msg)
    }
    -> Html msg
modal { isOpen, onClose, title, body, footer } =
    if isOpen then
        div []
            [ div
                [ class "modal d-block"
                , attribute "tabindex" "-1"
                , attribute "aria-modal" "true"
                , attribute "role" "dialog"
                ]
                [ div [ class "modal-dialog" ]
                    [ div [ class "modal-content" ]
                        [ div [ class "modal-header" ]
                            [ h5 [ class "modal-title" ] [ Html.text title ]
                            , button
                                [ type_ "button"
                                , class "btn-close"
                                , attribute "aria-label" "Cerrar"
                                , onClick onClose
                                ]
                                []
                            ]
                        , div [ class "modal-body" ] body
                        , div [ class "modal-footer" ] footer
                        ]
                    ]
                ]
            , div [ class "modal-backdrop show" ] []
            ]

    else
        Html.text ""


listGroup : List (Attribute msg) -> List (Html msg) -> Html msg
listGroup attrs children =
    ul (class "list-group" :: attrs) children


listGroupItem : List (Attribute msg) -> List (Html msg) -> Html msg
listGroupItem attrs children =
    li (class "list-group-item" :: attrs) children


type AlertVariant
    = AlertInfo
    | AlertSuccess
    | AlertWarning
    | AlertDanger


alert : AlertVariant -> List (Attribute msg) -> List (Html msg) -> Html msg
alert variant attrs children =
    let
        variantClass =
            case variant of
                AlertInfo ->
                    "alert-info"

                AlertSuccess ->
                    "alert-success"

                AlertWarning ->
                    "alert-warning"

                AlertDanger ->
                    "alert-danger"
    in
    div ([ class ("alert " ++ variantClass), attribute "role" "alert" ] ++ attrs) children


badge : String -> List (Attribute msg) -> List (Html msg) -> Html msg
badge colorClass attrs children =
    span (class ("badge " ++ colorClass) :: attrs) children



-- Form helpers


requiredMarker : Bool -> Html msg
requiredMarker isRequired =
    if isRequired then
        span [ class "text-danger ms-1" ] [ Html.text "*" ]

    else
        Html.text ""


controlClass : Form.FieldState CustomFormError String -> String -> String
controlClass state base =
    if hasErrorField state then
        base ++ " is-invalid"

    else
        base


feedback : Form.FieldState CustomFormError String -> Html Form.Msg
feedback state =
    if hasErrorField state then
        div [ class "invalid-feedback d-block" ] [ errorForField state ]

    else
        Html.text ""


fieldLabel : String -> String -> Bool -> Html Form.Msg
fieldLabel path labelText isRequired =
    Html.label [ class "form-label", for path ]
        [ Html.text labelText, requiredMarker isRequired ]


maybePlaceholder : Maybe String -> List (Attribute msg)
maybePlaceholder mp =
    case mp of
        Just p ->
            [ placeholder p ]

        Nothing ->
            []


textFormItem : Form.FieldState CustomFormError String -> { label : String, placeholder : Maybe String, required : Bool } -> Html Form.Msg
textFormItem state opts =
    div [ class "mb-3" ]
        [ fieldLabel state.path opts.label opts.required
        , Html.input
            ([ type_ "text"
             , class (controlClass state "form-control")
             , id state.path
             , value (Maybe.withDefault "" state.value)
             , onInput (\v -> Input state.path Form.Text (FormField.String v))
             , on "focusin" (Decode.succeed (Focus state.path))
             , on "focusout" (Decode.succeed (Blur state.path))
             , Attr.required opts.required
             ]
                ++ maybePlaceholder opts.placeholder
            )
            []
        , feedback state
        ]


montoFormItem : Form.FieldState CustomFormError String -> { label : String, placeholder : Maybe String, required : Bool } -> Html Form.Msg
montoFormItem state opts =
    div [ class "mb-3" ]
        [ fieldLabel state.path opts.label opts.required
        , Html.node "monto-input"
            ([ attribute "raw-value" (Maybe.withDefault "" state.value)
             , attribute "input-id" state.path
             , classList [ ( "is-invalid", hasErrorField state ) ]
             , on "autoNumeric:rawValueModified"
                (Decode.at [ "detail", "newRawValue" ] Decode.string
                    |> Decode.map (\v -> Input state.path Form.Text (FormField.String v))
                )
             , on "focusin" (Decode.succeed (Focus state.path))
             , on "focusout" (Decode.succeed (Blur state.path))
             , Attr.required opts.required
             ]
                ++ maybePlaceholder opts.placeholder
            )
            []
        , feedback state
        ]


selectFormItem : Form.FieldState CustomFormError String -> { label : String, required : Bool, options : List ( String, String ) } -> Html Form.Msg
selectFormItem state opts =
    div [ class "mb-3" ]
        [ fieldLabel state.path opts.label opts.required
        , Html.select
            [ class (controlClass state "form-select")
            , id state.path
            , on "change"
                (Decode.at [ "target", "value" ] Decode.string
                    |> Decode.map (\v -> Input state.path Form.Select (FormField.String v))
                )
            , on "focusin" (Decode.succeed (Focus state.path))
            , on "focusout" (Decode.succeed (Blur state.path))
            , Attr.required opts.required
            ]
            (opts.options
                |> List.map
                    (\( val, lbl ) ->
                        Html.option
                            [ value val, selected (state.value == Just val) ]
                            [ Html.text lbl ]
                    )
            )
        , feedback state
        ]


dateFormItem : Form.FieldState CustomFormError String -> { label : String, required : Bool } -> Html Form.Msg
dateFormItem state opts =
    div [ class "mb-3" ]
        [ fieldLabel state.path opts.label opts.required
        , Html.input
            [ type_ "date"
            , class (controlClass state "form-control")
            , id state.path
            , value (Maybe.withDefault "" state.value)
            , onInput (\v -> Input state.path Form.Text (FormField.String v))
            , on "focusin" (Decode.succeed (Focus state.path))
            , on "focusout" (Decode.succeed (Blur state.path))
            , Attr.required opts.required
            ]
            []
        , feedback state
        ]



-- Bare input controls (no label/feedback wrapper)


textInput : Form.FieldState CustomFormError String -> List (Attribute Form.Msg) -> Html Form.Msg
textInput state attrs =
    Html.input
        ([ type_ "text"
         , class (controlClass state "form-control")
         , id state.path
         , value (Maybe.withDefault "" state.value)
         , onInput (\v -> Input state.path Form.Text (FormField.String v))
         , on "focusin" (Decode.succeed (Focus state.path))
         , on "focusout" (Decode.succeed (Blur state.path))
         ]
            ++ attrs
        )
        []


montoInput : Form.FieldState CustomFormError String -> List (Attribute Form.Msg) -> Html Form.Msg
montoInput state attrs =
    Html.node "monto-input"
        ([ attribute "raw-value" (Maybe.withDefault "" state.value)
         , attribute "input-id" state.path
         , classList [ ( "is-invalid", hasErrorField state ) ]
         , on "autoNumeric:rawValueModified"
            (Decode.at [ "detail", "newRawValue" ] Decode.string
                |> Decode.map (\v -> Input state.path Form.Text (FormField.String v))
            )
         , on "focusin" (Decode.succeed (Focus state.path))
         , on "focusout" (Decode.succeed (Blur state.path))
         ]
            ++ attrs
        )
        []


selectInput : List ( String, String ) -> Form.FieldState CustomFormError String -> List (Attribute Form.Msg) -> Html Form.Msg
selectInput options state attrs =
    Html.select
        ([ class (controlClass state "form-select")
         , id state.path
         , on "change"
            (Decode.at [ "target", "value" ] Decode.string
                |> Decode.map (\v -> Input state.path Form.Select (FormField.String v))
            )
         , on "focusin" (Decode.succeed (Focus state.path))
         , on "focusout" (Decode.succeed (Blur state.path))
         ]
            ++ attrs
        )
        (options
            |> List.map
                (\( val, lbl ) ->
                    Html.option
                        [ value val, selected (state.value == Just val) ]
                        [ Html.text lbl ]
                )
        )


checkbox : Form.FieldState CustomFormError Bool -> { label : String } -> Html Form.Msg
checkbox state opts =
    div [ class "form-check" ]
        [ Html.input
            [ type_ "checkbox"
            , class "form-check-input"
            , id state.path
            , checked (state.value == Just True)
            , on "change"
                (Decode.at [ "target", "checked" ] Decode.bool
                    |> Decode.map (\v -> Input state.path Form.Checkbox (FormField.Bool v))
                )
            , on "focusin" (Decode.succeed (Focus state.path))
            , on "focusout" (Decode.succeed (Blur state.path))
            ]
            []
        , Html.label [ class "form-check-label", for state.path ] [ Html.text opts.label ]
        ]


fileInput : List (Attribute msg) -> Html msg
fileInput attrs =
    Html.input ([ type_ "file", class "form-control" ] ++ attrs) []



-- Segmented buttons (btn-group with active state)


segmentedButton : List (Attribute msg) -> List (Html msg) -> Html msg
segmentedButton attrs children =
    div ([ class "btn-group", attribute "role" "group" ] ++ attrs) children


segmentedButtonItem : { active : Bool, onSelect : msg } -> List (Attribute msg) -> List (Html msg) -> Html msg
segmentedButtonItem opts attrs children =
    button
        ([ type_ "button"
         , classList
            [ ( "btn", True )
            , ( "btn-primary", opts.active )
            , ( "btn-outline-primary", not opts.active )
            ]
         , onClick opts.onSelect
         ]
            ++ attrs
        )
        children
