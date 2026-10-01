module Components.Bootstrap exposing
    ( AlertVariant(..)
    , BtnVariant(..)
    , alert
    , badge
    , btn
    , card
    , cardBody
    , cardHeader
    , dateFormItem
    , fileInput
    , fondoCongelado
    , listGroup
    , listGroupItem
    , listGroupKeyed
    , modal
    , montoInput
    , navTab
    , navTabs
    , navbar
    , navbarBrand
    , requiredMarker
    , segmentedButton
    , segmentedButtonItem
    , selectInput
    , spinner
    , textFormItem
    , textInput
    , textoCongelado
    )

import Form exposing (Msg(..))
import Form.Field as FormField
import Html exposing (Attribute, Html, button, div, h5, li, nav, span, ul)
import Html.Attributes as Attr exposing (attribute, class, classList, for, id, placeholder, selected, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Html.Keyed as Keyed
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
    | SecondarySolid
    | Danger
    | Transparent
      -- El violeta de un grupo congelado y su versión invertida, para el botón
      -- blanco que va adentro del banner violeta. Los dos salen de `styles.css`.
    | Congelado
    | CongeladoInverso


btn : BtnVariant -> List (Attribute msg) -> List (Html msg) -> Html msg
btn variant attrs children =
    let
        -- Atributos y no un string de clases porque las variantes del grupo
        -- congelado no tienen clase: se arman con las variables de Bootstrap
        -- puestas inline.
        variantAttrs =
            case variant of
                Primary ->
                    [ class "btn-primary" ]

                Secondary ->
                    [ class "btn-outline-secondary" ]

                SecondarySolid ->
                    [ class "btn-secondary" ]

                Danger ->
                    [ class "btn-danger" ]

                Transparent ->
                    [ class "btn-outline-secondary border-0" ]

                Congelado ->
                    [ variablesDeBoton
                        { texto = violeta.contraste
                        , fondo = violeta.base
                        , hover = violeta.oscuro
                        , apretado = violeta.masOscuro
                        }
                    ]

                CongeladoInverso ->
                    [ variablesDeBoton
                        { texto = violeta.base
                        , fondo = violeta.contraste
                        , hover = violeta.claro
                        , apretado = violeta.masClaro
                        }
                    ]
    in
    button (type_ "button" :: class "btn" :: variantAttrs ++ attrs) children


{-| Las variables con las que Bootstrap 5.3 arma un botón, puestas inline en vez
de en una clase propia.

Va por `attribute "style"` y no por `Attr.style`: esta última termina en
`domNode.style[clave] = valor`, que para una custom property no hace nada —
hace falta `setProperty`—, mientras que el atributo pasa por el parser de CSS y
sí las toma.

OJO: no mezclar con `Attr.style` en el mismo botón. Elm aplica los estilos por
asignación y los atributos por `setAttribute`, así que uno pisa al otro.

-}
variablesDeBoton :
    { texto : String, fondo : String, hover : String, apretado : String }
    -> Attribute msg
variablesDeBoton { texto, fondo, hover, apretado } =
    Attr.attribute "style" <|
        String.join "; "
            [ "--bs-btn-color: " ++ texto
            , "--bs-btn-bg: " ++ fondo
            , "--bs-btn-border-color: " ++ fondo
            , "--bs-btn-hover-color: " ++ texto
            , "--bs-btn-hover-bg: " ++ hover
            , "--bs-btn-hover-border-color: " ++ hover
            , "--bs-btn-active-color: " ++ texto
            , "--bs-btn-active-bg: " ++ apretado
            , "--bs-btn-active-border-color: " ++ apretado
            , "--bs-btn-disabled-color: " ++ texto
            , "--bs-btn-disabled-bg: " ++ fondo
            , "--bs-btn-disabled-border-color: " ++ fondo
            ]


{-| El violeta de un grupo congelado, el que en pantalla se lee como "saldando
deudas". No sale de la paleta de Bootstrap —su `--bs-purple` es bastante más
apagado— así que vive acá, que es el único módulo que lo usa.

`oscuro` y `masOscuro` son el hover y el apretado del botón violeta; `claro` y
`masClaro`, los del invertido —el blanco que va adentro del banner—. Y
`contraste` es lo que se apoya sobre el violeta.

No hay variante para tema oscuro: donde se usa el violeta es siempre de fondo,
con texto blanco encima, así que el contraste lo trae el par y no depende del
tema.

-}
violeta : { base : String, oscuro : String, masOscuro : String, contraste : String, claro : String, masClaro : String }
violeta =
    { base = "#8e12fc"
    , oscuro = "#7a0fd9"
    , masOscuro = "#6a0cbd"
    , contraste = "#fff"
    , claro = "#f3e8ff"
    , masClaro = "#e9d5ff"
    }


{-| El violeta del grupo congelado como fondo y como color de texto, para lo que
no es un botón: el banner, el badge y el ícono del modal.
-}
fondoCongelado : Attribute msg
fondoCongelado =
    Attr.style "background-color" violeta.base


textoCongelado : Attribute msg
textoCongelado =
    Attr.style "color" violeta.base


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
    , centered : Bool
    }
    -> Html msg
modal { isOpen, onClose, title, body, footer, centered } =
    if isOpen then
        div []
            [ div
                [ class "modal d-block"
                , attribute "tabindex" "-1"
                , attribute "aria-modal" "true"
                , attribute "role" "dialog"
                ]
                [ div [ classList [ ( "modal-dialog", True ), ( "modal-dialog-centered", centered ) ] ]
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


{-| Igual que `listGroup` pero con cada item asociado a una clave estable, para
que al re-renderizar la lista el diff case los nodos por clave en vez de por
posición. Vale la pena en listas largas que se reordenan o se vuelven a pedir.
-}
listGroupKeyed : List (Attribute msg) -> List ( String, Html msg ) -> Html msg
listGroupKeyed attrs children =
    Keyed.ul (class "list-group" :: attrs) children


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


montoInput : Int -> Form.FieldState CustomFormError String -> List (Attribute Form.Msg) -> Html Form.Msg
montoInput decimales state attrs =
    Html.node "monto-input"
        ([ attribute "raw-value" (Maybe.withDefault "" state.value)
         , attribute "decimal-places" (String.fromInt decimales)
         , attribute "input-id" state.path
         , classList [ ( "is-invalid", hasErrorField state ) ]
         , on "autoNumeric:rawValueModified"
            -- Con emptyInputBehavior "null" el rawValue de un input vacío es null,
            -- y un decoder que sólo acepta String descarta el evento: el form se
            -- queda con el valor viejo mientras la pantalla muestra vacío.
            (Decode.at [ "detail", "newRawValue" ] (Decode.nullable Decode.string)
                |> Decode.map
                    (\v ->
                        Input state.path Form.Text (FormField.String (Maybe.withDefault "" v))
                    )
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
