module Components.Bootstrap exposing
    ( AlertVariant(..)
    , BtnVariant(..)
    , alert
    , badge
    , btn
    , card
    , cardBody
    , cardHeader
    , dropdown
    , listGroup
    , listGroupItem
    , modal
    , navTab
    , navTabs
    , navbar
    , navbarBrand
    )

import Html exposing (Attribute, Html, button, div, h5, li, nav, span, ul)
import Html.Attributes exposing (attribute, class, classList, type_)
import Html.Events exposing (onClick)


navbar : List (Attribute msg) -> List (Html msg) -> Html msg
navbar attrs children =
    nav (class "navbar navbar-expand-lg bg-body-tertiary" :: attribute "data-bs-theme" "dark" :: attrs)
        [ div [ class "container-fluid" ] children ]


navbarBrand : List (Attribute msg) -> List (Html msg) -> Html msg
navbarBrand attrs children =
    Html.a (class "navbar-brand" :: attrs) children


type BtnVariant
    = Primary
    | Danger
    | Transparent


btn : BtnVariant -> List (Attribute msg) -> List (Html msg) -> Html msg
btn variant attrs children =
    let
        variantClass =
            case variant of
                Primary ->
                    "btn-primary"

                Danger ->
                    "btn-danger"

                Transparent ->
                    "btn-outline-secondary border-0"
    in
    button ([ type_ "button", class ("btn " ++ variantClass) ] ++ attrs) children


dropdown :
    { isOpen : Bool
    , onToggle : msg
    , label : List (Html msg)
    , items : List (Html msg)
    , attrs : List (Attribute msg)
    }
    -> Html msg
dropdown { isOpen, onToggle, label, items, attrs } =
    div
        (classList [ ( "dropdown", True ), ( "show", isOpen ) ] :: attrs)
        [ button
            [ type_ "button"
            , class "btn btn-outline-secondary dropdown-toggle"
            , attribute "aria-expanded"
                (if isOpen then
                    "true"

                 else
                    "false"
                )
            , onClick onToggle
            ]
            label
        , ul
            [ classList [ ( "dropdown-menu", True ), ( "show", isOpen ) ] ]
            items
        ]


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
