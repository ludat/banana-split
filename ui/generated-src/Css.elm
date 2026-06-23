module Css exposing (barras_precio, eje_vertical, navbar_bottom, navbar_item, navbar_big_button, navbar_more, action_footer)

import Html
import Html.Attributes


barras_precio : Html.Attribute msg
barras_precio =
    Html.Attributes.class "barras-precio"


eje_vertical : Html.Attribute msg
eje_vertical =
    Html.Attributes.class "eje-vertical"


navbar_bottom : Html.Attribute msg
navbar_bottom =
    Html.Attributes.class "navbar-bottom"


navbar_item : Html.Attribute msg
navbar_item =
    Html.Attributes.class "navbar-item"


navbar_big_button : Html.Attribute msg
navbar_big_button =
    Html.Attributes.class "navbar-big-button"


navbar_more : Html.Attribute msg
navbar_more =
    Html.Attributes.class "navbar-more"


action_footer : Html.Attribute msg
action_footer =
    Html.Attributes.class "action-footer"
