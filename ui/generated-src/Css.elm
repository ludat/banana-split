module Css exposing (barras_precio, eje_vertical, toast_container)

import Html
import Html.Attributes


barras_precio : Html.Attribute msg
barras_precio =
    Html.Attributes.class "barras-precio"


eje_vertical : Html.Attribute msg
eje_vertical =
    Html.Attributes.class "eje-vertical"


toast_container : Html.Attribute msg
toast_container =
    Html.Attributes.class "toast-container"
