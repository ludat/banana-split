module Css exposing (barras_precio, toasts_container, toast)

import Html
import Html.Attributes


barras_precio : Html.Attribute msg
barras_precio =
    Html.Attributes.class "barras-precio"


toasts_container : Html.Attribute msg
toasts_container =
    Html.Attributes.class "toasts-container"


toast : Html.Attribute msg
toast =
    Html.Attributes.class "toast"
