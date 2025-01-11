module Css exposing (barras_precio, toast)

import Html
import Html.Attributes


barras_precio : Html.Attribute msg
barras_precio =
    Html.Attributes.class "barras-precio"


toast : Html.Attribute msg
toast =
    Html.Attributes.class "toast"
