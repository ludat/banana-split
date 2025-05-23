module Css exposing (barras_precio, arrow_container, is_size_6_5, toasts_container, toast)

import Html
import Html.Attributes


barras_precio : Html.Attribute msg
barras_precio =
    Html.Attributes.class "barras-precio"


arrow_container : Html.Attribute msg
arrow_container =
    Html.Attributes.class "arrow-container"


is_size_6_5 : Html.Attribute msg
is_size_6_5 =
    Html.Attributes.class "is-size-6-5"


toasts_container : Html.Attribute msg
toasts_container =
    Html.Attributes.class "toasts-container"


toast : Html.Attribute msg
toast =
    Html.Attributes.class "toast"
