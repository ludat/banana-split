module Components.Stepper exposing
    ( Paso
    , view
    )

{-| TEMPORAL: el stepper de Bootstrap 6, que en 5.3 no existe. Arma el mismo
markup con las mismas clases (`stepper`, `stepper-horizontal`, `stepper-item`,
`active`) y los estilos los pone `src/stepper.css`. Al pasar a Bootstrap 6 se
borra ese CSS y esto sigue funcionando; si para entonces no vale la pena tener
el módulo, se puede reemplazar por el markup directo.

Los pasos son botones (lo que Bootstrap 6 recomienda para navegar dentro de la
misma página), así que se puede saltar a cualquiera.

-}

import Html exposing (Attribute, Html, button, i, span, text)
import Html.Attributes exposing (attribute, class, classList, type_)


{-| Un paso del stepper.

  - `actual`: el paso en el que se está. Es el único `active` (en color
    primario): Bootstrap 6 lo usa también para los pasos completos, pero acá
    el color indica sólo dónde estás.
  - `conError`: muestra un ícono de error al lado del label.
  - `attrs`: lo del botón que decide el caller (el `onClick`, telemetría, ...).

-}
type alias Paso msg =
    { label : String
    , actual : Bool
    , conError : Bool
    , attrs : List (Attribute msg)
    }


view : List (Attribute msg) -> List (Paso msg) -> Html msg
view attrs pasos =
    Html.div (class "stepper stepper-horizontal w-100" :: attrs)
        (List.map viewPaso pasos)


viewPaso : Paso msg -> Html msg
viewPaso paso =
    button
        ([ type_ "button"
         , classList
            [ ( "stepper-item", True )
            , ( "active", paso.actual )
            ]
         ]
            ++ (if paso.actual then
                    [ attribute "aria-current" "step" ]

                else
                    []
               )
            ++ paso.attrs
        )
        [ span
            [ classList
                [ ( "small", True )
                , ( "fw-semibold", paso.actual )
                , ( "text-body-secondary", not paso.actual )
                ]
            ]
            (text paso.label
                :: (if paso.conError then
                        [ i [ class "bi bi-exclamation-circle-fill text-danger ms-1", attribute "aria-hidden" "true" ] []
                        , span [ class "visually-hidden" ] [ text "(con errores)" ]
                        ]

                    else
                        []
                   )
            )
        ]
