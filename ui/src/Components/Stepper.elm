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

  - `actual`: el paso en el que se está. Él y los anteriores van `active` (en
    color primario, con la línea entre ellos también) y los siguientes en gris,
    así el stepper muestra el avance. Se marca además con `aria-current` y el
    label en negrita.
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
    let
        -- Hasta el actual inclusive van como hechos. Si ninguno es el actual,
        -- ninguno.
        hechos =
            if List.any .actual pasos then
                pasos
                    |> List.foldl
                        (\paso ( acc, pasado ) ->
                            ( not pasado :: acc, pasado || paso.actual )
                        )
                        ( [], False )
                    |> Tuple.first
                    |> List.reverse

            else
                List.map (\_ -> False) pasos
    in
    Html.div (class "stepper stepper-horizontal w-100" :: attrs)
        (List.map2 viewPaso hechos pasos)


viewPaso : Bool -> Paso msg -> Html msg
viewPaso hecho paso =
    button
        ([ type_ "button"
         , classList
            [ ( "stepper-item", True )
            , ( "active", hecho )
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
