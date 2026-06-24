module Components.GraficoTorta exposing
    ( PorcionTorta
    , colorParaParticipante
    , porciones
    , viewDot
    , viewTortaGrande
    , viewTortaMini
    , viewTortaTrigger
    )

{-| Gráfico de torta (pie chart) reutilizable, construido con `elm-visualization`
(`Shape.pie` + `Shape.arc`). Se usa para mostrar cómo se reparte un gasto entre
los participantes.

El color de cada participante se deriva de su posición en la lista del grupo,
así la torta y los puntitos de la leyenda usan el mismo color de manera estable.

-}

import Color exposing (Color)
import Generated.Api exposing (Monto, Participante, ParticipanteId, ResumenNetos)
import Html exposing (Html)
import Html.Attributes exposing (attribute, class, id, style, type_)
import Models.Grupo exposing (GrupoLike, lookupNombreParticipante)
import Models.Monto as Monto
import Path
import Shape exposing (defaultPieConfig)
import Svg
import Svg.Attributes as SvgAttr


{-| Paleta categórica usada para asignar un color a cada participante. Tiene
suficientes colores para los grupos habituales; si se supera, los colores se
repiten cíclicamente.
-}
paleta : List Color
paleta =
    [ Color.rgb255 255 158 128
    , Color.rgb255 77 208 177
    , Color.rgb255 168 230 144
    , Color.rgb255 255 213 79
    , Color.rgb255 144 202 249
    , Color.rgb255 206 147 216
    , Color.rgb255 255 138 173
    , Color.rgb255 128 203 196
    , Color.rgb255 255 183 77
    , Color.rgb255 174 213 129
    ]


{-| Color de la porción "no distribuida": el hueco entre la suma de aportes y el
total del pago. Gris para que se lea como "todavía sin asignar".
-}
colorNoDistribuido : Color
colorNoDistribuido =
    Color.rgb255 222 226 230


colorParaIndice : Int -> Color
colorParaIndice i =
    let
        n =
            List.length paleta
    in
    paleta
        |> List.drop (modBy (max 1 n) i)
        |> List.head
        |> Maybe.withDefault (Color.rgb255 128 128 128)


{-| Color estable de un participante, según su orden dentro del grupo.
-}
colorParaParticipante : List Participante -> ParticipanteId -> Color
colorParaParticipante participantes participanteId =
    participantes
        |> List.indexedMap (\i p -> ( p.id, i ))
        |> List.filter (\( id, _ ) -> id == participanteId)
        |> List.head
        |> Maybe.map (\( _, i ) -> colorParaIndice i)
        |> Maybe.withDefault (colorParaIndice 0)


{-| Puntito de color para usar como leyenda al lado del nombre del participante.
-}
viewDot : Color -> Html msg
viewDot color =
    Html.span
        [ style "display" "inline-block"
        , style "width" "0.625rem"
        , style "height" "0.625rem"
        , style "border-radius" "50%"
        , style "background-color" (Color.toCssString color)
        , style "flex-shrink" "0"
        ]
        []


{-| Dibuja una torta de `size` píxeles a partir de una lista de `(color, valor)`.
Los valores se usan como pesos de cada porción (se ignoran los ≤ 0). El orden de
entrada se respeta para que los colores coincidan con la leyenda.
-}
viewTorta : Float -> List ( Color, Float ) -> Html msg
viewTorta size slices =
    let
        positivos =
            slices |> List.filter (\( _, valor ) -> valor > 0)

        radius =
            size / 2

        arcs =
            positivos
                |> List.map Tuple.second
                |> Shape.pie
                    { defaultPieConfig
                        | outerRadius = radius
                        , innerRadius = 0
                        , padAngle = 0

                        -- Mantenemos el orden de entrada para que cada porción
                        -- conserve el color de su participante.
                        , sortingFn = \_ _ -> EQ
                    }

        makeSlice ( color, _ ) arc =
            Path.element (Shape.arc arc)
                [ SvgAttr.fill (Color.toCssString color)
                , SvgAttr.stroke "var(--bs-body-bg)"
                , SvgAttr.strokeWidth "1"
                ]
    in
    Svg.svg
        [ SvgAttr.viewBox ("0 0 " ++ String.fromFloat size ++ " " ++ String.fromFloat size)
        , SvgAttr.width (String.fromFloat size)
        , SvgAttr.height (String.fromFloat size)
        ]
        [ Svg.g
            [ SvgAttr.transform ("translate(" ++ String.fromFloat radius ++ "," ++ String.fromFloat radius ++ ")") ]
            (List.map2 makeSlice positivos arcs)
        ]


{-| Una porción de la torta del reparto, con lo necesario para dibujarla y para
armar la leyenda.
-}
type alias PorcionTorta =
    { color : Color
    , nombre : String
    , monto : Monto
    }


{-| Arma las porciones de la torta a partir de los netos de un resumen de
sección (pagadores o deudores). El total de la torta es siempre el monto del
pago: lo que aún no se repartió aparece como una porción gris ("No distribuido"),
así el "100%" es el total. Si no hay un total conocido (`Nothing`) no se agrega
esa porción.
-}
porciones : GrupoLike g -> Maybe Monto -> ResumenNetos -> List PorcionTorta
porciones grupo totalPago resumenNetos =
    let
        aportes =
            resumenNetos.netos
                |> List.map
                    (\( participanteId, monto ) ->
                        { color = colorParaParticipante grupo.participantes participanteId
                        , nombre = lookupNombreParticipante grupo participanteId
                        , monto = Monto.abs monto
                        }
                    )

        sobrante =
            case totalPago of
                Just total ->
                    let
                        sumaAportes =
                            aportes |> List.map .monto |> List.foldl Monto.add Monto.zero
                    in
                    Monto.sub total sumaAportes

                Nothing ->
                    Monto.zero
    in
    aportes
        ++ (if Monto.toFloat sobrante > 0 then
                [ { color = colorNoDistribuido, nombre = "No distribuido", monto = sobrante } ]

            else
                []
           )


porcionesToSlices : List PorcionTorta -> List ( Color, Float )
porcionesToSlices porciones_ =
    porciones_ |> List.map (\p -> ( p.color, Monto.toFloat p.monto ))


{-| Ícono chico con la torta (una cajita de 3.5rem). No es interactivo por sí
mismo: el caller lo envuelve en el botón/disparador que prefiera (un modal de
Bootstrap, un `onClick`, etc.).
-}
viewTortaMini : List PorcionTorta -> Html msg
viewTortaMini porciones_ =
    Html.div
        [ class "border rounded d-flex align-items-center justify-content-center bg-body p-1"
        , style "width" "3.5rem"
        , style "height" "3.5rem"
        ]
        [ viewTorta 42 (porcionesToSlices porciones_) ]


{-| Torta en grande (240px) con su leyenda debajo. Pensada para el cuerpo de un
modal o panel desplegable.
-}
viewTortaGrande : List PorcionTorta -> Html msg
viewTortaGrande porciones_ =
    Html.div []
        [ Html.div [ class "d-flex justify-content-center mb-3" ]
            [ viewTorta 240 (porcionesToSlices porciones_) ]
        , Html.ul [ class "list-unstyled mb-0" ]
            (porciones_ |> List.map viewLegendItem)
        ]


{-| Ícono chico con la torta que, al tocarlo, abre un modal de Bootstrap con la
torta en grande y su leyenda. Incluye tanto el botón como el markup del modal.
`modalId` debe ser único en la página para no colisionar entre distintas tortas
(p. ej. pagadores vs deudores).

Sólo sirve cuando el disparador no vive dentro de otro modal: para mostrar la
torta desde un modal propio, usá `viewTortaMini` + `viewTortaGrande` con estado
propio (ver `Components.PagoDetalleModal`).

-}
viewTortaTrigger : String -> List PorcionTorta -> Html msg
viewTortaTrigger modalId porciones_ =
    Html.div []
        [ Html.button
            [ type_ "button"
            , class "btn p-0 border-0"
            , attribute "data-bs-toggle" "modal"
            , attribute "data-bs-target" ("#" ++ modalId)
            , attribute "aria-label" "Ver gráfico del reparto"
            ]
            [ viewTortaMini porciones_ ]
        , viewTortaModal modalId porciones_
        ]


viewTortaModal : String -> List PorcionTorta -> Html msg
viewTortaModal modalId porciones_ =
    Html.div
        [ class "modal fade"
        , id modalId
        , attribute "tabindex" "-1"
        , attribute "aria-hidden" "true"
        ]
        [ Html.div [ class "modal-dialog modal-dialog-centered modal-dialog-scrollable" ]
            [ Html.div [ class "modal-content" ]
                [ Html.div [ class "modal-header" ]
                    [ Html.h5 [ class "modal-title" ] [ Html.text "Reparto del gasto" ]
                    , Html.button
                        [ type_ "button"
                        , class "btn-close"
                        , attribute "data-bs-dismiss" "modal"
                        , attribute "aria-label" "Cerrar"
                        ]
                        []
                    ]
                , Html.div [ class "modal-body" ]
                    [ viewTortaGrande porciones_ ]
                ]
            ]
        ]


viewLegendItem : PorcionTorta -> Html msg
viewLegendItem porcion =
    Html.li [ class "d-flex align-items-center gap-2 py-1" ]
        [ viewDot porcion.color
        , Html.span [ class "flex-grow-1" ] [ Html.text porcion.nombre ]
        , Html.span [ class "fw-semibold" ] [ Html.text ("$ " ++ Monto.toString porcion.monto) ]
        ]
