module Utils.Telemetry exposing (Feedback, Severity, TelemetryEvent, changelogFeedback, click, codigoDeLoginRechazado, crearGrupo, feedbackGeneral, gastoEdicionAbandonada, repartijaAccion, repartijaAyudaAbierta, repartijaDetalleDeItemAbierto, repartijaFiltro, repartijaParticipantesExpandidos, repartijaRediseno, repartijaRedisenoComentario, seccionVista, severityToString, trackDecodeErrors)

import Html
import Html.Attributes
import Http
import Json.Encode as Json exposing (Value)
import Models.PagoForm exposing (Section(..))
import RemoteData exposing (RemoteData(..), WebData)


type alias TelemetryEvent =
    { name : String
    , severity : Severity
    , attributes : List ( String, Value )
    }


{-| Nombre estable para los clicks en este elemento: el span del click pasa a
llamarse `click <nombre>` y lo lleva en `app.click.target` (ver
`js/telemetry.js`). Vale también para lo que se toque adentro, como el ícono de
un botón.

Es `<pantalla>.<acción>`, siempre un literal: botones repetidos en una lista
comparten el nombre, nunca llevan un id ni nada de la persona.

-}
click : String -> Html.Attribute msg
click nombre =
    Html.Attributes.attribute "data-track" nombre


{-| Envuelve una parte de la pantalla para saber si alguien llegó a verla: la
primera vez que queda a la vista se manda `app.section_viewed` con el nombre en
`app.section` (ver `track-view` en `js/telemetry.js`). Una vez por cada vez que
se monta, o sea por visita a la pantalla.

El nombre sigue la misma regla que `click`: `<pantalla>.<sección>`, literal.

-}
seccionVista : String -> Html.Html msg -> Html.Html msg
seccionVista nombre contenido =
    Html.node "track-view"
        [ Html.Attributes.attribute "name" nombre
        , Html.Attributes.style "display" "block"
        ]
        [ contenido ]


{-| El intento de crear un grupo, desde la landing o desde la home. El resultado
es "ok", "error" (la API lo rechazó) o "invalido" (faltaban campos).
-}
crearGrupo : { origen : String, resultado : String } -> Maybe TelemetryEvent
crearGrupo { origen, resultado } =
    Just
        { name = "app.grupo.create"
        , severity =
            if resultado == "error" then
                Warn

            else
                Info
        , attributes =
            [ ( "app.grupo.origen", Json.string origen )
            , ( "outcome", Json.string resultado )
            ]
        }


trackDecodeErrors : String -> WebData a -> Maybe TelemetryEvent
trackDecodeErrors operation response =
    case response of
        Failure (Http.BadBody _) ->
            Just
                { name = "app.api.decode_error"
                , severity = Error
                , attributes =
                    [ ( "app.api.operation", Json.string operation ) ]
                }

        _ ->
            Nothing


codigoDeLoginRechazado : Http.Error -> Maybe TelemetryEvent
codigoDeLoginRechazado err =
    let
        outcome : String
        outcome =
            case err of
                Http.BadStatus 429 ->
                    "rate_limited"

                Http.BadStatus 401 ->
                    "rechazado"

                _ ->
                    "otro"
    in
    Just
        { name = "app.auth.code_rejected"
        , severity = Warn
        , attributes =
            [ ( "outcome", Json.string outcome ) ]
        }


gastoEdicionAbandonada : { esNuevo : Bool, seccion : Section } -> Maybe TelemetryEvent
gastoEdicionAbandonada { esNuevo, seccion } =
    Just
        { name = "app.gasto.edit_abandoned"
        , severity = Info
        , attributes =
            [ ( "mode"
              , Json.string <|
                    if esNuevo then
                        "nuevo"

                    else
                        "existente"
              )
            , ( "section"
              , Json.string <|
                    case seccion of
                        BasicPagoData ->
                            "basico"

                        PagadoresSection ->
                            "pagadores"

                        DeudoresSection ->
                            "deudores"
              )
            ]
        }


{-| De qué es un comentario escrito por la persona (`Effect.sendFeedback`): el
texto va en el body y esto en el nombre del evento y los atributos.
-}
type alias Feedback =
    { name : String
    , attributes : List ( String, Value )
    }


{-| El comentario libre del menú, que no es sobre nada en particular.
-}
feedbackGeneral : Feedback
feedbackGeneral =
    { name = "app.feedback"
    , attributes = []
    }


{-| Qué le pareció el rediseño de la repartija comparado con la pantalla
anterior. La opinión es una de "mejor", "igual" o "peor": sin texto libre, que
va aparte en `repartijaRedisenoComentario` y solo si la persona escribe uno.
-}
repartijaRediseno : String -> Maybe TelemetryEvent
repartijaRediseno opinion =
    Just
        { name = "app.repartija.redesign_feedback"
        , severity = Info
        , attributes =
            [ ( "app.feedback.opinion", Json.string opinion ) ]
        }


{-| El por qué de una opinión sobre el rediseño. Lleva la misma opinión como
atributo que `repartijaRediseno`, así los comentarios se pueden agrupar por
respuesta.
-}
repartijaRedisenoComentario : String -> Feedback
repartijaRedisenoComentario opinion =
    { name = "app.repartija.redesign_feedback.comment"
    , attributes =
        [ ( "app.feedback.opinion", Json.string opinion ) ]
    }


{-| Qué le parecieron las novedades del modal. Lleva los títulos de las que se
le mostraron, porque el modal muestra las del último mes y no son las mismas
para todos: así una opinión se puede atribuir a los cambios que vio.
-}
changelogFeedback : { opinion : String, novedades : List String } -> Maybe TelemetryEvent
changelogFeedback { opinion, novedades } =
    Just
        { name = "app.changelog.feedback"
        , severity = Info
        , attributes =
            [ ( "app.feedback.opinion", Json.string opinion )
            , ( "app.changelog.entries", Json.list Json.string novedades )
            , ( "app.changelog.entry_count", Json.int (List.length novedades) )
            ]
        }


{-| Que alguien abrió el detalle de un item de la repartija. Cerrarlo no se
cuenta: lo que se quiere saber es cuántos lo necesitan.
-}
repartijaDetalleDeItemAbierto : Bool -> Maybe TelemetryEvent
repartijaDetalleDeItemAbierto abierto =
    if abierto then
        Just
            { name = "app.repartija.item_detail_expanded"
            , severity = Info
            , attributes = []
            }

    else
        Nothing


repartijaFiltro : { filtro : String, activo : Bool } -> Maybe TelemetryEvent
repartijaFiltro { filtro, activo } =
    Just
        { name = "app.repartija.filter_toggled"
        , severity = Info
        , attributes =
            [ ( "app.repartija.filtro", Json.string filtro )
            , ( "app.repartija.activo", Json.bool activo )
            ]
        }


{-| Que alguien abrió la lista de quiénes ya reclamaron. Cerrarla no se cuenta:
lo que se quiere saber es cuántos la necesitan.
-}
repartijaParticipantesExpandidos : Bool -> Maybe TelemetryEvent
repartijaParticipantesExpandidos expandido =
    if expandido then
        Just
            { name = "app.repartija.participants_expanded"
            , severity = Info
            , attributes = []
            }

    else
        Nothing


repartijaAyudaAbierta : { ayuda : String, origen : String } -> Maybe TelemetryEvent
repartijaAyudaAbierta { ayuda, origen } =
    Just
        { name = "app.repartija.help_opened"
        , severity = Info
        , attributes =
            [ ( "app.repartija.ayuda", Json.string ayuda )
            , ( "app.repartija.origen", Json.string origen )
            ]
        }


{-| El resultado es `aplicada` o el motivo por el que el backend la rechazó.
-}
repartijaAccion : { accion : String, resultado : String } -> Maybe TelemetryEvent
repartijaAccion { accion, resultado } =
    Just
        { name = "app.repartija.action"
        , severity = Info
        , attributes =
            [ ( "app.repartija.accion", Json.string accion )
            , ( "app.repartija.resultado", Json.string resultado )
            ]
        }


type Severity
    = Info
    | Warn
    | Error


severityToString : Severity -> String
severityToString severity =
    case severity of
        Info ->
            "info"

        Warn ->
            "warn"

        Error ->
            "error"
