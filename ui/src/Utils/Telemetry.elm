module Utils.Telemetry exposing (gastoEdicionAbandonada, trackDecodeErrors)

{-| Lo único que el frontend reporta por su cuenta sobre las respuestas de la
API: las que no pudo decodificar.

El backend no puede verlas — contestó 200 y lo logeó como éxito — y la causa
habitual es un cliente corriendo un bundle viejo contra una API más nueva, así
que el evento se cruza con `service.version`, que lleva el hash del build.

El resto de las fallas HTTP (timeout, error de red, 4xx, 5xx) ya salen solas en
los spans de la instrumentación de XHR, con `error.type`. No hace falta
reportarlas desde acá.

-}

import Effect exposing (Effect)
import Http
import Models.PagoForm exposing (Section(..))
import RemoteData exposing (RemoteData(..), WebData)


{-| Reporta un evento si la respuesta falló al decodificar, y nada en cualquier
otro caso.

El `String` de `Http.BadBody` se descarta acá y a propósito: es el mensaje de
error del decoder, que incluye un fragmento del payload — nombres de
participantes, montos, descripciones de gastos. Lo único que viaja es el nombre
de la operación.

-}
trackDecodeErrors : String -> WebData a -> Effect msg
trackDecodeErrors operation response =
    case response of
        Failure (Http.BadBody _) ->
            Effect.telemetryEvent "api.decode_error"
                [ ( "app.api.operation", operation ) ]

        _ ->
            Effect.none


{-| Alguien cargó parte del formulario de un gasto y lo cerró sin guardar.

El backend solo ve los gastos que se guardaron, así que este es el único lugar
donde aparecen los abandonados. Lo accionable es en qué paso se rindieron.

-}
gastoEdicionAbandonada : { esNuevo : Bool, seccion : Section } -> Effect msg
gastoEdicionAbandonada { esNuevo, seccion } =
    Effect.telemetryEvent "gasto.edit_abandoned"
        [ ( "mode"
          , if esNuevo then
                "nuevo"

            else
                "existente"
          )
        , ( "section"
          , case seccion of
                BasicPagoData ->
                    "basico"

                PagadoresSection ->
                    "pagadores"

                DeudoresSection ->
                    "deudores"
          )
        ]
