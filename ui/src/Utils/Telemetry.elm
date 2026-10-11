module Utils.Telemetry exposing (Severity, TelemetryEvent, codigoDeLoginRechazado, gastoEdicionAbandonada, severityToString, trackDecodeErrors)

import Http
import Json.Encode as Json exposing (Value)
import Models.PagoForm exposing (Section(..))
import RemoteData exposing (RemoteData(..), WebData)


type alias TelemetryEvent =
    { name : String
    , severity : Severity
    , attributes : List ( String, Value )
    }


trackDecodeErrors : String -> WebData a -> Maybe TelemetryEvent
trackDecodeErrors operation response =
    case response of
        Failure (Http.BadBody _) ->
            Just
                { name = "api.decode_error"
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
        { name = "auth.code_rejected"
        , severity = Warn
        , attributes =
            [ ( "outcome", Json.string outcome ) ]
        }


gastoEdicionAbandonada : { esNuevo : Bool, seccion : Section } -> Maybe TelemetryEvent
gastoEdicionAbandonada { esNuevo, seccion } =
    Just
        { name = "gasto.edit_abandoned"
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
