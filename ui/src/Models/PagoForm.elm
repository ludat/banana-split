module Models.PagoForm exposing
    ( ModoPartes
    , Section(..)
    , distribucionDeSobrasToString
    , mostrarMontoFijoField
    , mostrarPartesField
    , validatePago
    , validatePagoInSection
    )

{-| Las validaciones del formulario de un gasto: cómo se lee un `Pago` de los
campos del form, y los nombres de los campos que comparten las vistas.

Viven acá y no en la pantalla ni en el componente porque las usan los dos: la
página de pantalla completa (`Pages.Grupos.GrupoId_.Gastos.New`) y el
formulario embebido en el popup (`Components.PagoEditForm`).

-}

import Form.Error as FormError
import Form.Validate as V exposing (Validation, nonEmpty)
import Generated.Api as Api exposing (Distribucion, DistribucionDeSobras(..), Moneda, Pago, Participante, ParticipanteId, Repartija, RepartijaItem, ULID)
import Models.Moneda as Moneda
import Models.Monto as Monto
import Utils.Day exposing (validateDay)
import Utils.Form exposing (CustomFormError)
import Utils.Ulid exposing (emptyUlid)


{-| Los pasos del formulario. Cada uno valida sólo sus campos, así el resumen
de un paso se puede calcular sin que los otros estén completos.
-}
type Section
    = BasicPagoData
    | PagadoresSection
    | DeudoresSection


validatePagoInSection : Section -> List Participante -> Validation CustomFormError Pago
validatePagoInSection section participantes =
    let
        emptyDistribucion =
            { id = emptyUlid, tipo = Api.TipoDistribucionPartes { id = emptyUlid, partes = [] } }
    in
    -- La moneda se lee primero porque es la que define con cuántos decimales se
    -- parsea cada monto del form (ver 'Monto.validateMonto').
    V.field "moneda" Moneda.validate
        |> V.andThen
            (\moneda ->
                V.succeed Pago
                    |> V.andMap (V.field "id" validateId)
                    |> V.andMap
                        (if section == BasicPagoData then
                            V.field "monto" (Monto.validateMonto moneda)

                         else
                            V.maybe (V.field "monto" (Monto.validateMonto moneda)) |> V.map (Maybe.withDefault Monto.zero)
                        )
                    |> V.andMap (V.succeed moneda)
                    |> V.andMap
                        (if section == BasicPagoData then
                            V.field "nombre" (V.string |> V.andThen nonEmpty)

                         else
                            V.succeed ""
                        )
                    |> V.andMap (V.field "fecha" validateDay)
                    |> V.andMap
                        (if section == PagadoresSection then
                            V.field "distribucion_pagadores" <| validateDistribucion moneda participantes

                         else
                            V.succeed emptyDistribucion
                        )
                    |> V.andMap
                        (if section == DeudoresSection then
                            V.field "distribucion_deudores" <| validateDistribucion moneda participantes

                         else
                            V.succeed emptyDistribucion
                        )
            )


validatePago : List Participante -> Validation CustomFormError Pago
validatePago participantes =
    V.field "moneda" Moneda.validate
        |> V.andThen
            (\moneda ->
                V.succeed Pago
                    |> V.andMap (V.field "id" validateId)
                    |> V.andMap (V.field "monto" (Monto.validateMonto moneda))
                    |> V.andMap (V.succeed moneda)
                    |> V.andMap (V.field "nombre" (V.string |> V.andThen nonEmpty))
                    |> V.andMap (V.field "fecha" validateDay)
                    |> V.andMap (V.field "distribucion_pagadores" <| validateDistribucion moneda participantes)
                    |> V.andMap (V.field "distribucion_deudores" <| validateDistribucion moneda participantes)
            )


validateId : Validation CustomFormError ULID
validateId =
    V.defaultValue emptyUlid V.string


distribucionDeSobrasToString : DistribucionDeSobras -> String
distribucionDeSobrasToString distribucionDeSobras =
    case distribucionDeSobras of
        SobrasNoDistribuir ->
            "SobrasNoDistribuir"

        SobrasProporcional ->
            "SobrasProporcional"


validateRepartija : Moneda -> V.Validation CustomFormError Repartija
validateRepartija moneda =
    V.succeed Repartija
        |> V.andMap (V.field "repartija_id" validateId)
        |> V.andMap (V.succeed "GENERATED")
        |> V.andMap (V.field "extra" (Monto.validateMonto moneda))
        |> V.andMap
            (V.field "distribucionDeSobras"
                (V.customValidation V.string
                    (\t ->
                        case t of
                            "SobrasNoDistribuir" ->
                                Ok SobrasNoDistribuir

                            "SobrasProporcional" ->
                                Ok SobrasProporcional

                            _ ->
                                Err <| FormError.value FormError.InvalidString
                    )
                )
            )
        |> V.andMap (V.field "items" (V.list (validateRepartijaItem moneda)))
        |> V.andMap (V.field "claims" (V.succeed []))


validateRepartijaItem : Moneda -> V.Validation CustomFormError RepartijaItem
validateRepartijaItem moneda =
    V.succeed RepartijaItem
        |> V.andMap (V.field "id" validateId)
        |> V.andMap (V.field "nombre" V.string)
        |> V.andMap (V.field "monto" (Monto.validateMonto moneda))
        |> V.andMap (V.field "cantidad" V.int)


validateDistribucion : Moneda -> List Participante -> Validation CustomFormError Distribucion
validateDistribucion moneda participantes =
    V.succeed Distribucion
        |> V.andMap (V.field "id" validateId)
        |> V.andMap
            (V.field "tipo" V.string
                |> V.andThen
                    (\t ->
                        case t of
                            "repartija" ->
                                validateRepartija moneda
                                    |> V.map Api.TipoDistribucionRepartija

                            "partes" ->
                                readModoPartes
                                    |> V.andThen
                                        (\mode ->
                                            V.succeed Api.DistribucionPartes
                                                |> V.andMap (V.field "partes_id" validateId)
                                                |> V.andMap
                                                    (V.field "partes"
                                                        (V.sequence
                                                            (participantes
                                                                |> List.map (\participante -> V.field participante.id (validateParte moneda mode participante.id))
                                                            )
                                                            |> V.map (List.filterMap identity)
                                                        )
                                                    )
                                                |> V.map Api.TipoDistribucionPartes
                                        )

                            _ ->
                                V.fail <| FormError.value FormError.Empty
                    )
            )


{-| Qué columnas del reparto por partes se tienen en cuenta. Los flags viven
dentro del propio formulario (`mostrar_partes` / `mostrar_monto_fijo`), así que
los valores de partes y monto fijo siempre quedan guardados; estos flags sólo
deciden cuáles se ignoran al construir las `Parte`. Con ambos en `False` el
gasto se reparte en partes iguales.
-}
type alias ModoPartes =
    { mostrarPartes : Bool
    , mostrarMontoFijo : Bool
    }


mostrarPartesField : String
mostrarPartesField =
    "mostrar_partes"


mostrarMontoFijoField : String
mostrarMontoFijoField =
    "mostrar_monto_fijo"


readModoPartes : Validation CustomFormError ModoPartes
readModoPartes =
    V.succeed ModoPartes
        |> V.andMap (V.field mostrarPartesField (V.defaultValue False V.bool))
        |> V.andMap (V.field mostrarMontoFijoField (V.defaultValue False V.bool))


validateParte : Moneda -> ModoPartes -> ParticipanteId -> Validation CustomFormError (Maybe Api.Parte)
validateParte moneda mode participanteId =
    let
        validateMonto =
            V.field "monto" (V.defaultValue Monto.zero (Monto.validateMonto moneda))

        validateCuota =
            V.field "cuota" (V.defaultValue 0 V.int)

        -- Cada modo exige exactamente los campos que necesita; si falta
        -- alguno el parser falla en vez de tratarlo como opcional.
        parte =
            case ( mode.mostrarMontoFijo, mode.mostrarPartes ) of
                ( True, True ) ->
                    V.map2 (\monto cuota -> Api.PonderadoYMontoFijo monto cuota participanteId)
                        validateMonto
                        validateCuota

                ( True, False ) ->
                    V.map (\monto -> Api.MontoFijo monto participanteId) validateMonto

                ( False, True ) ->
                    V.map (\cuota -> Api.Ponderado cuota participanteId) validateCuota

                ( False, False ) ->
                    -- En partes iguales: cada participante que participa aporta una parte.
                    V.succeed (Api.Ponderado 1 participanteId)
    in
    V.defaultValue False (V.field "participa" V.bool)
        |> V.andThen
            (\participa ->
                if participa then
                    V.map Just parte

                else
                    V.succeed Nothing
            )
