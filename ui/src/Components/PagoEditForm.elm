module Components.PagoEditForm exposing (Model, Msg, Outcome(..), ReceiptReadingState, Torta, init, update, view)

{-| El formulario de un gasto (crearlo o editarlo) embebido en el popup, en vez
de en una pantalla propia.

Es una copia del formulario de `Pages.Grupos.GrupoId_.Gastos.New` (las
validaciones, que no dependen de la vista, las comparten vía
`Models.PagoForm`) con las adaptaciones mínimas para vivir adentro de un modal:

  - el gasto y los participantes llegan por parámetro, así que no hay polling
    esperando que el store los traiga;
  - el pie de acciones no es la barra fija de abajo de la pantalla;
  - los gráficos de torta se muestran directo en chiquito en vez de detrás de
    un modal de Bootstrap (no se pueden anidar modales);
  - el selector de participantes muestra siempre las pills, sin el modal que
    usa la pantalla completa en mobile.

-}

import Base64.Encode
import Browser.Dom
import Bytes exposing (Bytes)
import Components.Bootstrap as Bs
import Components.GraficoTorta as GraficoTorta
import Date exposing (Date)
import Effect exposing (Effect)
import File exposing (File)
import Form exposing (Form)
import Form.Field as FormField
import Form.Init as Form
import Generated.Api as Api exposing (Distribucion, DistribucionDeSobras(..), Moneda, Monto, Pago, Participante, ParticipanteId, Repartija, ResumenNetos, ResumenPago, TipoDistribucion(..), ULID)
import Generated.Moneda exposing (escalaDe)
import Html exposing (Html, a, button, div, i, li, p, span, text)
import Html.Attributes as Attr exposing (accept, class, classList, disabled, placeholder, style, target, type_)
import Html.Events exposing (on, onClick, onSubmit)
import Http
import Json.Decode as Decode
import Models.Grupo exposing (GrupoLike)
import Models.LugarAccionable exposing (LugarParaAccionar(..))
import Models.Moneda as Moneda
import Models.Monto as Monto
import Models.PagoForm exposing (ModoPartes, Section(..), distribucionDeSobrasToString, mostrarMontoFijoField, mostrarPartesField, validatePago, validatePagoInSection)
import Models.Parte as Parte
import Models.ResumenNetos exposing (errorAccionableEn, errorMensaje)
import Models.Store as Store
import RemoteData exposing (RemoteData(..), WebData)
import Route.Path as Path
import Task
import Utils.Form exposing (CustomFormError, isDataModifyingEvent)
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts
import Utils.Ulid exposing (emptyUlid)



-- MODEL


type alias Model =
    { grupoId : ULID

    -- `Nothing` es un gasto que todavía no existe: se crea con POST en vez de
    -- actualizarse con PUT.
    , pagoId : Maybe ULID
    , participanteId : Maybe ParticipanteId
    , monedaPorDefecto : Moneda
    , currentSection : Section
    , pagoBasicoForm : Form CustomFormError Pago
    , pagadoresForm : Form CustomFormError Pago
    , resumenPagadores : WebData ResumenPago
    , deudoresForm : Form CustomFormError Pago
    , resumenDeudores : WebData ResumenPago
    , pagoForm : Form CustomFormError Pago
    , resumenPago : WebData ResumenPago
    , receiptParseState : Maybe ReceiptReadingState
    , storedClaims : Maybe { pagadores : List Api.RepartijaClaim, deudores : List Api.RepartijaClaim }
    , hasUnsavedChanges : Bool

    -- El selector de participantes de mobile, con el prefijo del form que está
    -- eligiendo (pagadores o deudores).
    , selectorAbierto : Maybe { titulo : String, prefix : String }
    , tortaAbierta : Maybe Torta
    }


{-| Cuál de los dos gráficos se está mirando en grande.
-}
type Torta
    = TortaPagadores
    | TortaDeudores


type ReceiptReadingState
    = ReadingFile
    | ProcessingWithAI
    | ErrorProcessing String


{-| Lo que el formulario le avisa a quien lo embebe: que se canceló la edición o
que el gasto se guardó (y con qué quedó).
-}
type Outcome
    = SigueEditando
    | Cancelado
    | Guardado Pago


{-| Con `pago = Nothing` el formulario arranca vacío para crear un gasto nuevo:
la fecha de hoy y el creador como único pagador.
-}
init :
    { grupoId : ULID
    , participantes : List Participante
    , participanteId : Maybe ParticipanteId
    , monedaPorDefecto : Moneda
    , today : Date
    , pago : Maybe Pago
    }
    -> ( Model, Effect Msg )
init { grupoId, participantes, participanteId, monedaPorDefecto, today, pago } =
    let
        vacio =
            { grupoId = grupoId
            , pagoId = pago |> Maybe.map .pagoId
            , participanteId = participanteId
            , monedaPorDefecto = monedaPorDefecto
            , currentSection = BasicPagoData
            , pagoBasicoForm = Form.initial [] (validatePagoInSection BasicPagoData participantes)
            , pagadoresForm = Form.initial [] (validatePagoInSection PagadoresSection participantes)
            , resumenPagadores = NotAsked
            , deudoresForm = Form.initial [] (validatePagoInSection DeudoresSection participantes)
            , resumenDeudores = NotAsked
            , pagoForm = Form.initial [] (validatePago participantes)
            , resumenPago = NotAsked
            , receiptParseState = Nothing
            , storedClaims = Nothing
            , hasUnsavedChanges = False
            , selectorAbierto = Nothing
            , tortaAbierta = Nothing
            }
    in
    -- Los resúmenes se piden comparando contra el modelo con los forms todavía
    -- vacíos, así arrancan calculados sobre el gasto que se está editando.
    ( initializePagoForms participantes participanteId today pago vacio, Effect.none )
        |> andThenUpdateResumenesFromForms vacio


initializePagoForms : List Participante -> Maybe ParticipanteId -> Date -> Maybe Pago -> Model -> Model
initializePagoForms participantes creadorId today pago model =
    let
        initialFormValues =
            [ Form.setString "id" (pago |> Maybe.map .pagoId |> Maybe.withDefault "")
            , Form.setString "nombre" (pago |> Maybe.map .nombre |> Maybe.withDefault "")
            , Form.setString "monto" (pago |> Maybe.map (.monto >> Monto.toRawString) |> Maybe.withDefault "")
            , Form.setString "moneda"
                (Moneda.toString (pago |> Maybe.map .moneda |> Maybe.withDefault model.monedaPorDefecto))
            , Form.setString "fecha"
                (pago |> Maybe.map .fecha |> Maybe.withDefault today |> Date.toIsoString)
            , Form.setGroup "distribucion_pagadores" <|
                pagadoresToForm participantes creadorId (Maybe.map .pagadores pago)
            , Form.setGroup "distribucion_deudores" <|
                deudoresToForm participantes (Maybe.map .deudores pago)
            ]
    in
    { model
        | pagoForm = Form.initial initialFormValues (validatePago participantes)
        , pagadoresForm = Form.initial initialFormValues (validatePagoInSection PagadoresSection participantes)
        , deudoresForm = Form.initial initialFormValues (validatePagoInSection DeudoresSection participantes)
        , pagoBasicoForm = Form.initial initialFormValues (validatePagoInSection BasicPagoData participantes)
        , storedClaims =
            pago
                |> Maybe.map
                    (\p ->
                        { pagadores = extractClaimsFromDistribucion p.pagadores
                        , deudores = extractClaimsFromDistribucion p.deudores
                        }
                    )
        , hasUnsavedChanges = False
    }



-- FORM <-> PAGO


extractClaimsFromDistribucion : Distribucion -> List Api.RepartijaClaim
extractClaimsFromDistribucion distribucion =
    case distribucion.tipo of
        TipoDistribucionRepartija repartija ->
            repartija.claims

        _ ->
            []


mergeClaimsIntoPago : Maybe { pagadores : List Api.RepartijaClaim, deudores : List Api.RepartijaClaim } -> Pago -> Pago
mergeClaimsIntoPago maybeClaims pago =
    case maybeClaims of
        Nothing ->
            pago

        Just claims ->
            { pago
                | pagadores = mergeClaimsIntoDistribucion claims.pagadores pago.pagadores
                , deudores = mergeClaimsIntoDistribucion claims.deudores pago.deudores
            }


mergeClaimsIntoDistribucion : List Api.RepartijaClaim -> Distribucion -> Distribucion
mergeClaimsIntoDistribucion claims distribucion =
    case distribucion.tipo of
        TipoDistribucionRepartija repartija ->
            { distribucion
                | tipo = TipoDistribucionRepartija { repartija | claims = claims }
            }

        _ ->
            distribucion


defaultRepartija : Repartija
defaultRepartija =
    { id = emptyUlid
    , nombre = "GENERATED"
    , items = []
    , claims = []
    , extra = Monto.zero
    , distribucionDeSobras = SobrasNoDistribuir
    }


{-| Los pagadores se editan con la misma distribución por partes que los
deudores; las distribuciones existentes se convierten conservando sus partes.
-}
pagadoresToForm : List Participante -> Maybe ParticipanteId -> Maybe Distribucion -> List ( String, FormField.Field )
pagadoresToForm participantes creadorId distribucion =
    let
        modo =
            case distribucion |> Maybe.map .tipo of
                Just (TipoDistribucionPartes _) ->
                    deriveModoPartes distribucion

                _ ->
                    { mostrarPartes = False, mostrarMontoFijo = False }

        partes =
            case distribucion |> Maybe.map .tipo of
                Just (TipoDistribucionPartes p) ->
                    p

                Nothing ->
                    { id = emptyUlid
                    , partes =
                        creadorId
                            |> Maybe.map (\id -> [ Api.Ponderado 1 id ])
                            |> Maybe.withDefault []
                    }

                _ ->
                    { id = emptyUlid, partes = [] }
    in
    [ Form.setString "id" (distribucion |> Maybe.map .id |> Maybe.withDefault emptyUlid)
    , Form.setBool mostrarPartesField modo.mostrarPartes
    , Form.setBool mostrarMontoFijoField modo.mostrarMontoFijo
    ]
        ++ partesToForm participantes partes


{-| Deduce qué columnas mostrar (partes / monto fijo) a partir de las `Parte`
ya guardadas. Si todas son `Ponderado 1` se asume "en partes iguales" (ambos
flags en `False`).
-}
deriveModoPartes : Maybe Distribucion -> ModoPartes
deriveModoPartes distribucion =
    case distribucion |> Maybe.map .tipo of
        Just (TipoDistribucionPartes partes) ->
            { mostrarPartes =
                partes.partes
                    |> List.any
                        (\parte ->
                            case parte of
                                Api.Ponderado cuota _ ->
                                    cuota /= 1

                                Api.PonderadoYMontoFijo _ _ _ ->
                                    True

                                Api.MontoFijo _ _ ->
                                    False
                        )
            , mostrarMontoFijo =
                partes.partes
                    |> List.any
                        (\parte ->
                            case parte of
                                Api.MontoFijo _ _ ->
                                    True

                                Api.PonderadoYMontoFijo _ _ _ ->
                                    True

                                Api.Ponderado _ _ ->
                                    False
                        )
            }

        _ ->
            { mostrarPartes = False, mostrarMontoFijo = False }


deudoresToForm : List Participante -> Maybe Distribucion -> List ( String, FormField.Field )
deudoresToForm participantes distribucion =
    let
        modo =
            deriveModoPartes distribucion

        partesEquitativas incluidos =
            { id = emptyUlid
            , partes = incluidos |> List.map (Api.Ponderado 1)
            }

        tipoInicial =
            case distribucion |> Maybe.map .tipo of
                Just (TipoDistribucionRepartija _) ->
                    "repartija"

                Just (TipoDistribucionPartes _) ->
                    "partes"

                Nothing ->
                    ""
    in
    [ Form.setString "id" (distribucion |> Maybe.map .id |> Maybe.withDefault emptyUlid)
    , Form.setBool mostrarPartesField modo.mostrarPartes
    , Form.setBool mostrarMontoFijoField modo.mostrarMontoFijo
    ]
        ++ (case distribucion |> Maybe.map .tipo of
                Just (TipoDistribucionRepartija repartija) ->
                    partesToForm participantes (partesEquitativas (participantes |> List.map .id))
                        ++ repartijaToForm repartija

                Just (TipoDistribucionPartes partes) ->
                    repartijaToForm defaultRepartija
                        ++ partesToForm participantes partes

                Nothing ->
                    repartijaToForm defaultRepartija
                        ++ partesToForm participantes (partesEquitativas (participantes |> List.map .id))
           )
        -- `partesToForm`/`repartijaToForm` fijan su propio "tipo"; lo
        -- sobreescribimos al final para que mande `tipoInicial`.
        ++ [ Form.setString "tipo" tipoInicial ]


partesToForm : List Participante -> Api.DistribucionPartes -> List ( String, FormField.Field )
partesToForm participantes distribucion =
    [ Form.setString "partes_id" distribucion.id
    , Form.setString "tipo" "partes"
    , Form.setGroup "partes"
        (participantes
            |> List.map
                (\participante ->
                    let
                        campos =
                            case
                                distribucion.partes
                                    |> List.filter (\parte -> Parte.participanteId parte == participante.id)
                                    |> List.head
                            of
                                Nothing ->
                                    { participa = False, cuota = "1", monto = Monto.toRawString Monto.zero }

                                Just (Api.Ponderado cuota _) ->
                                    { participa = True, cuota = String.fromInt cuota, monto = Monto.toRawString Monto.zero }

                                Just (Api.MontoFijo monto _) ->
                                    { participa = True, cuota = "1", monto = Monto.toRawString monto }

                                Just (Api.PonderadoYMontoFijo monto cuota _) ->
                                    { participa = True, cuota = String.fromInt cuota, monto = Monto.toRawString monto }
                    in
                    Form.setGroup participante.id
                        [ Form.setBool "participa" campos.participa
                        , Form.setString "cuota" campos.cuota
                        , Form.setString "monto" campos.monto
                        ]
                )
        )
    ]


repartijaToForm : Repartija -> List ( String, FormField.Field )
repartijaToForm repartija =
    [ Form.setString "repartija_id" repartija.id
    , Form.setString "tipo" "repartija"
    , Form.setString "extra" (Monto.toRawString repartija.extra)
    , Form.setString "distribucionDeSobras"
        (distribucionDeSobrasToString repartija.distribucionDeSobras)
    , Form.setList "items"
        (repartija.items
            |> List.map
                (\item ->
                    FormField.group
                        [ Form.setString "id" <| item.id
                        , Form.setString "monto" <| Monto.toRawString item.monto
                        , Form.setString "cantidad" <| String.fromInt item.cantidad
                        , Form.setString "nombre" item.nombre
                        ]
                )
        )
    ]


receiptItemsFormMsgs : String -> List Api.RepartijaItem -> Form CustomFormError Pago -> List Form.Msg
receiptItemsFormMsgs prefix items form =
    let
        startingIndex =
            List.length (Form.getListIndexes (prefix ++ ".items") form)
    in
    items
        |> List.indexedMap
            (\idx item ->
                let
                    itemPrefix =
                        prefix ++ ".items." ++ String.fromInt (startingIndex + idx)
                in
                [ Form.Append (prefix ++ ".items")
                , Form.Input (itemPrefix ++ ".id") Form.Text (FormField.String emptyUlid)
                , Form.Input (itemPrefix ++ ".nombre") Form.Text (FormField.String item.nombre)
                , Form.Input (itemPrefix ++ ".monto") Form.Text (FormField.String (Monto.toRawString item.monto))
                , Form.Input (itemPrefix ++ ".cantidad") Form.Text (FormField.String (String.fromInt item.cantidad))
                ]
            )
        |> List.concat



-- UPDATE


type Msg
    = NoOp
    | PagoForm Form.Msg
    | GuardadoPagoResponse (Result Http.Error Pago)
    | SelectSection Section
    | SubmitCurrentSection
    | ResumenPagoUpdated (WebData ResumenPago)
    | ResumenDeudoresUpdated (WebData ResumenPago)
    | ResumenPagadoresUpdated (WebData ResumenPago)
    | ReceiptImageSelected File
    | ReceiptImageBytes File Bytes
    | ReceiptParseResponse (Result Http.Error Api.ReceiptImageResponse)
    | ClearReceiptError
    | AbrirSelector { titulo : String, prefix : String }
    | CerrarSelector
    | AbrirTorta Torta
    | CerrarTorta
    | Cancel


update : List Participante -> Msg -> Model -> ( Model, Effect Msg, Outcome )
update participantes msg model =
    updateInterno participantes msg model
        |> avisarCambiosSinGuardar


{-| Mientras el formulario tenga cambios sin guardar, el browser pregunta antes
de recargar o cerrar la pestaña. Al salir de la edición (guardando o
cancelando) el aviso se apaga.
-}
avisarCambiosSinGuardar : ( Model, Effect Msg, Outcome ) -> ( Model, Effect Msg, Outcome )
avisarCambiosSinGuardar ( model, effect, outcome ) =
    let
        hayQueAvisar =
            case outcome of
                SigueEditando ->
                    model.hasUnsavedChanges

                _ ->
                    False
    in
    ( model
    , Effect.batch [ effect, Effect.setUnsavedChangesWarning hayQueAvisar ]
    , outcome
    )


updateInterno : List Participante -> Msg -> Model -> ( Model, Effect Msg, Outcome )
updateInterno participantes msg model =
    case msg of
        NoOp ->
            sigue ( model, Effect.none )

        Cancel ->
            ( model, Effect.none, Cancelado )

        GuardadoPagoResponse (Ok pago) ->
            ( { model | pagoId = Just pago.pagoId }
                |> initializePagoForms participantes model.participanteId pago.fecha (Just pago)
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshPagos model.grupoId model.participanteId
                , Store.setPago pago.pagoId pago
                , Toasts.pushToast Toasts.ToastSuccess <|
                    case model.pagoId of
                        Just _ ->
                            "Se actualizó el gasto"

                        Nothing ->
                            "Se creó el gasto"
                ]
            , Guardado pago
            )

        GuardadoPagoResponse (Err _) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger <|
                case model.pagoId of
                    Just _ ->
                        "Falló la actualización del gasto"

                    Nothing ->
                        "Falló la creación del gasto"
            , SigueEditando
            )

        PagoForm Form.Submit ->
            case Form.getOutput model.pagoForm of
                Just pago ->
                    let
                        pagoConClaims =
                            mergeClaimsIntoPago model.storedClaims pago
                    in
                    sigue
                        ( { model | pagoForm = Form.update (validatePago participantes) Form.Submit model.pagoForm }
                        , Effect.sendCmd <|
                            case model.pagoId of
                                Just pagoId ->
                                    Api.putGrupoByIdPagosByPagoId model.grupoId pagoId pagoConClaims GuardadoPagoResponse

                                Nothing ->
                                    Api.postGrupoByIdPagos model.grupoId pagoConClaims GuardadoPagoResponse
                        )

                Nothing ->
                    sigue
                        ( { model | pagoForm = Form.update (validatePago participantes) Form.Submit model.pagoForm }
                        , Effect.none
                        )

        PagoForm formMsg ->
            let
                newModel =
                    updateAllForms participantes [ formMsg ] model
            in
            sigue
                (( { newModel
                    | hasUnsavedChanges =
                        if isDataModifyingEvent formMsg then
                            True

                        else
                            model.hasUnsavedChanges
                   }
                 , Effect.none
                 )
                    |> andThenUpdateResumenesFromForms model
                )

        ResumenPagoUpdated resumen ->
            sigue ( { model | resumenPago = resumen }, Effect.none )

        ResumenDeudoresUpdated resumen ->
            sigue ( { model | resumenDeudores = resumen }, Effect.none )

        ResumenPagadoresUpdated resumen ->
            sigue ( { model | resumenPagadores = resumen }, Effect.none )

        ClearReceiptError ->
            sigue ( { model | receiptParseState = Nothing }, Effect.none )

        AbrirSelector selector ->
            sigue ( { model | selectorAbierto = Just selector }, Effect.none )

        CerrarSelector ->
            sigue ( { model | selectorAbierto = Nothing }, Effect.none )

        AbrirTorta torta ->
            sigue ( { model | tortaAbierta = Just torta }, Effect.none )

        CerrarTorta ->
            sigue ( { model | tortaAbierta = Nothing }, Effect.none )

        ReceiptImageSelected file ->
            if List.member (File.mime file) allowedMimeTypesForReceiptUpload then
                sigue
                    ( { model | receiptParseState = Just ReadingFile }
                    , Effect.sendCmd <| Task.perform (ReceiptImageBytes file) (File.toBytes file)
                    )

            else
                sigue
                    ( { model | receiptParseState = Just <| ErrorProcessing <| "Este archivo no es una imagen (" ++ File.mime file ++ ")" }
                    , Effect.none
                    )

        ReceiptImageBytes file bytes ->
            let
                base64 =
                    Base64.Encode.encode (Base64.Encode.bytes bytes)
            in
            sigue
                ( { model | receiptParseState = Just ProcessingWithAI }
                , Effect.sendCmd <| Api.postReceiptParseimage { imageBase64 = File.mime file ++ ";base64," ++ base64 } ReceiptParseResponse
                )

        ReceiptParseResponse result ->
            case result of
                Ok (Api.ReceiptImageSuccess { items }) ->
                    let
                        formMsgs =
                            Form.Input "distribucion_deudores.tipo" Form.Select (FormField.String "repartija")
                                :: receiptItemsFormMsgs "distribucion_deudores" items model.pagoForm

                        newModel =
                            updateAllForms participantes formMsgs { model | hasUnsavedChanges = True }
                    in
                    sigue
                        (( { newModel | receiptParseState = Nothing }
                         , Toasts.pushToast Toasts.ToastSuccess "Recibo parseado correctamente"
                         )
                            |> andThenUpdateResumenesFromForms model
                        )

                Ok (Api.ReceiptImageError { error }) ->
                    sigue ( { model | receiptParseState = Just (ErrorProcessing error) }, Effect.none )

                Err _ ->
                    sigue ( { model | receiptParseState = Just (ErrorProcessing "Error al enviar la imagen") }, Effect.none )

        SelectSection section ->
            -- Cambiar de paso cierra el selector: es de la sección que se deja.
            sigue
                (( { model | currentSection = section, selectorAbierto = Nothing }, Effect.none )
                    |> andThenFocusFieldIfSectionChanged model.currentSection
                    |> andThenUpdateResumenesFromForms model
                )

        SubmitCurrentSection ->
            case model.currentSection of
                BasicPagoData ->
                    sigue
                        (( { model
                            | pagoBasicoForm = Form.update (validatePagoInSection BasicPagoData participantes) Form.Submit model.pagoBasicoForm
                            , currentSection = PagadoresSection
                            , selectorAbierto = Nothing
                           }
                         , Effect.none
                         )
                            |> andThenFocusFieldIfSectionChanged model.currentSection
                            |> andThenUpdateResumenesFromForms model
                        )

                PagadoresSection ->
                    sigue
                        (( { model
                            | pagadoresForm = Form.update (validatePagoInSection PagadoresSection participantes) Form.Submit model.pagadoresForm
                            , currentSection = DeudoresSection
                            , selectorAbierto = Nothing
                           }
                         , Effect.none
                         )
                            |> andThenFocusFieldIfSectionChanged model.currentSection
                            |> andThenUpdateResumenesFromForms model
                        )

                DeudoresSection ->
                    updateInterno participantes (PagoForm Form.Submit) model


{-| El caso común: el formulario sigue abierto, no hay nada que avisarle a quien
lo embebe.
-}
sigue : ( Model, Effect Msg ) -> ( Model, Effect Msg, Outcome )
sigue ( model, effect ) =
    ( model, effect, SigueEditando )


updateAllForms : List Participante -> List Form.Msg -> Model -> Model
updateAllForms participantes formMsgs model =
    List.foldl
        (\formMsg m ->
            { m
                | pagoForm = Form.update (validatePago participantes) formMsg m.pagoForm
                , pagoBasicoForm = Form.update (validatePagoInSection BasicPagoData participantes) formMsg m.pagoBasicoForm
                , pagadoresForm = Form.update (validatePagoInSection PagadoresSection participantes) formMsg m.pagadoresForm
                , deudoresForm = Form.update (validatePagoInSection DeudoresSection participantes) formMsg m.deudoresForm
            }
        )
        model
        formMsgs


andThenFocusFieldIfSectionChanged : Section -> ( Model, Effect Msg ) -> ( Model, Effect Msg )
andThenFocusFieldIfSectionChanged oldSection ( model, oldEffects ) =
    let
        focusEffect =
            if oldSection == model.currentSection then
                Effect.none

            else
                case model.currentSection of
                    BasicPagoData ->
                        Effect.sendCmd <| Task.attempt (\_ -> NoOp) (Browser.Dom.focus "nombre")

                    PagadoresSection ->
                        Effect.sendCmd <| Task.attempt (\_ -> NoOp) (Browser.Dom.focus "pagadores-seleccionar")

                    DeudoresSection ->
                        Effect.sendCmd <| Task.attempt (\_ -> NoOp) (Browser.Dom.focus "deudores-modalidad")
    in
    ( model
    , Effect.batch [ oldEffects, focusEffect ]
    )


andThenUpdateResumenesFromForms : Model -> ( Model, Effect Msg ) -> ( Model, Effect Msg )
andThenUpdateResumenesFromForms originalModel ( model, oldEffects ) =
    let
        updateResumenFromForm getForm event =
            case Form.getOutput (getForm model) of
                Just pago ->
                    if Form.getOutput (getForm originalModel) == Just pago then
                        Effect.none

                    else
                        let
                            pagoWithClaims =
                                mergeClaimsIntoPago model.storedClaims pago
                        in
                        Effect.batch
                            [ Effect.sendMsg <| event Loading
                            , Effect.sendCmd <| Api.postPagosResumen pagoWithClaims (RemoteData.fromResult >> event)
                            ]

                Nothing ->
                    Effect.sendMsg <| event NotAsked
    in
    ( model
    , Effect.batch
        [ oldEffects
        , updateResumenFromForm .pagoForm ResumenPagoUpdated
        , updateResumenFromForm .pagadoresForm ResumenPagadoresUpdated
        , updateResumenFromForm .deudoresForm ResumenDeudoresUpdated
        ]
    )



-- VIEW


view : GrupoLike g -> Model -> Html Msg
view grupo model =
    div []
        [ if model.hasUnsavedChanges then
            Bs.alert Bs.AlertWarning [ style "margin-bottom" "1rem" ] [ text "Hay cambios sin guardar" ]

          else
            text ""
        , viewStepTabs model
        , case model.currentSection of
            BasicPagoData ->
                viewBasicSection model

            PagadoresSection ->
                viewPagadoresSection grupo model

            DeudoresSection ->
                viewDeudoresSection grupo model
        , viewSelectorOverlay grupo.participantes model
        , viewTortaOverlay grupo model
        ]


hasActionableErrors : LugarParaAccionar -> WebData ResumenPago -> (ResumenPago -> ResumenNetos) -> Bool
hasActionableErrors lugar resumenData accessor =
    case resumenData of
        Success resumen ->
            List.any (\e -> List.member lugar (errorAccionableEn e.tipo)) (accessor resumen).errores

        _ ->
            False


viewErrorFromResumenData : WebData ResumenPago -> (ResumenPago -> ResumenNetos) -> Html msg
viewErrorFromResumenData resumenData accessor =
    case resumenData of
        Success resumenPago ->
            viewErrorFromResumen Lugar_CreacionPago (accessor resumenPago)

        _ ->
            text ""


viewErrorFromResumen : LugarParaAccionar -> ResumenNetos -> Html msg
viewErrorFromResumen lugar resumen =
    case resumen.errores of
        [] ->
            text ""

        _ ->
            div []
                (resumen.errores
                    |> List.map
                        (\error ->
                            let
                                esAccionable =
                                    List.member lugar (errorAccionableEn error.tipo)

                                mensaje =
                                    errorMensaje error.tipo
                            in
                            Bs.alert
                                (if esAccionable then
                                    Bs.AlertDanger

                                 else
                                    Bs.AlertInfo
                                )
                                [ style "margin-bottom" "0.5rem" ]
                                [ case error.objeto of
                                    [] ->
                                        text mensaje

                                    _ ->
                                        div [ style "display" "flex", style "gap" "0.5rem", style "align-items" "baseline" ]
                                            [ Html.strong [] [ text (String.join " > " error.objeto ++ ":") ]
                                            , text mensaje
                                            ]
                                ]
                        )
                )


{-| Pie de acciones de cada paso. En la pantalla completa esto es una barra fija
al fondo; adentro del modal fluye al final del formulario y ya.
-}
viewActionFooter : List (Html Msg) -> Html Msg
viewActionFooter children =
    div [ class "mt-4" ] children


porcionesTorta : GrupoLike g -> Maybe Monto -> WebData ResumenPago -> (ResumenPago -> ResumenNetos) -> List GraficoTorta.PorcionTorta
porcionesTorta grupo totalPago resumenData accessor =
    case resumenData of
        Success resumenPago ->
            GraficoTorta.porciones grupo totalPago (accessor resumenPago)

        _ ->
            []


{-| La torta del paso, en chiquito y clickeable para verla grande. El grande no
es un modal de Bootstrap (no se pueden anidar) sino `viewTortaOverlay`.
-}
viewTortaFooter : GrupoLike g -> Maybe Monto -> WebData ResumenPago -> (ResumenPago -> ResumenNetos) -> Torta -> Html Msg
viewTortaFooter grupo totalPago resumenData accessor torta =
    button
        [ type_ "button"
        , class "btn p-0 border-0 flex-shrink-0 d-flex align-items-center"
        , Attr.attribute "aria-label" "Ver el gráfico en grande"
        , onClick (AbrirTorta torta)
        ]
        [ GraficoTorta.viewTortaMini (porcionesTorta grupo totalPago resumenData accessor) ]


{-| El gráfico en grande, encima del popup. Las porciones se recalculan al
dibujarlo, así sigue los cambios del formulario mientras está abierto.
-}
viewTortaOverlay : GrupoLike g -> Model -> Html Msg
viewTortaOverlay grupo model =
    case model.tortaAbierta of
        Nothing ->
            text ""

        Just torta ->
            let
                totalPago =
                    Form.getOutput model.pagoBasicoForm |> Maybe.map .monto

                ( titulo, porciones ) =
                    case torta of
                        TortaPagadores ->
                            ( "Pago", porcionesTorta grupo totalPago model.resumenPagadores .resumenPagadores )

                        TortaDeudores ->
                            ( "Reparto", porcionesTorta grupo totalPago model.resumenDeudores .resumenDeudores )
            in
            div []
                [ div
                    [ class "modal d-block"
                    , style "z-index" "1070"
                    , Attr.tabindex -1
                    , Attr.attribute "aria-modal" "true"
                    , Attr.attribute "role" "dialog"
                    ]
                    [ div [ class "modal-dialog modal-dialog-centered modal-dialog-scrollable" ]
                        [ div [ class "modal-content" ]
                            [ div [ class "modal-header" ]
                                [ Html.h5 [ class "modal-title" ] [ text titulo ]
                                , button
                                    [ type_ "button"
                                    , class "btn-close"
                                    , Attr.attribute "aria-label" "Cerrar"
                                    , onClick CerrarTorta
                                    ]
                                    []
                                ]
                            , div [ class "modal-body" ] [ GraficoTorta.viewTortaGrande porciones ]
                            ]
                        ]
                    ]
                , div [ class "modal-backdrop show", style "z-index" "1065" ] []
                ]


{-| Wizard de pasos como tabs por defecto de Bootstrap (`nav-tabs`). Marca el
paso actual y permite saltar a cualquier paso con `SelectSection`.
-}
viewStepTabs : Model -> Html Msg
viewStepTabs model =
    let
        tab section label =
            let
                active =
                    model.currentSection == section
            in
            li [ class "nav-item" ]
                [ button
                    [ type_ "button"
                    , classList
                        [ ( "nav-link", True )
                        , ( "active", active )

                        -- Paso incompleto (todavía sin completar): se ve grisado
                        -- en vez de como error, así no aparece "en rojo" al inicio.
                        , ( "text-body-tertiary", not active && sectionIncomplete model section )
                        ]
                    , onClick (SelectSection section)
                    ]
                    (text label
                        :: (if not (sectionIncomplete model section) && sectionHasError model section then
                                [ i [ class "bi bi-exclamation-circle-fill text-danger ms-1" ] [] ]

                            else
                                []
                           )
                    )
                ]
    in
    div [ class "d-flex align-items-end gap-3 mb-4" ]
        [ Html.ul [ class "nav nav-tabs flex-grow-1" ]
            [ tab BasicPagoData "Gasto"
            , tab PagadoresSection "Pago"
            , tab DeudoresSection "Reparto"
            ]
        , viewMontoChip model
        ]


{-| El total del gasto, para tenerlo a la vista mientras se reparte. En el paso
"Gasto" no hace falta: el monto se está editando ahí mismo.
-}
viewMontoChip : Model -> Html Msg
viewMontoChip model =
    case ( model.currentSection, Form.getOutput model.pagoBasicoForm ) of
        ( BasicPagoData, _ ) ->
            text ""

        ( _, Nothing ) ->
            text ""

        ( _, Just pago ) ->
            div [ class "text-end flex-shrink-0 mb-1" ]
                [ div [ class "text-body-secondary text-uppercase", style "font-size" "0.75rem" ] [ text "Monto" ]
                , div [ class "fw-bold" ] [ text (Moneda.simboloUnico pago.moneda ++ " " ++ Monto.toString pago.monto) ]
                ]


{-| Un paso está incompleto cuando su form todavía no produce un valor válido.
Se muestra grisado (no como error) para no alarmar al inicio.
-}
sectionIncomplete : Model -> Section -> Bool
sectionIncomplete model section =
    let
        faltaTotal =
            Form.getOutput model.pagoBasicoForm == Nothing
    in
    case section of
        BasicPagoData ->
            faltaTotal

        PagadoresSection ->
            let
                sinPagadores =
                    case Form.getOutput model.pagadoresForm of
                        Just pago ->
                            distribucionSinPartes pago.pagadores

                        Nothing ->
                            True
            in
            faltaTotal || sinPagadores

        DeudoresSection ->
            faltaTotal || Form.getOutput model.deudoresForm == Nothing


distribucionSinPartes : Distribucion -> Bool
distribucionSinPartes distribucion =
    case distribucion.tipo of
        Api.TipoDistribucionPartes { partes } ->
            List.isEmpty partes

        Api.TipoDistribucionRepartija _ ->
            False


{-| Un paso tiene error sólo cuando ya tiene datos cargados pero su resumen
marca errores accionables (p. ej. los montos no cuadran). Un paso simplemente
incompleto no cuenta como error: se muestra grisado vía `sectionIncomplete`.
-}
sectionHasError : Model -> Section -> Bool
sectionHasError model section =
    case section of
        BasicPagoData ->
            False

        PagadoresSection ->
            hasActionableErrors Lugar_CreacionPago model.resumenPagadores .resumenPagadores

        DeudoresSection ->
            hasActionableErrors Lugar_CreacionPago model.resumenDeudores .resumenDeudores


viewBasicSection : Model -> Html Msg
viewBasicSection model =
    let
        form =
            model.pagoBasicoForm

        nombreField =
            Form.getFieldAsString "nombre" form

        montoField =
            Form.getFieldAsString "monto" form

        monedaField =
            Form.getFieldAsString "moneda" form

        fechaField =
            Form.getFieldAsString "fecha" form
    in
    Html.form [ onSubmit SubmitCurrentSection ]
        [ Html.map PagoForm <|
            Bs.textFormItem nombreField
                { label = "Título"
                , placeholder = Just "Restaurant El Oso Pardo"
                , required = True
                }
        , div [ class "d-flex gap-3 flex-wrap align-items-start" ]
            [ div [ class "flex-grow-1 mb-3" ]
                [ Html.label [ class "form-label", Attr.for "monto" ]
                    [ text "Monto total", Bs.requiredMarker True ]
                , div [ class "d-flex gap-2" ]
                    [ Html.map PagoForm <|
                        Bs.selectInput
                            (Moneda.todas |> List.map (\m -> ( Moneda.toString m, Moneda.simboloUnico m )))
                            monedaField
                            [ style "max-width" "6.5rem" ]
                    , div [ class "flex-grow-1" ]
                        [ Html.map PagoForm <|
                            Bs.montoInput (decimalesDelForm form) montoField [ placeholder "33.000,00" ]
                        ]
                    ]
                ]
            , Html.map PagoForm <|
                Bs.dateFormItem fechaField
                    { label = "Fecha"
                    , required = True
                    }
            ]
        , viewActionFooter
            [ viewBotonera
                [ Bs.btn Bs.Primary
                    [ disabled (Form.getOutput form == Nothing)
                    , onClick SubmitCurrentSection
                    , class "flex-grow-1"
                    ]
                    [ text "Siguiente" ]
                ]
            ]
        ]


viewPagadoresSection : GrupoLike g -> Model -> Html Msg
viewPagadoresSection grupo model =
    let
        form =
            model.pagadoresForm

        prefix =
            "distribucion_pagadores"

        incluidos =
            grupo.participantes
                |> List.filter
                    (\participante ->
                        (Form.getFieldAsBool (prefix ++ ".partes." ++ participante.id ++ ".participa") form).value == Just True
                    )

        mode =
            { mostrarPartes =
                (Form.getFieldAsBool (prefix ++ "." ++ mostrarPartesField) form).value == Just True
            , mostrarMontoFijo =
                (Form.getFieldAsBool (prefix ++ "." ++ mostrarMontoFijoField) form).value == Just True
            }
    in
    Html.form [ onSubmit SubmitCurrentSection ]
        [ div [ class "d-flex flex-wrap align-items-center gap-2 mb-2" ]
            [ Html.h5 [ class "mb-0" ] [ text "Quienes pagaron" ] ]
        , viewSeleccionarParticipantes
            { titulo = "Quienes pagaron", id = "pagadores-seleccionar" }
            grupo.participantes
            prefix
            form
        , Html.hr [ class "my-4" ] []
        , div [ class "d-flex flex-wrap gap-2 mb-3" ]
            [ viewModoChip (prefix ++ "." ++ mostrarMontoFijoField) mode.mostrarMontoFijo "Monto fijo"
            , viewModoChip (prefix ++ "." ++ mostrarPartesField) mode.mostrarPartes "Partes"
            ]
        , viewPartesTable grupo.participantes prefix mode incluidos (Form.getOutput form |> Maybe.map (\pago -> sumaMontosFijos pago.pagadores)) form
        , viewActionFooter
            [ viewErrorFromResumenData model.resumenPagadores .resumenPagadores
            , viewBotonera
                [ viewTortaFooter grupo (Form.getOutput model.pagoBasicoForm |> Maybe.map .monto) model.resumenPagadores .resumenPagadores TortaPagadores
                , Bs.btn Bs.Primary
                    [ disabled (Form.getOutput form == Nothing)
                    , onClick SubmitCurrentSection
                    , class "flex-grow-1"
                    ]
                    [ text "Siguiente" ]
                ]
            ]
        ]


viewDeudoresSection : GrupoLike g -> Model -> Html Msg
viewDeudoresSection grupo model =
    let
        form =
            model.deudoresForm

        tipoField =
            Form.getFieldAsString "distribucion_deudores.tipo" form
    in
    Html.form [ onSubmit <| PagoForm Form.Submit ]
        [ viewReceiptBanner model.receiptParseState
        , viewModalidadSelector tipoField
        , case tipoField.value of
            Just "repartija" ->
                div []
                    [ viewRepartijaForm "distribucion_deudores" form
                    , viewRepartijaLink model.grupoId form
                    ]

            Just "partes" ->
                viewPartesForm grupo "distribucion_deudores" form

            _ ->
                text ""
        , viewActionFooter
            [ -- Sólo los errores de esta sección (deudores) y sin el tag de
              -- scope: `.resumenDeudores` ya viene sin el prefijo "deudores"
              -- que `.resumen` agrega al combinar pagadores y deudores.
              viewErrorFromResumenData model.resumenDeudores .resumenDeudores
            , viewBotonera
                [ viewTortaFooter grupo (Form.getOutput model.pagoBasicoForm |> Maybe.map .monto) model.resumenDeudores .resumenDeudores TortaDeudores
                , Bs.btn Bs.Primary
                    [ -- Se permite enviar aunque el gasto sea inválido; el
                      -- backend lo guarda igual y deja los motivos en su
                      -- resumen. Se bloquea si el form no es construible o
                      -- si no hay cambios para guardar.
                      disabled (Form.getOutput model.pagoForm == Nothing || not model.hasUnsavedChanges)
                    , onClick (PagoForm Form.Submit)
                    , Attr.id "pago-submit-button"
                    , class "flex-grow-1"
                    ]
                    [ text <|
                        case model.pagoId of
                            Just _ ->
                                "Actualizar gasto"

                            Nothing ->
                                "Crear"
                    ]
                ]
            ]
        ]


{-| La fila de botones del pie: lo que venga (la torta, el botón del paso) más
el "Cancelar" que vuelve al detalle del gasto.
-}
viewBotonera : List (Html Msg) -> Html Msg
viewBotonera children =
    div [ class "d-flex gap-2 align-items-stretch" ]
        (Bs.btn Bs.Transparent [ onClick Cancel ] [ text "Cancelar" ] :: children)


viewModalidadSelector : Form.FieldState CustomFormError String -> Html Msg
viewModalidadSelector tipoField =
    let
        card value icono titulo descripcion attrs =
            let
                active =
                    tipoField.value == Just value

                -- Borde resaltado con `--bs-emphasis-color`, que se invierte según
                -- el tema (casi negro en claro, casi blanco en oscuro), así se ve
                -- bien en ambos. Sin esto un borde fijo oscuro quedaría invisible
                -- sobre el fondo oscuro.
                emphasisBorder =
                    if active then
                        [ style "border-color" "var(--bs-emphasis-color)" ]

                    else
                        []
            in
            button
                ([ type_ "button"
                 , classList
                    [ ( "card flex-fill text-center border-2 position-relative", True )
                    , ( "shadow-sm", active )
                    ]
                 , onClick <| PagoForm <| Form.Input tipoField.path Form.Select (FormField.String value)
                 ]
                    ++ emphasisBorder
                    ++ attrs
                )
                [ -- El círculo del ícono se posiciona absoluto sobre el borde
                  -- superior (translate-middle lo centra ahí) y lleva fondo
                  -- `bg-body` para tapar la línea del borde por detrás.
                  span
                    [ class "position-absolute top-0 start-50 translate-middle d-inline-flex align-items-center justify-content-center rounded-circle fs-4 bg-body"
                    , style "width" "3.5rem"
                    , style "height" "3.5rem"
                    , style "border-style" "solid"
                    , style "border-width"
                        (if active then
                            "2px"

                         else
                            "var(--bs-border-width)"
                        )
                    , style "border-color"
                        (if active then
                            "var(--bs-emphasis-color)"

                         else
                            "var(--bs-border-color)"
                        )
                    ]
                    [ i [ class icono ] [] ]
                , div [ class "card-body px-2 pb-2", style "padding-top" "2.25rem" ]
                    [ Html.h6 [ class "mb-1" ] [ text titulo ]
                    , p [ class "text-body-secondary small mb-0" ] [ text descripcion ]
                    ]
                ]
    in
    div [ class "mb-4" ]
        [ Html.h5 [ class "mb-2" ] [ text "Modalidad" ]
        , div [ class "d-flex gap-3 mt-4" ]
            [ card "partes"
                "bi bi-robot"
                "Clásica"
                "Repartí de manera automática el gasto, con varias modalidades"
                [ Attr.id "deudores-modalidad" ]
            , card "repartija"
                "bi bi-people-fill"
                "Repartija"
                "Repartí el gasto como items de un recibo y realizá una carga colaborativa"
                []
            ]
        ]


viewReceiptBanner : Maybe ReceiptReadingState -> Html Msg
viewReceiptBanner receiptParseState =
    div [ class "card mb-4" ]
        [ div [ class "card-body" ]
            [ Html.h6 [ class "mb-1" ] [ i [ class "bi bi-stars me-1" ] [], text "Leer recibo con IA" ]
            , p [ class "text-body-secondary small mb-2" ]
                [ text "Subí una foto del recibo y cargá los items automáticamente" ]
            , Bs.fileInput
                [ accept "image/*"
                , on "change" (Decode.map ReceiptImageSelected fileDecoder)
                ]
            , case receiptParseState of
                Just ReadingFile ->
                    div [ class "d-flex gap-2 align-items-center mt-3" ]
                        [ Bs.spinner [ Attr.attribute "aria-hidden" "true" ]
                        , Bs.alert Bs.AlertInfo
                            [ style "margin-bottom" "0", style "flex" "1" ]
                            [ text "Leyendo la imagen..." ]
                        ]

                Just ProcessingWithAI ->
                    div [ class "mt-3" ]
                        [ div [ class "d-flex gap-2 align-items-center mb-3" ]
                            [ Bs.spinner [ Attr.attribute "aria-hidden" "true" ]
                            , Bs.alert Bs.AlertInfo
                                [ style "margin-bottom" "0", style "flex" "1" ]
                                [ text "Analizando el recibo con inteligencia artificial..." ]
                            ]
                        , Bs.alert Bs.AlertWarning
                            [ style "margin-bottom" "0" ]
                            [ text "Esto podria tomar varios minutos, no cierres esta ventana" ]
                        ]

                Just (ErrorProcessing errorMsg) ->
                    Bs.alert Bs.AlertDanger
                        [ class "mt-3"
                        , style "margin-bottom" "0"
                        , style "display" "flex"
                        , style "align-items" "center"
                        , style "gap" "0.5rem"
                        ]
                        [ span [ style "flex" "1" ] [ text ("Algo salio mal: " ++ errorMsg) ]
                        , button
                            [ type_ "button"
                            , class "btn-close"
                            , Attr.attribute "aria-label" "Cerrar"
                            , onClick ClearReceiptError
                            ]
                            []
                        ]

                Nothing ->
                    text ""
            ]
        ]


viewPartesForm : GrupoLike g -> String -> Form CustomFormError Pago -> Html Msg
viewPartesForm grupo prefix form =
    let
        incluidos =
            grupo.participantes
                |> List.filter
                    (\participante ->
                        (Form.getFieldAsBool (prefix ++ ".partes." ++ participante.id ++ ".participa") form).value == Just True
                    )

        mode =
            { mostrarPartes =
                (Form.getFieldAsBool (prefix ++ "." ++ mostrarPartesField) form).value == Just True
            , mostrarMontoFijo =
                (Form.getFieldAsBool (prefix ++ "." ++ mostrarMontoFijoField) form).value == Just True
            }
    in
    div [ class "mb-4" ]
        [ div [ class "d-flex flex-wrap align-items-center gap-2 mb-2" ]
            [ Html.h5 [ class "mb-0" ] [ text "Quienes participan" ] ]
        , viewSeleccionarParticipantes
            { titulo = "Quienes participan", id = "deudores-seleccionar" }
            grupo.participantes
            prefix
            form
        , Html.hr [ class "my-4" ] []
        , div [ class "d-flex flex-wrap gap-2 mb-3" ]
            [ viewModoChip (prefix ++ "." ++ mostrarPartesField) mode.mostrarPartes "Partes"
            , viewModoChip (prefix ++ "." ++ mostrarMontoFijoField) mode.mostrarMontoFijo "Monto fijo"
            ]
        , viewPartesTable grupo.participantes prefix mode incluidos (Form.getOutput form |> Maybe.map (\pago -> sumaMontosFijos pago.deudores)) form
        ]


{-| Pill con checkbox interno para activar/desactivar un modo de reparto
("Partes" o "Monto fijo"). El cuadradito refleja el estado igual que un
checkbox; al togglearlo se muestra u oculta la columna correspondiente.
-}
viewModoChip : String -> Bool -> String -> Html Msg
viewModoChip path active label =
    button
        [ type_ "button"
        , class
            ("btn rounded-pill d-inline-flex align-items-center gap-2 "
                ++ (if active then
                        "btn-secondary"

                    else
                        "btn-outline-secondary"
                   )
            )
        , Attr.attribute "aria-pressed"
            (if active then
                "true"

             else
                "false"
            )
        , onClick <| PagoForm <| Form.Input path Form.Checkbox (FormField.Bool (not active))
        ]
        [ i
            [ class
                (if active then
                    "bi bi-check-square-fill"

                 else
                    "bi bi-square"
                )
            ]
            []
        , text label
        ]


{-| Selector de participantes como grupo de pills. En desktop se muestran
directamente; en mobile se esconden detrás de un botón que abre el selector.

A diferencia de la pantalla completa, el selector no es un modal de Bootstrap
(no se pueden anidar) sino el overlay de `viewSelectorOverlay`, que se dibuja
por encima del popup y lo maneja el `Model`.

-}
viewSeleccionarParticipantes : { titulo : String, id : String } -> List Participante -> String -> Form CustomFormError Pago -> Html Msg
viewSeleccionarParticipantes selector participantes prefix form =
    div []
        [ div [ class "d-none d-md-block" ] [ viewPills participantes prefix form ]
        , div [ class "d-md-none" ]
            [ button
                [ type_ "button"
                , class "btn btn-light rounded-pill d-inline-flex align-items-center gap-2"
                , Attr.id selector.id
                , onClick (AbrirSelector { titulo = selector.titulo, prefix = prefix })
                ]
                [ i [ class "bi bi-person-fill" ] [], text "Seleccionar" ]
            ]
        ]


viewPills : List Participante -> String -> Form CustomFormError Pago -> Html Msg
viewPills participantes prefix form =
    div [ class "d-flex flex-wrap gap-2" ]
        (participantes |> List.map (\participante -> viewParticipantePill participante prefix form))


{-| El selector de participantes de mobile, encima del popup. Los z-index son
los mismos que usa el popup para los gráficos, así queda por arriba de su
diálogo y de su backdrop.
-}
viewSelectorOverlay : List Participante -> Model -> Html Msg
viewSelectorOverlay participantes model =
    case model.selectorAbierto of
        Nothing ->
            text ""

        Just { titulo, prefix } ->
            let
                form =
                    formDeLaSeccion model
            in
            div []
                [ div
                    [ class "modal d-block"
                    , style "z-index" "1070"
                    , Attr.tabindex -1
                    , Attr.attribute "aria-modal" "true"
                    , Attr.attribute "role" "dialog"
                    ]
                    [ div [ class "modal-dialog modal-dialog-scrollable modal-fullscreen-sm-down" ]
                        [ div [ class "modal-content" ]
                            [ div [ class "modal-header" ]
                                [ Html.h5 [ class "modal-title" ] [ text titulo ]
                                , button
                                    [ type_ "button"
                                    , class "btn-close"
                                    , Attr.attribute "aria-label" "Cerrar"
                                    , onClick CerrarSelector
                                    ]
                                    []
                                ]
                            , div [ class "modal-body" ] [ viewPills participantes prefix form ]
                            , div [ class "modal-footer" ]
                                [ Bs.btn Bs.Primary
                                    [ class "w-100", onClick CerrarSelector ]
                                    [ text "Listo" ]
                                ]
                            ]
                        ]
                    ]
                , div [ class "modal-backdrop show", style "z-index" "1065" ] []
                ]


{-| El form del paso que se está mostrando, que es de donde el selector tiene
que leer quién participa.
-}
formDeLaSeccion : Model -> Form CustomFormError Pago
formDeLaSeccion model =
    case model.currentSection of
        BasicPagoData ->
            model.pagoBasicoForm

        PagadoresSection ->
            model.pagadoresForm

        DeudoresSection ->
            model.deudoresForm


viewParticipantePill : Participante -> String -> Form CustomFormError Pago -> Html Msg
viewParticipantePill participante prefix form =
    let
        participaField =
            Form.getFieldAsBool (prefix ++ ".partes." ++ participante.id ++ ".participa") form

        participa =
            participaField.value == Just True
    in
    button
        [ type_ "button"
        , class "btn btn-light rounded-pill d-inline-flex align-items-center gap-2"
        , Attr.attribute "aria-pressed"
            (if participa then
                "true"

             else
                "false"
            )
        , onClick <| PagoForm <| Form.Input participaField.path Form.Checkbox (FormField.Bool (not participa))
        ]
        [ i
            [ class
                (if participa then
                    "bi bi-check-square-fill"

                 else
                    "bi bi-square"
                )
            ]
            []
        , text participante.nombre
        ]


{-| Con cuántos decimales dejan tipear los inputs de monto de este form, según
la moneda elegida.
-}
decimalesDelForm : Form CustomFormError Pago -> Int
decimalesDelForm form =
    (Form.getFieldAsString "moneda" form).value
        |> Maybe.andThen Moneda.fromString
        |> Maybe.map escalaDe
        |> Maybe.withDefault 2


{-| Indica si la columna "División" debe mostrarse. Sólo se oculta cuando el
único modo activo es el de monto fijo (no hay nada que repartir por partes).
-}
divisionVisible : ModoPartes -> Bool
divisionVisible mode =
    mode.mostrarPartes || not mode.mostrarMontoFijo


{-| Suma de los montos fijos de una distribución, usada en la fila "Suma". Toma
las `Parte` ya parseadas del `Pago` del form, así que no reparsea strings.
-}
sumaMontosFijos : Distribucion -> Monto
sumaMontosFijos distribucion =
    case distribucion.tipo of
        Api.TipoDistribucionPartes dp ->
            dp.partes
                |> List.filterMap
                    (\parte ->
                        case parte of
                            Api.MontoFijo monto _ ->
                                Just monto

                            Api.PonderadoYMontoFijo monto _ _ ->
                                Just monto

                            Api.Ponderado _ _ ->
                                Nothing
                    )
                |> List.foldl Monto.add Monto.zero

        Api.TipoDistribucionRepartija _ ->
            Monto.zero


{-| Total del listado de una repartija: la suma de los montos de cada ítem (la
cantidad no multiplica, sólo se usa al reclamar). No incluye la propina.
-}
totalItemsRepartija : Distribucion -> Monto
totalItemsRepartija distribucion =
    case distribucion.tipo of
        Api.TipoDistribucionRepartija repartija ->
            repartija.items
                |> List.map .monto
                |> List.foldl Monto.add Monto.zero

        Api.TipoDistribucionPartes _ ->
            Monto.zero


{-| Tabla de partes con columnas dinámicas: "Monto fijo" cuando ese modo está
activo y "División" (contador de partes o "En partes iguales") salvo cuando el
único modo es el de monto fijo. `suma` es el total de montos fijos parseado del
form, mostrado en el pie cuando se editan montos.
-}
viewPartesTable : List Participante -> String -> ModoPartes -> List Participante -> Maybe Monto -> Form CustomFormError Pago -> Html Msg
viewPartesTable participantesDelGrupo prefix mode incluidos suma form =
    let
        headerCells =
            Html.th [ Attr.scope "col" ] [ text "Participante" ]
                :: (if mode.mostrarMontoFijo then
                        [ Html.th [ Attr.scope "col", class "text-end" ] [ text "Monto fijo" ] ]

                    else
                        []
                   )
                ++ (if divisionVisible mode then
                        [ Html.th [ Attr.scope "col", class "text-end" ] [ text "Partes" ] ]

                    else
                        []
                   )
    in
    div [ class "table-responsive" ]
        [ Html.table [ class "table align-middle" ]
            [ Html.thead [] [ Html.tr [] headerCells ]
            , Html.tbody []
                (incluidos |> List.map (\participante -> viewParteRow participantesDelGrupo prefix mode participante form))
            , if mode.mostrarMontoFijo || mode.mostrarPartes then
                let
                    sumaRow =
                        Html.tr [ class "text-body-secondary" ]
                            (Html.td [] [ text "Total" ]
                                :: (if mode.mostrarMontoFijo then
                                        [ Html.td [ class "text-end" ]
                                            [ text ("$ " ++ (suma |> Maybe.map Monto.toString |> Maybe.withDefault "—")) ]
                                        ]

                                    else
                                        []
                                   )
                                ++ (if divisionVisible mode then
                                        [ Html.td [ class "text-end" ]
                                            [ if mode.mostrarPartes then
                                                let
                                                    totalPartes =
                                                        incluidos
                                                            |> List.map
                                                                (\participante ->
                                                                    (Form.getFieldAsString (prefix ++ ".partes." ++ participante.id ++ ".cuota") form).value
                                                                        |> Maybe.andThen String.toInt
                                                                        |> Maybe.withDefault 0
                                                                )
                                                            |> List.sum
                                                in
                                                text (String.fromInt totalPartes ++ " partes")

                                              else
                                                text ""
                                            ]
                                        ]

                                    else
                                        []
                                   )
                            )
                in
                Html.tfoot [] [ sumaRow ]

              else
                text ""
            ]
        ]


viewParteRow : List Participante -> String -> ModoPartes -> Participante -> Form CustomFormError Pago -> Html Msg
viewParteRow participantesDelGrupo prefix mode participante form =
    let
        basePath =
            prefix ++ ".partes." ++ participante.id
    in
    Html.tr []
        (Html.td [ class "fw-bold" ]
            [ div [ class "d-flex align-items-center gap-2" ]
                [ GraficoTorta.viewDot (GraficoTorta.colorParaParticipante participantesDelGrupo participante.id)
                , text participante.nombre
                ]
            ]
            :: (if mode.mostrarMontoFijo then
                    let
                        montoField =
                            Form.getFieldAsString (basePath ++ ".monto") form
                    in
                    [ Html.td [ class "text-end" ]
                        [ div [ class "ms-auto", style "width" "11rem" ]
                            [ Html.map PagoForm <|
                                Bs.montoInput (decimalesDelForm form) montoField [ placeholder "13.000,00", style "text-align" "right" ]
                            ]
                        ]
                    ]

                else
                    []
               )
            ++ (if divisionVisible mode then
                    [ Html.td [ class "text-end" ]
                        [ if mode.mostrarPartes then
                            viewCuotaCounter (basePath ++ ".cuota") form

                          else
                            span [ class "text-body-secondary" ] [ text "En partes iguales" ]
                        ]
                    ]

                else
                    []
               )
        )


viewCuotaCounter : String -> Form CustomFormError Pago -> Html Msg
viewCuotaCounter path form =
    let
        cuotaField =
            Form.getFieldAsString path form

        cuota =
            cuotaField.value
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault 0

        setCuota nuevaCuota =
            PagoForm <| Form.Input cuotaField.path Form.Text (FormField.String (String.fromInt (max 0 nuevaCuota)))
    in
    div [ class "d-flex align-items-center justify-content-end gap-2" ]
        [ button
            [ type_ "button"
            , class "btn btn-dark btn-sm rounded-circle"
            , Attr.attribute "aria-label" "Menos partes"
            , onClick (setCuota (cuota - 1))
            ]
            [ i [ class "bi bi-dash" ] [] ]
        , span [ class "px-1" ] [ text (String.fromInt cuota) ]
        , button
            [ type_ "button"
            , class "btn btn-dark btn-sm rounded-circle"
            , Attr.attribute "aria-label" "Más partes"
            , onClick (setCuota (cuota + 1))
            ]
            [ i [ class "bi bi-plus" ] [] ]
        ]


viewRepartijaLink : ULID -> Form CustomFormError Pago -> Html Msg
viewRepartijaLink grupoId form =
    let
        repartijaId =
            (Form.getFieldAsString "distribucion_deudores.repartija_id" form).value
                |> Maybe.withDefault emptyUlid
    in
    if repartijaId /= emptyUlid && repartijaId /= "" then
        p []
            [ a
                [ class "link-primary"
                , Path.href <| Path.Grupos_GrupoId__Repartijas_RepartijaId_ { grupoId = grupoId, repartijaId = repartijaId }
                , target "_blank"
                ]
                [ text "Ver repartija colaborativa" ]
            ]

    else
        text ""


fileDecoder : Decode.Decoder File
fileDecoder =
    Decode.at [ "target", "files" ] (Decode.list File.decoder)
        |> Decode.andThen
            (\files ->
                case files of
                    [] ->
                        Decode.fail "No files selected"

                    [ file ] ->
                        Decode.succeed file

                    _ ->
                        Decode.fail "Too many files"
            )


viewRepartijaForm : String -> Form CustomFormError Pago -> Html Msg
viewRepartijaForm prefix form =
    let
        montoField =
            Form.getFieldAsString (prefix ++ ".extra") form

        itemsIndexes =
            Form.getListIndexes (prefix ++ ".items") form
    in
    div []
        [ div [ class "mb-4" ]
            [ div [ class "d-flex justify-content-between align-items-center mb-2" ]
                [ Html.h6 [ class "mb-0" ] [ text "Ítems" ]
                , Bs.btn Bs.Secondary
                    [ onClick <| PagoForm <| Form.Append <| prefix ++ ".items"
                    , type_ "button"
                    , class "btn-sm rounded-pill"
                    ]
                    [ i [ class "bi bi-plus me-1" ] [], text "Item" ]
                ]
            , div [ class "table-responsive" ]
                [ Html.table [ class "table align-middle", style "min-width" "40rem" ]
                    [ Html.thead []
                        [ Html.tr []
                            [ Html.th [ Attr.scope "col" ] [ text "Nombre" ]
                            , Html.th [ Attr.scope "col", style "width" "5rem" ] [ text "Cantidad" ]
                            , Html.th [ Attr.scope "col" ] [ text "Monto" ]
                            , Html.th [ Attr.scope "col", style "width" "1%" ] []
                            ]
                        ]
                    , Html.tbody []
                        (List.map (\idx -> viewRepartijaItemForm idx prefix form) itemsIndexes)
                    , Html.tfoot []
                        [ Html.tr [ class "text-body-secondary" ]
                            [ Html.td [] [ text "Total" ]
                            , Html.td [] []
                            , Html.td []
                                [ text
                                    ("$ "
                                        ++ (Form.getOutput form
                                                |> Maybe.map (\pago -> Monto.toString (totalItemsRepartija pago.deudores))
                                                |> Maybe.withDefault "—"
                                           )
                                    )
                                ]
                            , Html.td [ style "width" "1%" ] []
                            ]
                        ]
                    ]
                ]
            ]
        , div [ class "mb-4" ]
            [ Html.h6 [ class "mb-2" ] [ text "Propina" ]
            , Html.map PagoForm <|
                Bs.montoInput (decimalesDelForm form) montoField [ placeholder "1000" ]
            ]
        , Html.hr [ class "my-3" ] []
        , div [ class "mb-4" ]
            [ Html.h6 [ class "mb-2" ] [ text "Distribución de sobras" ]
            , p [ class "text-body-secondary mb-2", style "font-size" "0.875rem" ]
                [ text "Qué hacer con los items que nadie reclamó. Podés cambiar esta opción en cualquier momento." ]
            , let
                distribucionDeSobrasField =
                    Form.getFieldAsString (prefix ++ ".distribucionDeSobras") form

                selectSobras v =
                    PagoForm <| Form.Input distribucionDeSobrasField.path Form.Select (FormField.String v)
              in
              Bs.segmentedButton []
                [ Bs.segmentedButtonItem
                    { active = distribucionDeSobrasField.value == Just "SobrasNoDistribuir"
                    , onSelect = selectSobras "SobrasNoDistribuir"
                    }
                    []
                    [ text "No distribuir" ]
                , Bs.segmentedButtonItem
                    { active = distribucionDeSobrasField.value == Just "SobrasProporcional"
                    , onSelect = selectSobras "SobrasProporcional"
                    }
                    []
                    [ text "Proporcional" ]
                ]
            ]
        ]


viewRepartijaItemForm : Int -> String -> Form CustomFormError Pago -> Html Msg
viewRepartijaItemForm i prefix form =
    let
        nombreField =
            Form.getFieldAsString (prefix ++ ".items." ++ String.fromInt i ++ ".nombre") form

        montoField =
            Form.getFieldAsString (prefix ++ ".items." ++ String.fromInt i ++ ".monto") form

        cantidadField =
            Form.getFieldAsString (prefix ++ ".items." ++ String.fromInt i ++ ".cantidad") form
    in
    Html.tr []
        [ Html.td []
            [ Html.map PagoForm <|
                Bs.textInput nombreField [ placeholder "Birrita" ]
            ]
        , Html.td [ style "width" "5rem" ]
            [ Html.map PagoForm <|
                Bs.textInput cantidadField [ placeholder "4" ]
            ]
        , Html.td []
            [ Html.map PagoForm <|
                Bs.montoInput (decimalesDelForm form) montoField [ placeholder "20.000", style "text-align" "right" ]
            ]
        , Html.td [ style "width" "1%" ]
            [ Bs.btn Bs.Danger
                [ type_ "button"
                , onClick <| PagoForm <| Form.RemoveItem (prefix ++ ".items") i
                , Attr.attribute "aria-label" "Eliminar item"
                ]
                [ Html.i [ class "bi bi-trash" ] [] ]
            ]
        ]


allowedMimeTypesForReceiptUpload : List String
allowedMimeTypesForReceiptUpload =
    [ "image/png"
    , "image/jpeg"
    , "image/webp"
    , "image/gif"
    ]
