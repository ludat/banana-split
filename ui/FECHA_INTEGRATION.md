# Date Field Integration Guide

This guide explains how to integrate the `fecha` (date) field into the pago and grupo forms once the backend has regenerated the Elm types.

## Prerequisites

The following packages have been added to `elm.json`:
- `mercurymedia/elm-datetime-picker` - The date picker component
- `justinmimbs/date` - Date manipulation library
- `justinmimbs/time-extra` - Additional time utilities
- `rtfeldman/elm-iso8601-date-strings` - ISO8601 parsing/formatting

## Backend Changes

The backend has been updated to include `fecha :: ZonedTime` fields in:
- `Pago` (src/core/BananaSplit/Core.hs:63)
- `ShallowPago` (src/core/BananaSplit/Core.hs:73)
- `Grupo` (src/core/BananaSplit/Core.hs:46)
- `ShallowGrupo` (src/core/BananaSplit/Core.hs:54)

The database migration adds `timestamptz` columns with `NOW()` defaults.

## Integration Steps

### Step 1: Run Backend to Generate Types

First, run the backend to regenerate `src/Generated/Api.elm` with the new `fecha` field:

```bash
# In the root directory
cabal run banana-split
```

This will add `fecha: Time.Posix` to the Pago and Grupo types in `Generated/Api.elm`.

### Step 2: Update Pago Validation

In `src/Pages/Grupos/GrupoId_/Pagos/New.elm`, update the validation functions:

```elm
-- Around line 185-193
validatePago : List Participante -> Validation CustomFormError Pago
validatePago participantes =
    V.succeed Pago
        |> V.andMap (V.field "id" validateId)
        |> V.andMap (V.field "monto" Monto.validateMonto)
        |> V.andMap (V.succeed False)
        |> V.andMap (V.field "nombre" (V.string |> V.andThen nonEmpty))
        |> V.andMap (V.field "fecha" validateFecha)  -- ADD THIS LINE
        |> V.andMap (V.field "distribucion_pagadores" <| validateDistribucion participantes)
        |> V.andMap (V.field "distribucion_deudores" <| validateDistribucion participantes)

-- Add this new validation function
validateFecha : Validation CustomFormError Time.Posix
validateFecha =
    V.customValidation V.string
        (\str ->
            case Iso8601.toTime str of
                Ok posix ->
                    Ok posix
                Err _ ->
                    Err (Form.Error.value Form.Error.InvalidString)
        )
```

### Step 3: Add Date Picker to Pago Form

In the same file, add the date picker to the model and update functions:

```elm
-- Add to imports at top
import Utils.DatePicker as DatePicker
import Iso8601
import Time

-- Update the Model type (around line 98-112)
type alias Model =
    { grupoId : String
    , currentPagoId : Maybe ULID
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
    , datePicker : DatePicker.DatePicker  -- ADD THIS LINE
    }

-- Update Msg type (around line 68-82)
type Msg
    = NoOp
    | PagoForm Form.Msg
    | AddedPagoResponse (Result Http.Error Pago)
    | UpdatedPagoResponse (Result Http.Error Pago)
    | SelectSection Section
    | SubmitCurrentSection
    | CheckIfPagoAndGrupoArePresent
    | ResumenPagoUpdated (WebData ResumenPago)
    | ResumenDeudoresUpdated (WebData ResumenPago)
    | ResumenPagadoresUpdated (WebData ResumenPago)
    | ReceiptImageSelected File
    | ReceiptImageBytes File Bytes
    | ReceiptParseResponse (Result Http.Error Api.ReceiptImageResponse)
    | ClearReceiptError
    | DatePickerMsg DatePicker.Msg  -- ADD THIS LINE

-- Update init function (around line 115-147)
init : ULID -> Store -> ( Model, Effect Msg )
init grupoId store =
    let
        defaultDistribucionValues =
            [ Form.setGroup "distribucion_pagadores"
                [ Form.setString "tipo" "monto_equitativo"
                ]
            , Form.setGroup "distribucion_deudores"
                [ Form.setString "tipo" "monto_equitativo"
                ]
            ]
    in
    ( { grupoId = grupoId
      , currentPagoId = Nothing
      , currentSection = BasicPagoData
      , pagoBasicoForm = Form.initial defaultDistribucionValues (validatePagoInSection BasicPagoData [])
      , deudoresForm = Form.initial defaultDistribucionValues (validatePagoInSection DeudoresSection [])
      , resumenDeudores = NotAsked
      , pagadoresForm = Form.initial defaultDistribucionValues (validatePagoInSection PagadoresSection [])
      , resumenPagadores = NotAsked
      , pagoForm = Form.initial defaultDistribucionValues (validatePago [])
      , resumenPago = NotAsked
      , receiptParseState = Nothing
      , storedClaims = Nothing
      , hasUnsavedChanges = False
      , datePicker = DatePicker.init Nothing  -- ADD THIS LINE
      }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Effect.getCurrentUser grupoId
        ]
    )
        |> andThenSendWarningOnExit

-- Add to update function
update : Store -> Maybe ULID -> Msg -> Model -> ( Model, Effect Msg )
update store userId msg model =
    case msg of
        -- ... existing cases ...

        DatePickerMsg datePickerMsg ->
            let
                ( newDatePicker, datePickerCmd ) =
                    DatePicker.update datePickerMsg model.datePicker

                -- Update the form when a date is selected
                updatedModel =
                    case DatePicker.getSelectedDate newDatePicker of
                        Just posix ->
                            let
                                isoString = Iso8601.fromTime posix
                                formMsg = Form.Input "fecha" Form.Text (FormField.String isoString)
                            in
                            { model
                                | datePicker = newDatePicker
                                , pagoForm = Form.update (validatePago participantes) formMsg model.pagoForm
                                , pagoBasicoForm = Form.update (validatePagoInSection BasicPagoData participantes) formMsg model.pagoBasicoForm
                            }

                        Nothing ->
                            { model | datePicker = newDatePicker }
            in
            ( updatedModel
            , Effect.sendCmd (Cmd.map DatePickerMsg datePickerCmd)
            )
```

### Step 4: Add Date Picker to View

In `pagoForm` function (around line 1126), add the date picker:

```elm
pagoForm : List Participante -> Form CustomFormError Pago -> DatePicker.DatePicker -> Html Msg
pagoForm participantes form datePicker =
    let
        nombreField =
            Form.getFieldAsString "nombre" form

        montoField =
            Form.getFieldAsString "monto" form

        fechaField =
            Form.getFieldAsString "fecha" form
    in
    Html.form [ class "mb-6", onSubmit <| SubmitCurrentSection ]
        [ div [ class "field mb-5" ]
            [ div [ class "field mb-5" ]
                [ label [ class "label" ]
                    [ text "Nombre" ]
                , div [ class "control" ]
                    [ Html.map PagoForm <|
                        FormInput.textInput nombreField
                            [ class "input"
                            , type_ "text"
                            , placeholder "Pago de deudas"
                            , classList [ ( "is-danger", hasErrorField nombreField ) ]
                            , id "pago-nombre"
                            ]
                    , errorForField nombreField
                    ]
                ]
            , label [ class "label" ]
                [ text "Monto" ]
            , div [ class "control" ]
                [ Html.map PagoForm <|
                    FormInput.textInput montoField
                        [ class "input"
                        , type_ "text"
                        , placeholder "2000"
                        , classList [ ( "is-danger", hasErrorField montoField ) ]
                        ]
                , errorForField montoField
                ]
            -- ADD THIS SECTION
            , div [ class "field mb-5" ]
                [ label [ class "label" ]
                    [ text "Fecha" ]
                , div [ class "control" ]
                    [ Html.map DatePickerMsg <|
                        DatePicker.view datePicker
                            SingleDatePicker.defaultSettings
                    ]
                , errorForField fechaField
                ]
            ]
        , button [ class "button is-primary", disabled (Form.getOutput form == Nothing) ] [ text "Siguiente seccion" ]
        ]
```

### Step 5: Update View Calls

Update all calls to `pagoForm` to pass the date picker:

```elm
-- In the view function (around line 942)
BasicPagoData ->
    div []
        [ div [ class "content mb-4" ]
            [ p [] [ text "Ingresá la información básica del pago: un nombre descriptivo y el monto total." ]
            ]
        , pagoForm grupo.participantes model.pagoBasicoForm model.datePicker  -- ADD model.datePicker
        ]
```

### Step 6: Load Date When Editing

In the `CheckIfPagoAndGrupoArePresent` case (around line 583), initialize the date picker with the pago's fecha:

```elm
( Success grupo, Success pago ) ->
    let
        initialFormValues =
            [ Form.setString "id" pagoId
            , Form.setString "nombre" pago.nombre
            , Form.setString "monto" (Monto.toString pago.monto)
            , Form.setString "fecha" (Iso8601.fromTime pago.fecha)  -- ADD THIS LINE
            , Form.setGroup "distribucion_pagadores" (distribucionToForm pago.pagadores)
            , Form.setGroup "distribucion_deudores" (distribucionToForm pago.deudores)
            ]

        claimsToStore =
            { pagadores = extractClaimsFromDistribucion pago.pagadores
            , deudores = extractClaimsFromDistribucion pago.deudores
            }
    in
    ( { model
        | pagoForm = Form.initial initialFormValues (validatePago grupo.participantes)
        , pagadoresForm = Form.initial initialFormValues (validatePagoInSection PagadoresSection grupo.participantes)
        , deudoresForm = Form.initial initialFormValues (validatePagoInSection DeudoresSection grupo.participantes)
        , pagoBasicoForm = Form.initial initialFormValues (validatePagoInSection BasicPagoData grupo.participantes)
        , resumenPago = Loading
        , storedClaims = Just claimsToStore
        , hasUnsavedChanges = False
        , datePicker = DatePicker.init (Just pago.fecha)  -- ADD THIS LINE
      }
    , Effect.none
    )
        |> andThenUpdateResumenesFromForms model
        |> andThenSendWarningOnExit
```

## Date Picker Settings

You can customize the date picker by modifying `SingleDatePicker.defaultSettings`:

```elm
import DatePicker.SingleDatePicker as SingleDatePicker

customSettings : SingleDatePicker.Settings
customSettings =
    { defaultSettings
        | placeholder = "Seleccionar fecha"
        , dateStringFn = \date -> Date.format "dd/MM/y" date
        -- Disable future dates
        , isDisabled = \date -> Date.compare date (Date.fromPosix zone Time.now) == GT
    }
```

## Testing

After making these changes:

1. Run the backend to ensure the Elm types are generated correctly
2. Test creating a new pago - the date picker should appear and default to today
3. Test editing an existing pago - the date picker should show the pago's fecha
4. Verify that the fecha is saved correctly to the database

## Notes

- The `fecha` field will be automatically set to `NOW()` by the database for existing pagos and grupos
- The date picker uses `Time.Posix` which represents timestamps
- The backend expects ISO8601 formatted strings for date fields
- The `ZonedTime` type in Haskell preserves timezone information
