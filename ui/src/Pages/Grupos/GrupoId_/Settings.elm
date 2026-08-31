module Pages.Grupos.GrupoId_.Settings exposing (EstadoDeLaMoneda, Model, Msg, page)

import Components.Bootstrap as Bs
import Effect exposing (Effect)
import Form exposing (Form, Msg(..))
import Form.Error as FormError
import Form.Field
import Form.Init as Form
import Form.Validate as V exposing (Validation)
import Generated.Api as Api exposing (Moneda, Monto, ResumenGrupo, ShallowGrupo, TasaDeCambio, ULID, UpdateGrupoParams, User)
import Html exposing (Html, a, button, div, i, input, label, option, select, span, text)
import Html.Attributes as Attr exposing (class, classList, disabled, for, id, selected, type_, value)
import Html.Events exposing (on, onClick, onInput, onSubmit)
import Http
import Json.Decode
import Layouts
import Models.Grupo exposing (ownedParticipante)
import Models.Moneda as Moneda
import Models.Monto as Monto
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Utils.Form exposing (CustomFormError, errorForField, hasErrorField)
import Utils.Http exposing (viewHttpError)
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts
import Utils.Ulid
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init shared.store route.params.grupoId
        , update = update shared.store
        , subscriptions = subscriptions
        , view = view shared.origin shared.currentUser shared.store
        }
        |> Page.withLayout (\_ -> Layouts.Default_Grupo {})


type alias Model =
    { grupoId : String
    , ajustesForm : Maybe (Form CustomFormError UpdateGrupoParams)
    , moneda : EstadoDeLaMoneda
    , tasasForm : Maybe (Form CustomFormError (List (Maybe TasaDeCambio)))
    }


{-| La moneda que muestra el select: la que ya está guardada en el grupo, o la
que el usuario eligió y todavía no confirmó el backend.
-}
type EstadoDeLaMoneda
    = MonedaDelGrupo
    | EsperandoConfirmacion Moneda


esperandoConfirmacion : EstadoDeLaMoneda -> Bool
esperandoConfirmacion estado =
    case estado of
        MonedaDelGrupo ->
            False

        EsperandoConfirmacion _ ->
            True


type Msg
    = FreezeGrupo
    | FreezeGrupoResponse (Result Http.Error ShallowGrupo)
    | UnfreezeGrupo
    | UnfreezeGrupoResponse (Result Http.Error ShallowGrupo)
    | AjustesForm Form.Msg
    | UpdateGrupoResponse (Result Http.Error ShallowGrupo)
    | EditarAjustes
    | CancelarEdicionAjustes
    | ShareEmailAddress String
    | SeleccionarMoneda Moneda
    | EditarTasas
    | CancelarEdicionTasas
    | TasasForm Form.Msg
    | TasasGuardadas (Result Http.Error (List TasaDeCambio))
    | MonedaGuardada (Result Http.Error ShallowGrupo)
    | VaciarTasa Int


init : Store -> ULID -> ( Model, Effect Msg )
init store grupoId =
    ( { grupoId = grupoId
      , ajustesForm = Nothing
      , moneda = MonedaDelGrupo
      , tasasForm = Nothing
      }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Store.ensureResumen grupoId store
        , Effect.getCurrentUser grupoId
        , Effect.setUnsavedChangesWarning False
        ]
    )


validateUpdateGrupoParams : Validation CustomFormError UpdateGrupoParams
validateUpdateGrupoParams =
    V.succeed UpdateGrupoParams
        |> V.andMap (V.field "nombre" (V.string |> V.andThen V.nonEmpty))
        |> V.andMap (V.field "moneda" Moneda.validate)


seedAjustesForm : ShallowGrupo -> Form CustomFormError UpdateGrupoParams
seedAjustesForm grupo =
    Form.initial
        [ Form.setString "nombre" grupo.nombre
        , Form.setString "moneda" (Moneda.toString grupo.monedaPorDefecto)
        ]
        validateUpdateGrupoParams


update : Store -> Msg -> Model -> ( Model, Effect Msg )
update store msg model =
    case msg of
        FreezeGrupo ->
            ( model
            , Effect.sendCmd <|
                Api.postGrupoByIdFreeze
                    model.grupoId
                    FreezeGrupoResponse
            )

        FreezeGrupoResponse (Ok _) ->
            ( model
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshGrupo model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Grupo congelado"
                ]
            )

        FreezeGrupoResponse (Err _) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "No se pudo congelar el grupo"
            )

        UnfreezeGrupo ->
            ( model
            , Effect.sendCmd <|
                Api.deleteGrupoByIdFreeze
                    model.grupoId
                    UnfreezeGrupoResponse
            )

        UnfreezeGrupoResponse (Ok _) ->
            ( model
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshGrupo model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Grupo descongelado"
                ]
            )

        UnfreezeGrupoResponse (Err _) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "No se pudo descongelar el grupo"
            )

        EditarAjustes ->
            ( { model
                | ajustesForm =
                    Store.getGrupo model.grupoId store
                        |> RemoteData.toMaybe
                        |> Maybe.map seedAjustesForm
              }
            , Effect.none
            )

        CancelarEdicionAjustes ->
            ( { model | ajustesForm = Nothing }
            , Effect.none
            )

        AjustesForm Form.Submit ->
            case model.ajustesForm of
                Nothing ->
                    ( model, Effect.none )

                Just form ->
                    let
                        enviado =
                            { model
                                | ajustesForm =
                                    Just (Form.update validateUpdateGrupoParams Form.Submit form)
                            }
                    in
                    case Form.getOutput form of
                        Just params ->
                            ( enviado
                            , Effect.sendCmd <|
                                Api.putGrupoById model.grupoId params UpdateGrupoResponse
                            )

                        Nothing ->
                            ( enviado, Effect.none )

        AjustesForm formMsg ->
            ( { model
                | ajustesForm =
                    model.ajustesForm
                        |> Maybe.map (Form.update validateUpdateGrupoParams formMsg)
              }
            , Effect.none
            )

        UpdateGrupoResponse (Ok grupo) ->
            ( { model | ajustesForm = Nothing }
            , Effect.batch
                [ Store.setGrupo model.grupoId grupo
                , Store.refreshResumen model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Grupo actualizado"
                ]
            )

        UpdateGrupoResponse (Err _) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "No se pudo actualizar el grupo"
            )

        ShareEmailAddress address ->
            ( model
            , Effect.share { title = "Cargar gastos por email", url = address }
            )

        SeleccionarMoneda moneda ->
            case Store.getGrupo model.grupoId store |> RemoteData.toMaybe of
                Nothing ->
                    ( model, Effect.none )

                Just grupo ->
                    ( { model | moneda = EsperandoConfirmacion moneda }
                    , Effect.sendCmd <|
                        Api.putGrupoById model.grupoId
                            { nombre = grupo.nombre, monedaPorDefecto = moneda }
                            MonedaGuardada
                    )

        EditarTasas ->
            if esperandoConfirmacion model.moneda then
                ( model, Effect.none )

            else
                ( { model | tasasForm = seedTasasDesdeStore store model }
                , Effect.none
                )

        CancelarEdicionTasas ->
            ( { model | tasasForm = Nothing }
            , Effect.none
            )

        TasasForm Form.Submit ->
            case ( model.tasasForm, Store.getGrupo model.grupoId store |> RemoteData.toMaybe ) of
                ( Just form, Just grupo ) ->
                    let
                        enviado =
                            { model | tasasForm = Just (Form.update validateTasas Form.Submit form) }
                    in
                    case tasasDelForm form of
                        Just tasas ->
                            ( enviado
                            , Effect.sendCmd <|
                                Api.putGrupoByIdTasasdecambioByMoneda model.grupoId grupo.monedaPorDefecto tasas TasasGuardadas
                            )

                        Nothing ->
                            ( enviado, Effect.none )

                _ ->
                    ( model, Effect.none )

        TasasForm formMsg ->
            ( { model
                | tasasForm =
                    model.tasasForm
                        |> Maybe.map (Form.update validateTasas formMsg)
              }
            , Effect.none
            )

        TasasGuardadas (Ok _) ->
            ( { model | tasasForm = Nothing }
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Tasas de cambio guardadas"
                ]
            )

        TasasGuardadas (Err _) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "No se pudieron guardar las tasas de cambio"
            )

        MonedaGuardada (Ok grupo) ->
            ( { model | moneda = MonedaDelGrupo }
            , Effect.batch
                [ Store.setGrupo model.grupoId grupo
                , Store.refreshResumen model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Moneda por defecto actualizada"
                ]
            )

        MonedaGuardada (Err _) ->
            ( { model | moneda = MonedaDelGrupo }
            , Effect.batch
                [ Store.refreshGrupo model.grupoId
                , Toasts.pushToast Toasts.ToastDanger "No se pudo cambiar la moneda por defecto"
                ]
            )

        VaciarTasa indice ->
            let
                prefix =
                    "tasas." ++ String.fromInt indice

                vaciarFila form =
                    [ ( ".unMonto", "1" ), ( ".otroMonto", "" ) ]
                        |> List.foldl
                            (\( campo, valor ) acc ->
                                Form.update validateTasas
                                    (Input (prefix ++ campo) Form.Text (Form.Field.String valor))
                                    acc
                            )
                            form
            in
            ( { model | tasasForm = model.tasasForm |> Maybe.map vaciarFila }
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : String -> WebData User -> Store -> Model -> View Msg
view origin currentUser store model =
    case store |> Store.getGrupo model.grupoId of
        NotAsked ->
            { title = "Loading..."
            , body = []
            }

        Loading ->
            { title = "Cargando"
            , body =
                [ div [ class "container py-4 text-muted" ] [ text "Cargando..." ]
                ]
            }

        Failure e ->
            { title = "Fallo"
            , body =
                [ div [ class "container py-4" ] [ viewHttpError e ]
                ]
            }

        Success grupo ->
            { title = grupo.nombre ++ " - Configuración"
            , body =
                [ div [ class "container py-4" ]
                    [ div [ class "row justify-content-center" ]
                        [ div [ class "col-lg-8" ]
                            [ viewAjustesSection grupo model.ajustesForm
                            , viewTasasSection grupo (Store.getResumen model.grupoId store) model
                            , viewEmailSection origin currentUser grupo
                            , viewFreezeSection grupo
                            ]
                        ]
                    ]
                ]
            }


viewAjustesSection : ShallowGrupo -> Maybe (Form CustomFormError UpdateGrupoParams) -> Html Msg
viewAjustesSection grupo edicion =
    div [ class "card mb-4" ]
        [ div [ class "card-header" ] [ text "Ajustes generales" ]
        , div [ class "card-body" ]
            [ case edicion of
                Just form ->
                    viewAjustesEditando grupo form

                Nothing ->
                    viewAjustesGuardados grupo
            ]
        ]


viewAjustesGuardados : ShallowGrupo -> Html Msg
viewAjustesGuardados grupo =
    div []
        [ div [ class "list-group mb-3" ]
            [ viewDatoGuardado "Nombre" grupo.nombre ]
        , Bs.btn Bs.Primary
            [ onClick EditarAjustes ]
            [ text "Editar" ]
        ]


viewDatoGuardado : String -> String -> Html Msg
viewDatoGuardado etiqueta valor =
    div [ class "list-group-item d-flex justify-content-between align-items-center gap-2" ]
        [ span [ class "text-muted" ] [ text etiqueta ]
        , span [] [ text valor ]
        ]


viewAjustesEditando : ShallowGrupo -> Form CustomFormError UpdateGrupoParams -> Html Msg
viewAjustesEditando grupo form =
    let
        dirty =
            Form.getOutput form /= Just { nombre = grupo.nombre, monedaPorDefecto = grupo.monedaPorDefecto }
    in
    Html.form
        [ onSubmit (AjustesForm Form.Submit) ]
        [ Html.map AjustesForm <|
            viewTextFormItem "Nombre" True (Form.getFieldAsString "nombre" form)
        , div [ class "d-flex gap-2" ]
            [ Bs.btn Bs.Primary
                [ disabled (not dirty || Form.getOutput form == Nothing)
                , onClick (AjustesForm Form.Submit)
                ]
                [ text "Guardar" ]
            , button
                [ type_ "button"
                , class "btn btn-outline-secondary"
                , onClick CancelarEdicionAjustes
                ]
                [ text "Cancelar" ]
            ]
        ]


type alias ContextoDeTasas =
    { monedaPorDefecto : Moneda
    , conPagos : List Moneda
    , guardadas : List TasaDeCambio
    }


contextoDeTasas : Store -> Model -> Maybe ContextoDeTasas
contextoDeTasas store model =
    Maybe.map2 armarContexto
        (Store.getGrupo model.grupoId store |> RemoteData.toMaybe)
        (Store.getResumen model.grupoId store |> RemoteData.toMaybe)


armarContexto : ShallowGrupo -> ResumenGrupo -> ContextoDeTasas
armarContexto grupo resumen =
    { monedaPorDefecto = grupo.monedaPorDefecto
    , conPagos = resumen.netos |> List.map Tuple.first
    , guardadas = resumen.tasasDeCambio
    }


monedasDelGrupo : ContextoDeTasas -> List Moneda
monedasDelGrupo contexto =
    contexto.monedaPorDefecto
        :: contexto.conPagos
        ++ (contexto.guardadas |> List.concatMap (\tasa -> [ tasa.unaMoneda, tasa.otraMoneda ]))
        |> List.foldl
            (\moneda acc ->
                if List.member moneda acc then
                    acc

                else
                    acc ++ [ moneda ]
            )
            []


monedasAConvertir : ContextoDeTasas -> List Moneda
monedasAConvertir contexto =
    monedasDelGrupo contexto
        |> List.filter (\moneda -> moneda /= contexto.monedaPorDefecto)


mismoPar : Moneda -> Moneda -> TasaDeCambio -> Bool
mismoPar una otra tasa =
    (tasa.unaMoneda == una && tasa.otraMoneda == otra)
        || (tasa.unaMoneda == otra && tasa.otraMoneda == una)


tasaDeMoneda : ContextoDeTasas -> Moneda -> Maybe TasaDeCambio
tasaDeMoneda contexto moneda =
    contexto.guardadas
        |> List.filter (mismoPar moneda contexto.monedaPorDefecto)
        |> List.head
        |> Maybe.map
            (\tasa ->
                if tasa.unaMoneda == moneda then
                    tasa

                else
                    { tasa
                        | unaMoneda = tasa.otraMoneda
                        , otraMoneda = tasa.unaMoneda
                        , unMonto = tasa.otroMonto
                        , otroMonto = tasa.unMonto
                    }
            )


faltaLaTasa : ContextoDeTasas -> Moneda -> Bool
faltaLaTasa contexto moneda =
    List.member moneda contexto.conPagos
        && tasaDeMoneda contexto moneda
        == Nothing



-- Formulario de tasas


validateTasas : Validation CustomFormError (List (Maybe TasaDeCambio))
validateTasas =
    V.field "tasas" (V.list validateTasa)


validateTasa : Validation CustomFormError (Maybe TasaDeCambio)
validateTasa =
    V.succeed
        (\tasaId unaMoneda otraMoneda unMonto otroMonto ->
            Maybe.map2
                (\desde hasta ->
                    { id = tasaId
                    , unaMoneda = unaMoneda
                    , otraMoneda = otraMoneda
                    , unMonto = desde
                    , otroMonto = hasta
                    }
                )
                unMonto
                otroMonto
        )
        |> V.andMap (V.field "id" V.string)
        |> V.andMap (V.field "unaMoneda" Moneda.validate)
        |> V.andMap (V.field "otraMoneda" Moneda.validate)
        |> V.andMap (V.field "unMonto" validateMontoDeTasa)
        |> V.andMap (V.field "otroMonto" validateMontoDeTasa)


validateMontoDeTasa : Validation CustomFormError (Maybe Monto)
validateMontoDeTasa =
    V.oneOf
        [ V.emptyString |> V.map (always Nothing)
        , Monto.validateMonto
            |> V.andThen
                (\monto ->
                    if monto.valor > 0 then
                        V.succeed (Just monto)

                    else
                        V.fail <| FormError.value <| FormError.GreaterFloatThan 0
                )
        ]


type alias FilaDeTasa =
    { id : ULID
    , unaMoneda : Moneda
    , otraMoneda : Moneda
    , unMonto : String
    , otroMonto : String
    }


seedTasasForm : ContextoDeTasas -> Form CustomFormError (List (Maybe TasaDeCambio))
seedTasasForm contexto =
    Form.initial
        [ Form.setList "tasas"
            (monedasAConvertir contexto
                |> List.map (filaDeMoneda contexto >> filaAFormGroup)
            )
        ]
        validateTasas


seedTasasDesdeStore : Store -> Model -> Maybe (Form CustomFormError (List (Maybe TasaDeCambio)))
seedTasasDesdeStore store model =
    contextoDeTasas store model |> Maybe.map seedTasasForm


filaDeMoneda : ContextoDeTasas -> Moneda -> FilaDeTasa
filaDeMoneda contexto moneda =
    case tasaDeMoneda contexto moneda of
        Just tasa ->
            { id = tasa.id
            , unaMoneda = tasa.unaMoneda
            , otraMoneda = tasa.otraMoneda
            , unMonto = Monto.toRawString tasa.unMonto
            , otroMonto = Monto.toRawString tasa.otroMonto
            }

        Nothing ->
            { id = Utils.Ulid.emptyUlid
            , unaMoneda = moneda
            , otraMoneda = contexto.monedaPorDefecto
            , unMonto = "1"
            , otroMonto = ""
            }


filaAFormGroup : FilaDeTasa -> Form.Field.Field
filaAFormGroup fila =
    Form.Field.group
        [ Form.setString "id" fila.id
        , Form.setString "unaMoneda" (Moneda.toString fila.unaMoneda)
        , Form.setString "otraMoneda" (Moneda.toString fila.otraMoneda)
        , Form.setString "unMonto" fila.unMonto
        , Form.setString "otroMonto" fila.otroMonto
        ]


viewTasasSection : ShallowGrupo -> WebData ResumenGrupo -> Model -> Html Msg
viewTasasSection grupo resumen model =
    case resumen of
        Success resumenGrupo ->
            let
                contexto =
                    armarContexto grupo resumenGrupo
            in
            div [ class "card mb-4" ]
                [ div [ class "card-header" ] [ text "Monedas" ]
                , div [ class "card-body" ]
                    [ viewMonedaPorDefecto contexto model
                    , viewTasas contexto model
                    ]
                ]

        _ ->
            text ""


viewMonedaPorDefecto : ContextoDeTasas -> Model -> Html Msg
viewMonedaPorDefecto contexto model =
    let
        editandoTasas =
            model.tasasForm /= Nothing

        elegida =
            case model.moneda of
                MonedaDelGrupo ->
                    contexto.monedaPorDefecto

                EsperandoConfirmacion moneda ->
                    moneda
    in
    div []
        [ label [ for "moneda-por-defecto", class "form-label mb-1" ]
            [ text "Moneda por defecto" ]
        , div [ class "d-flex align-items-center gap-2" ]
            [ select
                [ id "moneda-por-defecto"
                , class "form-select"
                , disabled (editandoTasas || esperandoConfirmacion model.moneda)
                , on "change"
                    (Json.Decode.at [ "target", "value" ] Api.jsonDecMoneda
                        |> Json.Decode.map SeleccionarMoneda
                    )
                ]
                (Moneda.todas
                    |> List.map
                        (\moneda ->
                            option
                                [ value (Moneda.toString moneda)
                                , selected (moneda == elegida)
                                ]
                                [ text (Moneda.nombre moneda) ]
                        )
                )
            , case model.moneda of
                EsperandoConfirmacion _ ->
                    Bs.spinner
                        [ Attr.style "width" "1.25rem"
                        , Attr.style "height" "1.25rem"
                        , Attr.style "border-width" "0.2em"
                        , Attr.style "flex" "0 0 auto"
                        , Attr.attribute "aria-hidden" "true"
                        ]

                MonedaDelGrupo ->
                    text ""
            ]
        , div [ class "text-muted small mt-1" ]
            [ text <|
                case ( model.moneda, editandoTasas ) of
                    ( EsperandoConfirmacion _, _ ) ->
                        "Guardando la moneda por defecto..."

                    ( MonedaDelGrupo, True ) ->
                        "Guardá o cancelá las tasas antes de cambiar la moneda."

                    ( MonedaDelGrupo, False ) ->
                        "Las deudas de todo el grupo se juntan en esta moneda. Cambiarla se guarda al toque."
            ]
        ]


viewTasas : ContextoDeTasas -> Model -> Html Msg
viewTasas contexto model =
    if List.isEmpty (monedasAConvertir contexto) then
        text ""

    else
        div []
            [ Html.hr [ class "my-4" ] []
            , div [ class "form-label mb-0" ] [ text "Tasas de cambio" ]
            , div [ class "text-muted small mb-3" ]
                [ text <|
                    case model.tasasForm of
                        Just _ ->
                            "Las tasas que dejes vacías se borran al guardar."

                        Nothing ->
                            "Con estas convertimos las demás monedas del grupo."
                ]
            , case model.tasasForm of
                Just form ->
                    viewTasasEditando form

                Nothing ->
                    viewTasasGuardadas contexto (esperandoConfirmacion model.moneda)
            ]


viewTasasGuardadas : ContextoDeTasas -> Bool -> Html Msg
viewTasasGuardadas contexto guardandoMoneda =
    div []
        [ div [ class "list-group mb-3" ]
            (monedasAConvertir contexto |> List.map (viewTasaGuardada contexto))
        , button
            [ type_ "button"
            , class "btn btn-sm btn-outline-secondary"
            , disabled guardandoMoneda
            , onClick EditarTasas
            ]
            [ i [ class "bi bi-pencil me-1" ] []
            , text "Editar tasas"
            ]
        ]


viewTasaGuardada : ContextoDeTasas -> Moneda -> Html Msg
viewTasaGuardada contexto moneda =
    div [ class "list-group-item d-flex justify-content-between align-items-center gap-2" ]
        [ span [ class "text-muted" ] [ text (Moneda.nombre moneda) ]
        , case tasaDeMoneda contexto moneda of
            Just tasa ->
                span [] [ text (tasaEnTexto tasa) ]

            Nothing ->
                if faltaLaTasa contexto moneda then
                    span [ class "text-warning-emphasis" ]
                        [ i [ class "bi bi-exclamation-triangle-fill me-1" ] []
                        , text "Sin tasa"
                        ]

                else
                    span [ class "text-muted" ] [ text "Sin tasa" ]
        ]


tasaEnTexto : TasaDeCambio -> String
tasaEnTexto tasa =
    Monto.toString tasa.unMonto
        ++ " "
        ++ Moneda.simboloUnico tasa.unaMoneda
        ++ " = "
        ++ Monto.toString tasa.otroMonto
        ++ " "
        ++ Moneda.simboloUnico tasa.otraMoneda


viewTasasEditando : Form CustomFormError (List (Maybe TasaDeCambio)) -> Html Msg
viewTasasEditando form =
    div []
        [ div []
            (Form.getListIndexes "tasas" form
                |> List.map (viewTasaRow form)
            )
        , div [ class "d-flex gap-2 mt-3" ]
            [ Bs.btn Bs.Primary
                [ disabled (tasasDelForm form == Nothing)
                , onClick (TasasForm Form.Submit)
                ]
                [ text "Guardar tasas" ]
            , button
                [ type_ "button"
                , class "btn btn-outline-secondary"
                , onClick CancelarEdicionTasas
                ]
                [ text "Cancelar" ]
            ]
        ]


viewTasaRow : Form CustomFormError (List (Maybe TasaDeCambio)) -> Int -> Html Msg
viewTasaRow form indice =
    let
        prefix =
            "tasas." ++ String.fromInt indice

        monedaDe campo =
            valorDeCampo form (prefix ++ "." ++ campo)
                |> Maybe.withDefault ""
    in
    div [ class "row g-2 align-items-start mb-2" ]
        [ div [ class "col" ]
            [ Html.map TasasForm <| viewLadoDeTasa form prefix "unMonto" (monedaDe "unaMoneda") ]
        , div [ class "col-auto fw-bold pt-2" ] [ text "=" ]
        , div [ class "col" ]
            [ Html.map TasasForm <| viewLadoDeTasa form prefix "otroMonto" (monedaDe "otraMoneda") ]
        , div [ class "col-auto" ]
            [ button
                [ type_ "button"
                , class "btn btn-sm btn-outline-secondary"
                , Attr.title "Vaciar esta tasa"
                , onClick (VaciarTasa indice)
                ]
                [ i [ class "bi bi-x-lg" ] [] ]
            ]
        ]


viewLadoDeTasa : Form CustomFormError (List (Maybe TasaDeCambio)) -> String -> String -> String -> Html Form.Msg
viewLadoDeTasa form prefix campoMonto codigoMoneda =
    let
        field =
            Form.getFieldAsString (prefix ++ "." ++ campoMonto) form
    in
    div []
        [ div [ class "d-flex align-items-center gap-2" ]
            [ div [ class "flex-grow-1" ] [ Bs.montoInput field [] ]
            , span [ class "text-muted text-nowrap" ] [ text codigoMoneda ]
            ]
        , if hasErrorField field then
            div [ class "invalid-feedback d-block" ] [ errorForField field ]

          else
            text ""
        ]


tasasDelForm : Form CustomFormError (List (Maybe TasaDeCambio)) -> Maybe (List TasaDeCambio)
tasasDelForm form =
    Form.getOutput form |> Maybe.map (List.filterMap identity)


valorDeCampo : Form CustomFormError a -> String -> Maybe String
valorDeCampo form path =
    Form.getFieldAsString path form |> .value


{-| The per-grupo inbound-email address. Sending an email here (from the
account's own address) creates a pago in this grupo. The email domain matches
the app's own domain, so we derive it from `origin`.
-}
grupoEmailAddress : String -> ULID -> String
grupoEmailAddress origin grupoId =
    "gasto+" ++ grupoId ++ "@" ++ emailDomain origin


emailDomain : String -> String
emailDomain origin =
    origin
        |> String.split "//"
        |> List.drop 1
        |> List.head
        |> Maybe.withDefault origin


viewEmailSection : String -> WebData User -> ShallowGrupo -> Html Msg
viewEmailSection origin currentUser grupo =
    let
        address =
            grupoEmailAddress origin grupo.id

        -- The webhook resolves the sender to a user account and then requires
        -- that user to own a participante in this grupo, so the address is only
        -- useful to someone logged in who has claimed their participante here.
        canUseEmail =
            case currentUser of
                Success user ->
                    ownedParticipante user.id grupo /= Nothing

                _ ->
                    False
    in
    div [ class "card mb-4" ]
        [ div [ class "card-header d-flex align-items-center gap-2" ]
            [ text "Cargar gastos por email"
            , if canUseEmail then
                text ""

              else
                span [ class "badge text-bg-secondary" ]
                    [ i [ class "bi bi-lock-fill me-1" ] [], text "Solo para miembros" ]
            ]
        , div [ class "card-body" ]
            [ div [ class "mb-3 text-muted" ]
                [ text "Reenviá a esta dirección los mails de tus compras —tickets, pedidos, confirmaciones de pago— o describí un gasto vos mismo, y lo cargamos como gasto automáticamente. Mandalo "
                , Html.strong [] [ text "desde la dirección de tu cuenta" ]
                , text "."
                ]
            , div
                [ class "input-group"
                , classList [ ( "opacity-50", not canUseEmail ) ]
                ]
                [ input
                    [ type_ "text"
                    , class "form-control"
                    , disabled True
                    , value address
                    ]
                    []
                , button
                    [ type_ "button"
                    , class "btn btn-outline-secondary"
                    , disabled (not canUseEmail)
                    , onClick (ShareEmailAddress address)
                    ]
                    [ text "Copiar" ]
                ]
            , if canUseEmail then
                text ""

              else
                div [ class "alert alert-secondary d-flex align-items-start gap-2 mt-3 mb-0" ]
                    [ i [ class "bi bi-lock-fill mt-1" ] []
                    , span []
                        (case currentUser of
                            Success _ ->
                                [ text "Reclamá tu participante en este grupo para poder cargar gastos por email." ]

                            _ ->
                                [ text "Cargá gastos sin abrir la app: "
                                , a [ Path.href Path.Login ] [ text "iniciá sesión o registrate" ]
                                , text " y reclamá tu participante en el grupo."
                                ]
                        )
                    ]
            ]
        ]


viewFreezeSection : ShallowGrupo -> Html Msg
viewFreezeSection grupo =
    div [ class "card" ]
        [ div [ class "card-header" ] [ text "Congelar grupo" ]
        , div [ class "card-body" ]
            [ div [ class "mb-3" ]
                [ text <|
                    if grupo.isFrozen then
                        "Este grupo está congelado. Las deudas están fijas y no se pueden agregar, editar ni eliminar pagos."

                    else
                        "Congelar el grupo fija las deudas actuales. No se podrán agregar, editar ni eliminar pagos mientras esté congelado."
                ]
            , viewFreezeButton grupo
            ]
        ]


viewFreezeButton : ShallowGrupo -> Html Msg
viewFreezeButton grupo =
    if grupo.isFrozen then
        button
            [ type_ "button"
            , class "btn btn-outline-secondary"
            , onClick UnfreezeGrupo
            ]
            [ text "Descongelar" ]

    else
        Bs.btn Bs.Primary
            [ onClick FreezeGrupo ]
            [ text "Congelar" ]


viewTextFormItem : String -> Bool -> Form.FieldState CustomFormError String -> Html Form.Msg
viewTextFormItem labelText isRequired field =
    div [ class "mb-3" ]
        [ label [ for field.path, class "form-label" ] [ text labelText ]
        , input
            [ type_ "text"
            , id field.path
            , class "form-control"
            , classList [ ( "is-invalid", hasErrorField field ) ]
            , value (Maybe.withDefault "" field.value)
            , onInput (\v -> Input field.path Form.Text (Form.Field.String v))
            , on "focus" (Json.Decode.succeed (Focus field.path))
            , on "blur" (Json.Decode.succeed (Blur field.path))
            , Attr.required isRequired
            ]
            []
        , if hasErrorField field then
            div [ class "invalid-feedback" ] [ errorForField field ]

          else
            text ""
        ]
