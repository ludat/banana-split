module Components.TransferenciaModal exposing (Campos, Context, Model, Msg, Ruta, Vista, context, init, linkANuevaTransferencia, linkATransferencia, onUrlChanged, update, view)

{-| El popup de una transferencia, desde cualquier pantalla del grupo, como el de
los gastos: muestra una y deja editarla o borrarla, o registra una nueva.

Lo que muestra está en la URL (`?transferencia=` con un id, o `nueva`): se abre
con un link, sobrevive a una recarga, y abrirlo y cerrarlo reemplaza la entrada
del historial en vez de agregar una.

Las transferencias se leen del store, que ya tiene la lista entera del grupo: no
hay un endpoint para traer una sola.

-}

import Components.Bootstrap as Bs
import Dict exposing (Dict)
import Effect exposing (Effect)
import Form exposing (Form)
import Form.Error
import Form.Init as Form
import Form.Validate as V exposing (Validation)
import Generated.Api as Api exposing (Grupo, Moneda, Monto, NuevaTransferenciaParams, Transferencia, ULID)
import Generated.Moneda exposing (escalaDe)
import Html exposing (Html, button, div, h4, i, label, span, text)
import Html.Attributes as Attr exposing (class, target)
import Html.Events exposing (on, onClick, onSubmit, preventDefaultOn)
import Http
import Json.Decode as Decode
import Models.Grupo exposing (estaCongelado, grupoIdFromPath)
import Models.Moneda as Moneda
import Models.Monto as Monto
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Models.Transferencia
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Route.Path as Path
import Shared
import Time exposing (Zone)
import Utils.Form exposing (CustomFormError, isDataModifyingEvent)
import Utils.Posix as Posix
import Utils.Telemetry as Telemetry
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts



-- MODEL


type Vista
    = Cerrado
    | Nueva
    | Detalle ULID


{-| `form` está mientras se carga una nueva o se edita la que se está mirando;
sin él, el detalle se muestra tal cual. `enviando` frena un segundo envío
mientras el primero no volvió.

Con `cambiosSinGuardar` cerrar pregunta antes (`confirmandoDescartar`) y el
click afuera deja de cerrar, igual que en el popup de gastos.

-}
type alias Model =
    { vista : Vista
    , form : Maybe (Form CustomFormError Campos)
    , cambiosSinGuardar : Bool
    , confirmandoDescartar : Bool
    , enviando : Bool
    , confirmandoBorrar : Bool
    }


type alias Campos =
    { from : ULID
    , to : ULID
    , monto : Monto
    , moneda : Moneda
    }


type alias Context =
    { grupoId : ULID
    , participanteId : Maybe ULID
    , path : Path.Path
    , query : Dict String String
    , store : Store
    , zone : Zone
    }


context : Shared.Model -> Route () -> Context
context shared route =
    let
        grupoId =
            grupoIdFromPath route.path |> Maybe.withDefault ""
    in
    { grupoId = grupoId
    , participanteId = Shared.currentParticipante shared grupoId
    , path = route.path
    , query = route.query
    , store = shared.store
    , zone = shared.timezone
    }


init : Context -> ( Model, Effect Msg )
init ctx =
    abrir ctx (vistaDe ctx.query)


abrir : Context -> Vista -> ( Model, Effect Msg )
abrir ctx vista =
    ( { vista = vista
      , form =
            case vista of
                Nueva ->
                    -- Quien transfiere arranca siendo uno mismo, que es lo
                    -- más común; se puede cambiar.
                    Just (Form.initial [ Form.setString "from" (Maybe.withDefault "" ctx.participanteId) ] (validar Api.ARS))

                _ ->
                    Nothing
      , cambiosSinGuardar = False
      , confirmandoDescartar = False
      , enviando = False
      , confirmandoBorrar = False
      }
    , case vista of
        Detalle _ ->
            Store.ensureTransferencias ctx.grupoId ctx.store

        _ ->
            Effect.none
    )


{-| La moneda arranca vacía y se lee como la del grupo. Así el form de una nueva
se puede armar antes de que el grupo esté en el store, que es lo que pasa al
recargar con el popup abierto.
-}
validar : Moneda -> Validation CustomFormError Campos
validar monedaPorDefecto =
    V.field "moneda" (V.oneOf [ Moneda.validate, V.succeed monedaPorDefecto ])
        |> V.andThen
            (\moneda ->
                V.field "from" noVacio
                    |> V.andThen
                        (\from ->
                            V.succeed (Campos from)
                                |> V.andMap (V.field "to" (noVacio |> V.andThen (distintoDe from)))
                                |> V.andMap (V.field "monto" (Monto.validateMonto moneda))
                                |> V.andMap (V.succeed moneda)
                        )
            )


noVacio : Validation CustomFormError String
noVacio =
    V.string |> V.andThen V.nonEmpty


distintoDe : ULID -> ULID -> Validation CustomFormError ULID
distintoDe from to =
    if to == from then
        V.fail (Form.Error.value Form.Error.NotIncludedIn)

    else
        V.succeed to


formDeEdicion : Transferencia -> Form CustomFormError Campos
formDeEdicion t =
    Form.initial
        [ Form.setString "from" t.from
        , Form.setString "to" t.to
        , Form.setString "monto" (Monto.toRawString t.monto)
        , Form.setString "moneda" (Moneda.toString t.moneda)
        ]
        (validar t.moneda)



-- URL


{-| Una ruta con su query, como la quieren `Route.href` y `Effect.replaceRoute`.
-}
type alias Ruta =
    { path : Path.Path, query : Dict String String, hash : Maybe String }


queryTransferencia : String
queryTransferencia =
    "transferencia"


{-| El valor que abre el popup en modo creación. No puede chocar con un id
porque los ULID son mayúsculas y dígitos.
-}
queryTransferenciaNueva : String
queryTransferenciaNueva =
    "nueva"


vistaDe : Dict String String -> Vista
vistaDe query =
    case Dict.get queryTransferencia query of
        Nothing ->
            Cerrado

        Just param ->
            if param == queryTransferenciaNueva then
                Nueva

            else
                Detalle param


{-| Link que abre el popup creando una. `abrir` es el mensaje de quien lo dibuja,
que tiene que responderlo con `Effect.replaceRoute`: el popup se abre desde
`onUrlChanged`, como cuando se entra por un link.
-}
linkANuevaTransferencia : (Ruta -> msg) -> Path.Path -> List (Html.Attribute msg)
linkANuevaTransferencia abrirRuta desde =
    linkAlPopup abrirRuta desde queryTransferenciaNueva


linkATransferencia : (Ruta -> msg) -> Path.Path -> ULID -> List (Html.Attribute msg)
linkATransferencia abrirRuta desde transferenciaId =
    linkAlPopup abrirRuta desde transferenciaId


linkAlPopup : (Ruta -> msg) -> Path.Path -> String -> List (Html.Attribute msg)
linkAlPopup abrirRuta desde param =
    let
        ruta =
            { path = desde, query = Dict.singleton queryTransferencia param, hash = Nothing }
    in
    [ Route.href ruta
    , target "_self"
    , preventDefaultOn "click" (Decode.succeed ( abrirRuta ruta, True ))
    ]


onUrlChanged : { from : Route (), to : Route () } -> Msg
onUrlChanged { from, to } =
    let
        param route =
            Dict.get queryTransferencia route.query
    in
    if param from == param to then
        NoOp

    else
        QueryChanged (vistaDe to.query)


{-| Cerrar es sacar el parámetro y dejar el resto de la URL como estaba.
-}
sacarDeLaUrl : Context -> Effect msg
sacarDeLaUrl ctx =
    Effect.replaceRoute
        { path = ctx.path
        , query = ctx.query |> Dict.remove queryTransferencia
        , hash = Nothing
        }



-- UPDATE


type Msg
    = NoOp
    | QueryChanged Vista
    | Cerrar
    | PedirCerrar
    | PedidoDeCierre
    | SeguirEditando
    | Editar Transferencia
    | FormMsg Moneda Form.Msg
    | Guardada (Result Http.Error Transferencia)
    | PedirBorrar
    | CancelarBorrar
    | Borrar ULID
    | Borrada (Result Http.Error ULID)


update : Context -> Msg -> Model -> ( Model, Effect Msg )
update ctx msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        QueryChanged vista ->
            let
                ( abierto, efecto ) =
                    abrir ctx vista
            in
            ( abierto, Effect.batch [ efecto, Effect.setUnsavedChangesWarning False ] )

        -- Cierra de una, sin preguntar: es a donde llegan las demás formas
        -- de cerrar cuando no hay nada que perder, y "Descartar".
        Cerrar ->
            ( { model | vista = Cerrado, form = Nothing, cambiosSinGuardar = False, confirmandoDescartar = False }
            , Effect.batch [ sacarDeLaUrl ctx, Effect.setUnsavedChangesWarning False ]
            )

        -- La X y "Cancelar": con cambios sin guardar preguntan antes.
        PedirCerrar ->
            if model.cambiosSinGuardar then
                ( { model | confirmandoDescartar = True }, Effect.none )

            else
                update ctx Cerrar model

        -- Escape o el "atrás" de mobile: cierran lo de más arriba, igual que su
        -- X o su "cancelar".
        PedidoDeCierre ->
            if model.confirmandoDescartar then
                ( { model | confirmandoDescartar = False }, Effect.none )

            else if model.confirmandoBorrar then
                ( { model | confirmandoBorrar = False }, Effect.none )

            else
                update ctx PedirCerrar model

        SeguirEditando ->
            ( { model | confirmandoDescartar = False }, Effect.none )

        Editar transferencia ->
            ( { model | form = Just (formDeEdicion transferencia), cambiosSinGuardar = False, confirmandoBorrar = False }
            , Effect.none
            )

        FormMsg monedaPorDefecto formMsg ->
            case model.form of
                Just form ->
                    let
                        actualizado =
                            Form.update (validar monedaPorDefecto) formMsg form
                    in
                    case ( formMsg, Form.getOutput actualizado, model.enviando ) of
                        ( Form.Submit, Just campos, False ) ->
                            let
                                params =
                                    NuevaTransferenciaParams campos.from campos.to campos.monto campos.moneda
                            in
                            case model.vista of
                                Nueva ->
                                    ( { model | form = Just actualizado, enviando = True }
                                    , Effect.sendCmd <| Api.postGrupoByIdTransferencias ctx.grupoId params Guardada
                                    )

                                Detalle transferenciaId ->
                                    ( { model | form = Just actualizado, enviando = True }
                                    , Effect.sendCmd <|
                                        Api.putGrupoByIdTransferenciasByTransferenciaId ctx.grupoId transferenciaId params Guardada
                                    )

                                Cerrado ->
                                    ( model, Effect.none )

                        _ ->
                            if isDataModifyingEvent formMsg && not model.cambiosSinGuardar then
                                -- Desde el primer cambio el browser pregunta
                                -- antes de recargar o cerrar la pestaña.
                                ( { model | form = Just actualizado, cambiosSinGuardar = True }
                                , Effect.setUnsavedChangesWarning True
                                )

                            else
                                ( { model | form = Just actualizado }, Effect.none )

                Nothing ->
                    ( model, Effect.none )

        Guardada (Ok transferencia) ->
            ( { model | form = Nothing, cambiosSinGuardar = False, enviando = False }
            , Effect.batch
                [ Store.setTransferencia ctx.grupoId transferencia
                , Effect.setUnsavedChangesWarning False
                , -- Una transferencia hecha mueve los netos.
                  Store.refreshResumen ctx.grupoId
                , case model.vista of
                    -- Recién creada se cierra; editada vuelve al detalle.
                    Nueva ->
                        sacarDeLaUrl ctx

                    _ ->
                        Effect.none
                , Toasts.pushToast Toasts.ToastSuccess <|
                    case model.vista of
                        Nueva ->
                            "Se registró la transferencia"

                        _ ->
                            "Se guardó la transferencia"
                ]
            )

        Guardada (Err _) ->
            ( { model | enviando = False }
            , Toasts.pushToast Toasts.ToastDanger "No se pudo guardar la transferencia"
            )

        PedirBorrar ->
            ( { model | confirmandoBorrar = True }, Effect.none )

        CancelarBorrar ->
            ( { model | confirmandoBorrar = False }, Effect.none )

        Borrar transferenciaId ->
            ( { model | enviando = True }
            , Effect.sendCmd <| Api.deleteGrupoByIdTransferenciasByTransferenciaId ctx.grupoId transferenciaId Borrada
            )

        Borrada (Ok transferenciaId) ->
            ( { model | vista = Cerrado, form = Nothing, enviando = False, confirmandoBorrar = False }
            , Effect.batch
                [ Store.removeTransferencia ctx.grupoId transferenciaId
                , Store.refreshResumen ctx.grupoId
                , sacarDeLaUrl ctx
                , Toasts.pushToast Toasts.ToastSuccess "Se borró la transferencia"
                ]
            )

        Borrada (Err _) ->
            ( { model | enviando = False, confirmandoBorrar = False }
            , Toasts.pushToast Toasts.ToastDanger "No se pudo borrar la transferencia"
            )



-- VIEW


{-| Congelado, las transferencias son el plan que se calculó: el backend no deja
cargarlas, editarlas ni borrarlas a mano.
-}
motivoCongelado : String
motivoCongelado =
    "Mientras el grupo está saldando deudas las transferencias no se pueden cargar, editar ni borrar a mano: marcá como realizadas las que se calcularon."


view : Context -> Grupo -> Model -> Html Msg
view ctx grupo model =
    case model.vista of
        Cerrado ->
            text ""

        Nueva ->
            viewModal model
                { titulo = "Registrar una transferencia"
                , body =
                    if estaCongelado grupo then
                        [ Bs.alert Bs.AlertInfo [ class "mb-0" ] [ text motivoCongelado ] ]

                    else
                        model.form |> Maybe.map (viewForm grupo) |> Maybe.withDefault []
                , footer = footerDelForm (estaCongelado grupo || model.enviando) "Registrar"
                }

        Detalle transferenciaId ->
            case Store.getTransferencias ctx.grupoId ctx.store of
                Success transferencias ->
                    case transferencias |> List.filter (\t -> t.id == transferenciaId) |> List.head of
                        Just transferencia ->
                            viewDetalle ctx grupo model transferencia

                        Nothing ->
                            viewModal model
                                { titulo = "Transferencia"
                                , body = [ Bs.alert Bs.AlertWarning [ class "mb-0" ] [ text "Esta transferencia no existe o fue borrada." ] ]
                                , footer = [ Bs.btn Bs.Secondary [ onClick Cerrar ] [ text "Cerrar" ] ]
                                }

                Failure _ ->
                    viewModal model
                        { titulo = "Transferencia"
                        , body = [ Bs.alert Bs.AlertDanger [ class "mb-0" ] [ text "No se pudo cargar la transferencia." ] ]
                        , footer = [ Bs.btn Bs.Secondary [ onClick Cerrar ] [ text "Cerrar" ] ]
                        }

                _ ->
                    viewModal model
                        { titulo = "Transferencia"
                        , body = [ div [ class "text-muted" ] [ text "Cargando..." ] ]
                        , footer = []
                        }


viewDetalle : Context -> Grupo -> Model -> Transferencia -> Html Msg
viewDetalle ctx grupo model transferencia =
    case model.form of
        Just form ->
            viewModal model
                { titulo = "Editar transferencia"
                , body = viewForm grupo form
                , footer = footerDelForm model.enviando "Guardar"
                }

        Nothing ->
            -- Como el detalle de un gasto: el título arriba, "Editar" a mano
            -- debajo, y lo destructivo escondido en el menú de al lado.
            viewMarco model
                [ div [ Attr.id "transferencia-header", class "modal-header flex-column align-items-stretch border-bottom-0 pb-0" ]
                    [ div [ class "d-flex justify-content-between align-items-start gap-2" ]
                        [ div []
                            [ div [ class "text-muted text-uppercase fw-semibold small" ] [ text "Transferencia" ]
                            , h4 [ class "modal-title mb-0" ] (Models.Transferencia.conFlechas grupo transferencia)
                            ]
                        , button
                            [ Attr.type_ "button"
                            , class "btn-close"
                            , Attr.attribute "aria-label" "Cerrar"
                            , Telemetry.click "transferencia.close"
                            , onClick Cerrar
                            ]
                            []
                        ]
                    , div [ Attr.id "transferencia-acciones", class "d-flex align-items-center gap-2 mt-3" ]
                        [ Bs.btn Bs.SecondarySolid
                            [ Attr.id "transferencia-editar"
                            , class "rounded-pill px-4"
                            , Telemetry.click "transferencia.edit"
                            , onClick (Editar transferencia)
                            , Attr.disabled (estaCongelado grupo)
                            ]
                            [ text "Editar" ]
                        , viewMenuDeAcciones grupo model transferencia
                        ]
                    ]
                , div [ class "modal-body" ]
                    [ case Models.Transferencia.estado transferencia of
                        Models.Transferencia.Hecha saldadaAt ->
                            div [ class "d-flex align-items-center gap-2" ]
                                [ Bs.badge "text-bg-success" [] [ text "Realizada" ]
                                , span [ class "text-body-secondary small" ] [ text (Posix.toString ctx.zone saldadaAt) ]
                                ]

                        Models.Transferencia.Pendiente ->
                            Bs.badge "text-bg-warning" [] [ text "Pendiente" ]
                    , if estaCongelado grupo then
                        Bs.alert Bs.AlertInfo [ class "mt-3 mb-0" ] [ text motivoCongelado ]

                      else
                        text ""
                    ]
                ]


{-| Los tres puntos, con "Eliminar" adentro. La confirmación se pide en el mismo
menú, que no se cierra al tocar lo de adentro (`data-bs-auto-close`).
-}
viewMenuDeAcciones : Grupo -> Model -> Transferencia -> Html Msg
viewMenuDeAcciones grupo model transferencia =
    div [ class "dropdown" ]
        [ button
            [ Attr.type_ "button"
            , class "btn btn-light rounded-circle d-flex align-items-center justify-content-center"
            , Attr.style "width" "2.5rem"
            , Attr.style "height" "2.5rem"
            , Attr.attribute "aria-label" "Más acciones"
            , Attr.attribute "data-bs-toggle" "dropdown"
            , Attr.attribute "data-bs-auto-close" "outside"
            , Attr.attribute "aria-expanded" "false"
            , Attr.disabled (estaCongelado grupo)
            ]
            [ i [ class "bi bi-three-dots-vertical" ] [] ]
        , div [ class "dropdown-menu dropdown-menu-end" ]
            (if model.confirmandoBorrar then
                [ div [ class "px-3 py-2", Attr.style "min-width" "16rem" ]
                    [ div [ class "small text-danger mb-2" ]
                        [ text "¿Eliminar esta transferencia? Deja de contar en los netos del grupo y no se puede deshacer." ]
                    , div [ class "d-flex gap-2" ]
                        [ Bs.btn Bs.Transparent
                            [ Telemetry.click "transferencia.delete_cancel"
                            , onClick CancelarBorrar
                            , Attr.disabled model.enviando
                            ]
                            [ text "Cancelar" ]
                        , Bs.btn Bs.Danger
                            [ Attr.id "transferencia-borrar-confirmar"
                            , Telemetry.click "transferencia.delete_confirm"
                            , onClick (Borrar transferencia.id)
                            , Attr.disabled model.enviando
                            ]
                            [ text "Eliminar" ]
                        ]
                    ]
                ]

             else
                [ button
                    [ Attr.type_ "button"
                    , Attr.id "transferencia-borrar"
                    , class "dropdown-item text-danger"
                    , Telemetry.click "transferencia.delete"
                    , onClick PedirBorrar
                    ]
                    [ i [ class "bi bi-trash me-2" ] [], text "Eliminar" ]
                ]
            )
        ]


{-| El marco del popup, con el contenido a cargo de quien lo llama: el detalle
arma su propio encabezado, con las acciones. No es `Bs.modal` porque cierra
como el popup de gastos: con el click afuera (mientras no haya cambios sin
guardar), con Escape y con el "atrás" de mobile.
-}
viewMarco : Model -> List (Html Msg) -> Html Msg
viewMarco model contenido =
    div []
        [ div
            [ Attr.id modalId
            , class "modal d-block"
            , Attr.attribute "tabindex" "-1"
            , Attr.attribute "aria-modal" "true"
            , Attr.attribute "role" "dialog"
            , on "click"
                (if model.cambiosSinGuardar then
                    Decode.succeed NoOp

                 else
                    cerrarAlClickearAfuera modalId Cerrar
                )
            ]
            [ div [ class "modal-dialog modal-dialog-centered" ]
                [ div [ class "modal-content" ] contenido ]
            ]
        , div [ class "modal-backdrop show" ] []
        , viewDescartar model
        , viewCloseWatcher
        ]


{-| Un popup con título, cuerpo y footer, y la X arriba.
-}
viewModal : Model -> { titulo : String, body : List (Html Msg), footer : List (Html Msg) } -> Html Msg
viewModal model { titulo, body, footer } =
    viewMarco model
        [ div [ class "modal-header" ]
            [ h4 [ class "modal-title fs-5 fw-bold" ] [ text titulo ]
            , button
                [ Attr.type_ "button"
                , class "btn-close"
                , Attr.attribute "aria-label" "Cerrar"
                , Telemetry.click "transferencia.close"
                , onClick PedirCerrar
                ]
                []
            ]
        , div [ class "modal-body" ] body
        , if List.isEmpty footer then
            text ""

          else
            div [ class "modal-footer" ] footer
        ]


modalId : String
modalId =
    "transferencia-modal"


{-| La confirmación de descartar, por encima del formulario, como en el popup
de gastos. El click afuera equivale a "seguir editando", que es lo que no rompe
nada.
-}
viewDescartar : Model -> Html Msg
viewDescartar model =
    if not model.confirmandoDescartar then
        text ""

    else
        div []
            [ div
                [ Attr.id descartarId
                , class "modal d-block"
                , Attr.style "z-index" "1070"
                , Attr.attribute "tabindex" "-1"
                , Attr.attribute "aria-modal" "true"
                , Attr.attribute "role" "dialog"
                , on "click" (cerrarAlClickearAfuera descartarId SeguirEditando)
                ]
                [ div [ class "modal-dialog modal-dialog-centered modal-sm" ]
                    [ div [ class "modal-content" ]
                        [ div [ class "modal-body" ]
                            [ h4 [ class "fs-5 fw-bold mb-2" ] [ text "¿Descartar los cambios?" ]
                            , Html.p [ class "text-body-secondary mb-0" ]
                                [ text "Lo que cargaste en esta transferencia se va a perder." ]
                            ]
                        , div [ class "modal-footer border-top-0" ]
                            [ Bs.btn Bs.Transparent
                                [ Telemetry.click "transferencia.discard_cancel", onClick SeguirEditando ]
                                [ text "Seguir editando" ]
                            , Bs.btn Bs.Danger
                                [ Telemetry.click "transferencia.discard_confirm", onClick Cerrar ]
                                [ text "Descartar" ]
                            ]
                        ]
                    ]
                ]
            , div [ class "modal-backdrop show", Attr.style "z-index" "1065" ] []
            ]


descartarId : String
descartarId =
    "transferencia-descartar"


{-| Mientras el popup está abierto, Escape y el "atrás" de mobile llegan como
`PedidoDeCierre` sin tocar el historial. Es el mismo `<close-watcher>` que usa
el popup de gastos (ver `PagoDetalleModal.viewCloseWatcher`).
-}
viewCloseWatcher : Html Msg
viewCloseWatcher =
    Html.node "close-watcher" [ on "close" (Decode.succeed PedidoDeCierre) ] []


{-| El click en un diálogo sólo cuenta como "afuera" cuando pegó en el propio
contenedor del modal y no en algo de adentro.
-}
cerrarAlClickearAfuera : String -> Msg -> Decode.Decoder Msg
cerrarAlClickearAfuera unId alCerrar =
    Decode.map2
        (\targetId currentId ->
            if targetId == currentId && currentId == unId then
                alCerrar

            else
                NoOp
        )
        (Decode.at [ "target", "id" ] Decode.string)
        (Decode.at [ "currentTarget", "id" ] Decode.string)


{-| "Cancelar" cierra el popup, como en el de gastos, pasando por la misma
pregunta que la X si hay cambios sin guardar.
-}
footerDelForm : Bool -> String -> List (Html Msg)
footerDelForm deshabilitado textoGuardar =
    [ Bs.btn Bs.Secondary
        [ Telemetry.click "transferencia.form_cancel", onClick PedirCerrar ]
        [ text "Cancelar" ]
    , Bs.btn Bs.Primary
        [ Attr.id "transferencia-guardar"
        , Telemetry.click "transferencia.form_submit"
        , Attr.type_ "submit"
        , Attr.attribute "form" "transferencia-form"
        , Attr.disabled deshabilitado
        ]
        [ text textoGuardar ]
    ]


viewForm : Grupo -> Form CustomFormError Campos -> List (Html Msg)
viewForm grupo form =
    let
        campoMoneda =
            Form.getFieldAsString "moneda" form

        moneda =
            campoMoneda.value
                |> Maybe.andThen Moneda.fromString
                |> Maybe.withDefault grupo.monedaPorDefecto

        from =
            Form.getFieldAsString "from" form

        participantes excepto =
            grupo.participantes
                |> List.filter (\p -> Just p.id /= excepto)
                |> List.map (\p -> ( p.id, p.nombre ))
    in
    [ Html.map (FormMsg grupo.monedaPorDefecto) <|
        -- El botón de submit vive en el footer del modal, fuera de este form;
        -- se asocia con el atributo `form`.
        Html.form [ Attr.id "transferencia-form", onSubmit Form.Submit ]
            [ div [ class "row g-2 mb-3" ]
                [ div [ class "col-6" ]
                    [ label [ class "form-label" ] [ text "Quién transfirió" ]
                    , Bs.selectInput (( "", "Elegí" ) :: participantes Nothing) from []
                    ]
                , div [ class "col-6" ]
                    [ label [ class "form-label" ] [ text "A quién" ]
                    , Bs.selectInput
                        (( "", "Elegí" ) :: participantes from.value)
                        (Form.getFieldAsString "to" form)
                        []
                    ]
                ]
            , div [ class "row g-2" ]
                [ div [ class "col-8" ]
                    [ label [ class "form-label" ] [ text "Cuánto" ]
                    , Bs.montoInput (escalaDe moneda)
                        (Form.getFieldAsString "monto" form)
                        []
                    ]
                , div [ class "col-4" ]
                    [ label [ class "form-label" ] [ text "Moneda" ]
                    , Bs.selectInput
                        (Moneda.todas |> List.map (\m -> ( Moneda.toString m, Moneda.toString m )))
                        { campoMoneda | value = Just (Moneda.toString moneda) }
                        []
                    ]
                ]
            ]
    , div [ class "form-text mt-2" ]
        [ text "Queda registrada como realizada y cuenta en los netos del grupo." ]
    ]
