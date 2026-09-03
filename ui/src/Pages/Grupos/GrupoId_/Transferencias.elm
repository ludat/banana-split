module Pages.Grupos.GrupoId_.Transferencias exposing (Confirmacion, Model, Msg, page)

import Components.Bootstrap as Bs
import Effect exposing (Effect)
import Form exposing (Form)
import Form.Init as Form
import Form.Validate as V exposing (Validation)
import Generated.Api as Api exposing (Moneda, NuevaTransferenciaParams, ShallowGrupo, Transferencia, ULID)
import Generated.Moneda exposing (escalaDe)
import Html exposing (Html, div, label, p, span, text)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onClick)
import Http
import Layouts
import Models.Moneda as Moneda
import Models.Monto as Monto
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Models.Transferencia as Transferencia
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared
import Time exposing (Zone)
import Utils.Form exposing (CustomFormError)
import Utils.Posix as Posix exposing (Posix)
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    let
        yo =
            Shared.currentParticipante shared route.params.grupoId
    in
    Page.new
        { init = \() -> init route.params.grupoId shared.store
        , update = update yo
        , subscriptions = subscriptions
        , view = view shared.store shared.timezone shared.now yo
        }
        |> Page.withLayout (\_ -> Layouts.Default_Grupo {})


type alias Model =
    { grupoId : String
    , confirmando : Maybe Confirmacion
    , nuevaForm : Maybe (Form CustomFormError NuevaTransferenciaParams)
    }


{-| Lo que se pidió hacerle a una transferencia y todavía no se confirmó. Las
dos cosas dicen que la plata se movió (o que no) y el resto del grupo lo ve, así
que ninguna va de un solo click.
-}
type Confirmacion
    = CambiarEstadoDe ULID
    | BorrarA ULID


idConfirmado : Confirmacion -> ULID
idConfirmado confirmacion =
    case confirmacion of
        CambiarEstadoDe transferenciaId ->
            transferenciaId

        BorrarA transferenciaId ->
            transferenciaId


init : ULID -> Store -> ( Model, Effect Msg )
init grupoId store =
    ( { grupoId = grupoId
      , confirmando = Nothing
      , nuevaForm = Nothing
      }
    , Effect.batch
        [ Store.ensureResumen grupoId store
        , Store.ensureGrupo grupoId store
        , Effect.getCurrentUser grupoId
        ]
    )


type Msg
    = PedirConfirmacion Confirmacion
    | CancelarConfirmacion
    | BorrarTransferencia ULID
    | SaldarTransferencia ULID
    | DesmarcarTransferencia ULID
    | TransferenciaResponse String (Result Http.Error ULID)
    | AbrirNuevaTransferencia Moneda
    | CerrarNuevaTransferencia
    | NuevaForm Form.Msg
    | TransferenciaCreada (Result Http.Error Transferencia)


validarNuevaTransferencia : ULID -> Validation CustomFormError NuevaTransferenciaParams
validarNuevaTransferencia from =
    V.field "moneda" Moneda.validate
        |> V.andThen
            (\moneda ->
                V.succeed (\to monto -> NuevaTransferenciaParams from to monto moneda)
                    |> V.andMap (V.field "to" (V.string |> V.andThen V.nonEmpty))
                    |> V.andMap (V.field "monto" (Monto.validateMonto moneda))
            )


{-| Todas las acciones terminan igual: la transferencia cambió de estado y hay que
releer el resumen, porque los netos ya no son los mismos.
-}
refrescarYAvisar : Model -> String -> ( Model, Effect Msg )
refrescarYAvisar model mensaje =
    ( { model | confirmando = Nothing }
    , Effect.batch
        -- Solo el resumen: marcar una transferencia ya no crea un pago, así que
        -- ni los pagos ni el grupo cambian.
        [ Store.refreshResumen model.grupoId
        , Toasts.pushToast Toasts.ToastSuccess mensaje
        ]
    )


update : Maybe ULID -> Msg -> Model -> ( Model, Effect Msg )
update yo msg model =
    case msg of
        AbrirNuevaTransferencia monedaPorDefecto ->
            ( { model
                | nuevaForm =
                    Just <|
                        Form.initial
                            [ Form.setString "moneda" (Moneda.toString monedaPorDefecto) ]
                            (validarNuevaTransferencia (Maybe.withDefault "" yo))
              }
            , Effect.none
            )

        CerrarNuevaTransferencia ->
            ( { model | nuevaForm = Nothing }
            , Effect.none
            )

        NuevaForm formMsg ->
            case ( model.nuevaForm, yo ) of
                ( Just form, Just from ) ->
                    let
                        validacion =
                            validarNuevaTransferencia from

                        actualizado =
                            Form.update validacion formMsg form
                    in
                    case ( formMsg, Form.getOutput actualizado ) of
                        ( Form.Submit, Just params ) ->
                            ( { model | nuevaForm = Just actualizado }
                            , Effect.sendCmd <|
                                Api.postGrupoByIdTransferencias model.grupoId params TransferenciaCreada
                            )

                        _ ->
                            ( { model | nuevaForm = Just actualizado }, Effect.none )

                _ ->
                    ( model, Effect.none )

        TransferenciaCreada (Ok _) ->
            let
                ( cerrado, efecto ) =
                    refrescarYAvisar model "Se registró la transferencia"
            in
            ( { cerrado | nuevaForm = Nothing }, efecto )

        TransferenciaCreada (Err _) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "No se pudo registrar la transferencia"
            )

        PedirConfirmacion confirmacion ->
            ( { model | confirmando = Just confirmacion }
            , Effect.none
            )

        BorrarTransferencia transferenciaId ->
            ( { model | confirmando = Nothing }
            , Effect.sendCmd <|
                Api.deleteGrupoByIdTransferenciasByTransferenciaId
                    model.grupoId
                    transferenciaId
                    (TransferenciaResponse "Se borró la transferencia")
            )

        CancelarConfirmacion ->
            ( { model | confirmando = Nothing }
            , Effect.none
            )

        SaldarTransferencia transferenciaId ->
            ( { model | confirmando = Nothing }
            , Effect.sendCmd <|
                Api.postGrupoByIdTransferenciasByTransferenciaIdSaldar
                    model.grupoId
                    transferenciaId
                    (TransferenciaResponse "Se registró la transferencia")
            )

        DesmarcarTransferencia transferenciaId ->
            ( { model | confirmando = Nothing }
            , Effect.sendCmd <|
                Api.deleteGrupoByIdTransferenciasByTransferenciaIdSaldar
                    model.grupoId
                    transferenciaId
                    (TransferenciaResponse "La transferencia volvió a quedar pendiente")
            )

        TransferenciaResponse mensaje (Ok _) ->
            refrescarYAvisar model mensaje

        TransferenciaResponse _ (Err _) ->
            ( model
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Toasts.pushToast Toasts.ToastDanger "No se pudo cambiar el estado de la transferencia"
                ]
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Store -> Zone -> Posix -> Maybe ULID -> Model -> View Msg
view store zone ahora yo model =
    case store |> Store.getGrupo model.grupoId of
        NotAsked ->
            { title = "Loading...", body = [] }

        Loading ->
            { title = "Cargando", body = [ div [ class "container-fluid py-4 text-muted" ] [ text "Cargando..." ] ] }

        Failure _ ->
            { title = "Fallo", body = [] }

        Success grupo ->
            { title = grupo.nombre
            , body =
                [ div [ class "container-fluid py-3" ]
                    [ viewContent store zone ahora yo model grupo ]
                ]
            }


viewContent : Store -> Zone -> Posix -> Maybe ULID -> Model -> ShallowGrupo -> Html Msg
viewContent store zone ahora yo model grupo =
    case store |> Store.getResumen model.grupoId of
        NotAsked ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Loading ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Failure _ ->
            Bs.alert Bs.AlertDanger [] [ text "Error cargando los datos del grupo." ]

        -- Sin congelar no hay plan que liquidar: las transferencias pendientes
        -- existen como filas recién cuando congelar decide cuáles son. Sí se
        -- puede registrar una que ya se hizo, que es lo único que el grupo
        -- descongelado acepta.
        Success (Api.GrupoAbierto resumen) ->
            let
                hechas =
                    aplanar resumen.transferenciasHechas
                        |> List.map (\( moneda, h ) -> ( Transferencia.Hecha h.saldadaAt, ( moneda, h.transferencia ) ))
                        |> List.sortBy (\( _, ( _, t ) ) -> t.id |> Maybe.withDefault "")
                        |> List.reverse
            in
            div []
                [ Bs.alert Bs.AlertInfo
                    []
                    [ text "Este grupo no está congelado, así que todavía no hay transferencias que hacer." ]
                , if List.isEmpty hechas then
                    text ""

                  else
                    div [ class "list-group" ]
                        (hechas |> List.map (viewTransferencia zone ahora grupo Borrar))
                , viewNuevaTransferencia yo grupo model
                , buscarConfirmacion model.confirmando hechas
                    |> viewConfirmacionModal grupo
                ]

        Success (Api.GrupoCongelado resumen) ->
            let
                todas : List ( Transferencia.Estado, ( Moneda, Transferencia ) )
                todas =
                    (aplanar resumen.transferenciasParaSaldar
                        |> List.map (Tuple.pair Transferencia.Pendiente)
                    )
                        ++ (aplanar resumen.transferenciasHechas
                                |> List.map
                                    (\( moneda, h ) -> ( Transferencia.Hecha h.saldadaAt, ( moneda, h.transferencia ) ))
                           )
                        -- Más nueva primero. El id es el orden en que se
                        -- crearon y no cambia al marcarlas, así que una fila no
                        -- se mueve de lugar por debajo de quien la está
                        -- tocando.
                        |> List.sortBy (\( _, ( _, t ) ) -> t.id |> Maybe.withDefault "")
                        |> List.reverse
            in
            if List.isEmpty todas then
                Bs.alert Bs.AlertInfo
                    []
                    [ text "Este congelamiento no dejó transferencias." ]

            else
                div []
                    [ viewResumenDeEstados todas
                    , div [ class "list-group" ]
                        (todas |> List.map (viewTransferencia zone ahora grupo CambiarEstado))
                    , buscarConfirmacion model.confirmando todas
                        |> viewConfirmacionModal grupo
                    ]


{-| Qué se puede hacer con una fila, que depende de en qué estado está el grupo:
congelado hay un plan que se va marcando, descongelado las que quedan son plata
que ya se movió y lo único que tiene sentido es borrar la que nunca pasó.
-}
type AccionDeFila
    = CambiarEstado
    | Borrar


buscarConfirmacion :
    Maybe Confirmacion
    -> List ( Transferencia.Estado, ( Moneda, Transferencia ) )
    -> Maybe ( Confirmacion, ( Transferencia.Estado, ( Moneda, Transferencia ) ) )
buscarConfirmacion confirmando filas =
    confirmando
        |> Maybe.andThen
            (\confirmacion ->
                filas
                    |> List.filter (\( _, ( _, t ) ) -> t.id == Just (idConfirmado confirmacion))
                    |> List.head
                    |> Maybe.map (Tuple.pair confirmacion)
            )


{-| 'PorMoneda' agrupa por moneda, pero acá las filas van todas juntas y en
orden, así que conviene la lista plana con la moneda pegada a cada una.
-}
aplanar : Api.PorMoneda (List a) -> List ( Moneda, a )
aplanar =
    List.concatMap (\( moneda, ts ) -> ts |> List.map (\t -> ( moneda, t )))


viewResumenDeEstados : List ( Transferencia.Estado, ( Moneda, Transferencia ) ) -> Html Msg
viewResumenDeEstados todas =
    let
        hechas =
            todas
                |> List.filter
                    (\( estado, _ ) ->
                        case estado of
                            Transferencia.Hecha _ ->
                                True

                            Transferencia.Pendiente ->
                                False
                    )
                |> List.length
    in
    div [ class "text-muted small mb-3" ]
        [ text (String.fromInt hechas)
        , text " de "
        , text (String.fromInt (List.length todas))
        , text
            (if List.length todas == 1 then
                " transferencia hecha"

             else
                " transferencias hechas"
            )
        ]


{-| Una fila de la lista: el estado, quién le transfiere a quién, cuánto, y la
acción que lo cambia. Sin distinguir entre "tuyas" y "ajenas": esta pantalla es
para ver y corregir el congelamiento entero.
-}
viewTransferencia : Zone -> Posix -> ShallowGrupo -> AccionDeFila -> ( Transferencia.Estado, ( Moneda, Transferencia ) ) -> Html Msg
viewTransferencia zone ahora grupo accion ( estado, ( moneda, t ) ) =
    div [ class "list-group-item d-flex align-items-center gap-3 flex-wrap" ]
        [ case estado of
            Transferencia.Pendiente ->
                span [ class "badge text-bg-secondary" ] [ text "Pendiente" ]

            Transferencia.Hecha _ ->
                span [ class "badge text-bg-success" ] [ text "Hecha" ]
        , span
            [ class "flex-grow-1"
            , class
                (case estado of
                    Transferencia.Pendiente ->
                        ""

                    Transferencia.Hecha _ ->
                        "text-muted"
                )
            ]
            (Transferencia.frase grupo moneda t
                ++ (case estado of
                        Transferencia.Pendiente ->
                            []

                        Transferencia.Hecha saldadaAt ->
                            [ span
                                [ class "text-muted small ms-2 text-nowrap"
                                , Attr.title (Posix.toString zone saldadaAt)
                                ]
                                [ text (Posix.relativo ahora saldadaAt) ]
                            ]
                   )
            )
        , case ( accion, estado, t.id ) of
            ( CambiarEstado, Transferencia.Pendiente, Just transferenciaId ) ->
                Bs.btn Bs.Secondary
                    [ class "btn-sm text-nowrap"
                    , onClick (PedirConfirmacion (CambiarEstadoDe transferenciaId))
                    ]
                    [ text "Marcar como hecha" ]

            ( CambiarEstado, Transferencia.Hecha _, Just transferenciaId ) ->
                Bs.btn Bs.Secondary
                    [ class "btn-sm text-nowrap"
                    , onClick (PedirConfirmacion (CambiarEstadoDe transferenciaId))
                    ]
                    [ text "Volver a pendiente" ]

            ( Borrar, _, Just transferenciaId ) ->
                Bs.btn Bs.Danger
                    [ class "btn-sm text-nowrap"
                    , onClick (PedirConfirmacion (BorrarA transferenciaId))
                    ]
                    [ text "Borrar" ]

            ( _, _, Nothing ) ->
                text ""
        ]


{-| El cambio de estado se relee antes de aplicarlo. El texto sale del estado
actual: se confirma pasar a hecha, o volver a pendiente.
-}
viewConfirmacionModal : ShallowGrupo -> Maybe ( Confirmacion, ( Transferencia.Estado, ( Moneda, Transferencia ) ) ) -> Html Msg
viewConfirmacionModal grupo confirmando =
    let
        ( titulo, cuerpo, accion ) =
            case confirmando of
                Just ( CambiarEstadoDe transferenciaId, ( Transferencia.Pendiente, ( moneda, t ) ) ) ->
                    ( "Marcar como hecha"
                    , Transferencia.frase grupo moneda t ++ [ text ". ¿Ya pasó?" ]
                    , Just ( Bs.Primary, SaldarTransferencia transferenciaId )
                    )

                Just ( CambiarEstadoDe transferenciaId, ( Transferencia.Hecha _, ( moneda, t ) ) ) ->
                    ( "Volver a pendiente"
                    , Transferencia.frase grupo moneda t
                        ++ [ text ". Vuelve a la lista como pendiente." ]
                    , Just ( Bs.Primary, DesmarcarTransferencia transferenciaId )
                    )

                Just ( BorrarA transferenciaId, ( _, ( moneda, t ) ) ) ->
                    ( "Borrar la transferencia"
                    , Transferencia.frase grupo moneda t
                        ++ [ text ". Se borra para siempre y deja de contar en los netos del grupo." ]
                    , Just ( Bs.Danger, BorrarTransferencia transferenciaId )
                    )

                Nothing ->
                    ( "", [], Nothing )
    in
    Bs.modal
        { isOpen = confirmando /= Nothing
        , onClose = CancelarConfirmacion
        , title = titulo
        , centered = True
        , body = [ p [] cuerpo ]
        , footer =
            [ Bs.btn Bs.Secondary
                [ onClick CancelarConfirmacion ]
                [ text "Cancelar" ]
            , case accion of
                Just ( variante, msg ) ->
                    Bs.btn variante [ onClick msg ] [ text titulo ]

                Nothing ->
                    text ""
            ]
        }


{-| Registrar a mano una transferencia que ya se hizo. Va escondida detrás de un
botón chico: es para cuando la plata se movió sin pasar por un congelamiento, no
el camino principal.

Solo con el grupo descongelado, porque congelado el plan de transferencias ya
está decidido y una que no esté en él lo invalidaría. Y solo si sabemos quién
sos en el grupo, porque vos sos el `from`.

-}
viewNuevaTransferencia : Maybe ULID -> ShallowGrupo -> Model -> Html Msg
viewNuevaTransferencia yo grupo model =
    case yo of
        Nothing ->
            text ""

        Just from ->
            div [ class "mt-4" ]
                [ Bs.btn Bs.Transparent
                    [ class "btn-sm text-muted p-0"
                    , onClick (AbrirNuevaTransferencia grupo.monedaPorDefecto)
                    ]
                    [ text "Registrar una transferencia que hice" ]
                , viewNuevaTransferenciaModal from grupo model.nuevaForm
                ]


viewNuevaTransferenciaModal : ULID -> ShallowGrupo -> Maybe (Form CustomFormError NuevaTransferenciaParams) -> Html Msg
viewNuevaTransferenciaModal from grupo nuevaForm =
    let
        campos =
            case nuevaForm of
                Nothing ->
                    []

                Just form ->
                    let
                        -- El monto lleva los decimales de la moneda elegida en
                        -- el select de al lado, no los de la del grupo.
                        moneda =
                            Form.getFieldAsString "moneda" form
                                |> .value
                                |> Maybe.andThen Moneda.fromString
                                |> Maybe.withDefault grupo.monedaPorDefecto
                    in
                    [ Html.map NuevaForm <|
                        div []
                            [ div [ class "mb-3" ]
                                [ label [ class "form-label" ] [ text "A quién" ]
                                , Bs.selectInput
                                    (( "", "Elegí a quién le transferiste" )
                                        :: (grupo.participantes
                                                |> List.filter (\p -> p.id /= from)
                                                |> List.map (\p -> ( p.id, p.nombre ))
                                           )
                                    )
                                    (Form.getFieldAsString "to" form)
                                    []
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
                                        (Form.getFieldAsString "moneda" form)
                                        []
                                    ]
                                ]
                            ]
                    , div [ class "form-text mt-2" ]
                        [ text "Queda registrada como hecha y cuenta en los netos del grupo." ]
                    ]
    in
    Bs.modal
        { isOpen = nuevaForm /= Nothing
        , onClose = CerrarNuevaTransferencia
        , title = "Registrar una transferencia que hice"
        , centered = True
        , body = campos
        , footer =
            [ Bs.btn Bs.Secondary
                [ onClick CerrarNuevaTransferencia ]
                [ text "Cancelar" ]
            , Bs.btn Bs.Primary
                [ onClick (NuevaForm Form.Submit) ]
                [ text "Registrar" ]
            ]
        }
