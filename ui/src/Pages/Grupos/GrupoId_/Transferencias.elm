module Pages.Grupos.GrupoId_.Transferencias exposing (Confirmacion, Model, Msg, page)

import Components.Bootstrap as Bs
import Effect exposing (Effect)
import Form exposing (Form)
import Form.Init as Form
import Form.Validate as V exposing (Validation)
import Generated.Api as Api exposing (Grupo, Moneda, NuevaTransferenciaParams, Transferencia, ULID)
import Generated.Moneda exposing (escalaDe)
import Html exposing (Html, div, em, h6, i, label, li, p, span, strong, text, ul)
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
import RemoteData exposing (RemoteData(..), WebData)
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
    , mostrarModalDeConfirmacion : Bool
    , congelarGrupoRequest : WebData Grupo
    , bannerAbierto : Bool
    , descongelarGrupoRequest : WebData Grupo
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
      , mostrarModalDeConfirmacion = False
      , congelarGrupoRequest = NotAsked
      , bannerAbierto = False
      , descongelarGrupoRequest = NotAsked
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
    | MostrarModalDeCongelarGrupo
    | CancelarModalDeCongelarGrupo
    | CongelarGrupo
    | CongelarGrupoResponse (Result Http.Error Grupo)
    | AlternarBanner
    | DescongelarGrupo
    | DescongelarGrupoResponse (Result Http.Error Grupo)


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
        MostrarModalDeCongelarGrupo ->
            ( { model | mostrarModalDeConfirmacion = True }
            , Effect.none
            )

        CancelarModalDeCongelarGrupo ->
            ( { model | mostrarModalDeConfirmacion = False }
            , Effect.none
            )

        CongelarGrupo ->
            ( { model | congelarGrupoRequest = Loading }
            , Effect.sendCmd <| Api.postGrupoByIdFreeze model.grupoId CongelarGrupoResponse
            )

        CongelarGrupoResponse (Ok grupo) ->
            ( { model | congelarGrupoRequest = Success grupo, mostrarModalDeConfirmacion = False }
            , Effect.batch
                [ Store.refreshGrupo model.grupoId
                , Store.refreshResumen model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Empezaron a saldarse las deudas"
                ]
            )

        CongelarGrupoResponse (Err error) ->
            ( { model | congelarGrupoRequest = Failure error, mostrarModalDeConfirmacion = False }
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Toasts.pushToast Toasts.ToastDanger "No se pudo empezar a saldar las deudas. Revisá que no queden gastos inválidos y que estén cargadas las tasas de cambio de todas las monedas con deuda."
                ]
            )

        AlternarBanner ->
            ( { model | bannerAbierto = not model.bannerAbierto }
            , Effect.none
            )

        DescongelarGrupo ->
            ( { model | descongelarGrupoRequest = Loading }
            , Effect.sendCmd <| Api.deleteGrupoByIdFreeze model.grupoId DescongelarGrupoResponse
            )

        DescongelarGrupoResponse (Ok grupo) ->
            -- Desactivar tira las pendientes y el grupo vuelve a aceptar
            -- gastos: cambia la pantalla entera, así que se releen las dos
            -- cosas.
            ( { model | descongelarGrupoRequest = Success grupo, bannerAbierto = False }
            , Effect.batch
                [ Store.refreshGrupo model.grupoId
                , Store.refreshResumen model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "El grupo dejó de saldar deudas"
                ]
            )

        DescongelarGrupoResponse (Err error) ->
            ( { model | descongelarGrupoRequest = Failure error }
            , Toasts.pushToast Toasts.ToastDanger "No se pudo dejar de saldar deudas"
            )

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
                    [ div [ class "row justify-content-center" ]
                        [ div [ class "col-md-10 col-lg-7 col-xl-5" ]
                            [ viewContent store zone ahora yo model grupo
                            , espacioParaLaBarraInferior
                            ]
                        ]
                    ]
                ]
            }


espacioParaLaBarraInferior : Html msg
espacioParaLaBarraInferior =
    div [ class "d-md-none", Attr.style "height" "7rem" ] []


iconoCongelado : String
iconoCongelado =
    "bi bi-cash-stack"


viewContent : Store -> Zone -> Posix -> Maybe ULID -> Model -> Grupo -> Html Msg
viewContent store zone ahora yo model grupo =
    case store |> Store.getResumen model.grupoId of
        NotAsked ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Loading ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Failure _ ->
            Bs.alert Bs.AlertDanger [] [ text "Error cargando los datos del grupo." ]

        Success (Api.GrupoAbierto resumen) ->
            let
                hechas =
                    resumen.transferencias
                        |> List.filter Transferencia.estaHecha
                        |> List.sortBy .id
                        |> List.reverse
            in
            div []
                [ viewInvitacionACongelar resumen model
                , viewPrevisualizar
                , if List.isEmpty hechas then
                    text ""

                  else
                    div [ class "card mt-4" ]
                        [ Bs.cardHeader []
                            [ span [ class "text-uppercase small fw-bold" ]
                                [ text "Transferencias registradas" ]
                            ]
                        , div [ class "list-group list-group-flush" ]
                            (hechas |> List.map (viewTransferencia zone ahora grupo Borrar))
                        ]
                , viewNuevaTransferencia yo grupo model
                , buscarConfirmacion model.confirmando hechas
                    |> viewConfirmacionModal grupo
                , viewActivacionModal model
                ]

        Success (Api.GrupoCongelado resumen) ->
            let
                ordenadas : List Transferencia -> List Transferencia
                ordenadas =
                    -- Más nueva primero. El id es el orden en que se crearon y
                    -- no cambia al marcarlas, así que una fila no se mueve de
                    -- lugar por debajo de quien la está tocando.
                    List.sortBy .id >> List.reverse

                -- El plan que calculó este congelamiento, que es lo que hay para hacer.
                calculadas : List Transferencia
                calculadas =
                    resumen.transferencias
                        |> List.filter (Transferencia.esPropuesta grupo)
                        |> ordenadas

                -- Lo que quedó de antes: planes de congelamientos anteriores y las que
                -- alguien registró a mano. Son historia, no hay nada que
                -- hacerles.
                anteriores : List Transferencia
                anteriores =
                    resumen.transferencias
                        |> List.filter (not << Transferencia.esPropuesta grupo)
                        |> ordenadas
            in
            div []
                [ viewBannerCongelado model
                , if List.isEmpty calculadas then
                    Bs.alert Bs.AlertInfo
                        [ class "mt-3" ]
                        [ text "No quedaron transferencias que hacer para saldar las deudas." ]

                  else
                    viewCalculadas zone ahora grupo calculadas
                , viewAnteriores zone ahora grupo anteriores
                , buscarConfirmacion model.confirmando resumen.transferencias
                    |> viewConfirmacionModal grupo
                ]


{-| Los tres cambios que trae congelar para todo el grupo. El mismo texto se dice
antes de activarlo —en el modal, para decidir— y después —en el banner, para
recordar qué pasó—, así que vive en un solo lugar.
-}
viewCambiosAlCongelar : List (Html msg)
viewCambiosAlCongelar =
    [ li []
        [ text "Ya no podrán "
        , em [] [ text "cargar" ]
        , text " y "
        , em [] [ text "editar" ]
        , text " gastos en este grupo."
        ]
    , li []
        [ text "La página de "
        , em [] [ text "Resumen del grupo" ]
        , text " cambiará y empezará a mostrar información relevante sobre las transferencias."
        ]
    , li []
        [ text "Esta sección "
        , em [] [ text "Transferencias" ]
        , text " comenzará a mostrar el estado de las transferencias de todo el grupo."
        ]
    ]


{-| El banner violeta de arriba de todo con el grupo saldando deudas. Arranca
cerrado —todos los días lo que importa es la lista de abajo— y al abrirlo
explica qué pasó y ofrece descongelar.
-}
viewBannerCongelado : Model -> Html Msg
viewBannerCongelado model =
    div [ Bs.fondoCongelado, class "text-white rounded mb-3" ]
        [ Html.button
            [ Attr.type_ "button"
            , class "btn w-100 d-flex align-items-center gap-2 text-white fw-semibold px-3 py-3 border-0 shadow-none"
            , Attr.attribute "aria-expanded"
                (if model.bannerAbierto then
                    "true"

                 else
                    "false"
                )
            , onClick AlternarBanner
            ]
            [ i [ class iconoCongelado ] []
            , span [ class "flex-grow-1 text-start" ] [ text "Saldando deudas" ]
            , i
                [ class
                    (if model.bannerAbierto then
                        "bi bi-chevron-up"

                     else
                        "bi bi-chevron-down"
                    )
                ]
                []
            ]
        , if model.bannerAbierto then
            let
                desactivando =
                    RemoteData.isLoading model.descongelarGrupoRequest
            in
            div [ class "px-3 pb-3" ]
                [ p []
                    [ text "Este grupo se encuentra saldando deudas, que ayuda a los participantes a saldar sus gastos. Al empezar se calcularon todas las transferencias de dinero que se deben realizar entre los participantes para quedar saldados." ]
                , p [ class "fw-bold mb-2" ] [ text "Cambios experimentados por todos los participantes:" ]
                , ul [ class "mb-3" ] viewCambiosAlCongelar
                , Bs.btn Bs.CongeladoInverso
                    [ class "w-100 py-2 d-flex align-items-center justify-content-center gap-2"
                    , onClick DescongelarGrupo
                    , Attr.disabled desactivando
                    ]
                    (if desactivando then
                        [ Bs.spinner
                            [ Attr.style "width" "1.25rem"
                            , Attr.style "height" "1.25rem"
                            , Attr.style "border-width" "0.2em"
                            , Attr.style "flex" "0 0 auto"
                            ]
                        , text "Desactivando..."
                        ]

                     else
                        [ i [ class "bi bi-x-circle" ] []
                        , text "Dejar de saldar deudas"
                        ]
                    )
                ]

          else
            text ""
        ]


{-| El plan que dejó el congelamiento: lo que hay para hacer, con su progreso.
-}
viewCalculadas : Zone -> Posix -> Grupo -> List Transferencia -> Html Msg
viewCalculadas zone ahora grupo calculadas =
    div [ class "card" ]
        [ Bs.cardHeader []
            -- El `flex-wrap` es para los teléfonos angostos: abajo de ~410px de
            -- ancho el título y el contador no entran juntos, y el contador
            -- baja a una segunda línea en vez de apretar el título.
            [ div [ class "d-flex align-items-center justify-content-between gap-2 flex-wrap" ]
                [ span [ class "text-uppercase small fw-bold" ]
                    [ text "Transferencias calculadas" ]
                , viewContador grupo calculadas
                ]
            , p [ class "text-body-secondary small mb-0 mt-1" ]
                [ text "Al empezar a saldar deudas se calcularon las siguientes transferencias a realizar." ]
            ]
        , div [ class "list-group list-group-flush" ]
            (calculadas |> List.map (viewTransferencia zone ahora grupo CambiarEstado))
        ]


{-| Lo que quedó de antes: planes de congelamientos anteriores y las que alguien registró
a mano. Son historia —no hay nada que marcar ni corregir—, así que van sin
acciones y con hace cuánto se hicieron.
-}
viewAnteriores : Zone -> Posix -> Grupo -> List Transferencia -> Html Msg
viewAnteriores zone ahora grupo anteriores =
    if List.isEmpty anteriores then
        text ""

    else
        div [ class "card mt-4" ]
            [ Bs.cardHeader []
                [ span [ class "text-uppercase small fw-bold" ]
                    [ text "Transferencias anteriores" ]
                , p [ class "text-body-secondary small mb-0 mt-1" ]
                    [ text "Estas son transferencias calculadas en congelamientos pasados o registradas por los participantes." ]
                ]
            , div [ class "list-group list-group-flush" ]
                (anteriores |> List.map (viewTransferenciaAnterior zone ahora grupo))
            ]


viewTransferenciaAnterior : Zone -> Posix -> Grupo -> Transferencia -> Html Msg
viewTransferenciaAnterior zone ahora grupo t =
    div [ class "list-group-item d-flex align-items-center justify-content-between gap-3 flex-wrap py-3" ]
        [ div [] (Transferencia.conFlechas grupo t)
        , case t.saldadaAt of
            Just saldadaAt ->
                -- Hace cuánto, igual que en las filas de arriba. La fecha
                -- exacta queda en el `title`, que es donde importa para algo
                -- viejo.
                span
                    [ class "text-body-secondary small text-nowrap"
                    , Attr.title (Posix.toString zone saldadaAt)
                    ]
                    [ text (Posix.relativo ahora saldadaAt) ]

            Nothing ->
                text ""
        ]


{-| Lo que ve un grupo abierto: qué es saldar deudas, qué cambia para
todos, y el botón que abre el modal de confirmación.
-}
viewInvitacionACongelar : Api.ResumenAbierto -> Model -> Html Msg
viewInvitacionACongelar resumen model =
    let
        monedasSinTasa : List Moneda
        monedasSinTasa =
            resumen.consolidado.monedasSinTasa

        gastosInvalidos : Int
        gastosInvalidos =
            resumen.cantidadPagosInvalidos

        activando : Bool
        activando =
            RemoteData.isLoading model.congelarGrupoRequest
    in
    div []
        [ p []
            [ text "Banana Split calcula todos los envíos de dinero que deben realizarse entre los participantes del grupo para quedar saldados. Para esto el grupo tiene que "
            , strong [] [ text "empezar a saldar deudas" ]
            , text "."
            ]
        , h6 [ class "fw-bold mb-1" ] [ text "Saldando deudas" ]
        , p [ class "mb-1" ]
            [ text "Este es un cambio de estado del grupo que modificará la visualización de todos los participantes y prohibirá la carga y edición de gastos*, esto es necesario para el cálculo de las transferencias." ]
        , p [ class "text-muted small" ]
            [ text "*Esta acción se puede deshacer pero hay que tener en cuenta su implicancia en las transferencias realizadas." ]
        , viewAvisosParaActivar monedasSinTasa gastosInvalidos
        , Bs.btn Bs.Congelado
            [ class "w-100 py-3 fs-5 d-flex align-items-center justify-content-center gap-2"
            , onClick MostrarModalDeCongelarGrupo
            , Attr.disabled (not (List.isEmpty monedasSinTasa) || gastosInvalidos /= 0 || activando)
            ]
            (if activando then
                -- Calcular las transferencias mínimas puede tardar bastante
                -- —es un solver, no una cuenta—, así que mientras tanto el
                -- botón tiene que mostrar que algo está pasando.
                [ Bs.spinner
                    [ Attr.style "width" "1.25rem"
                    , Attr.style "height" "1.25rem"
                    , Attr.style "border-width" "0.2em"
                    , Attr.style "flex" "0 0 auto"
                    , class "text-white"
                    ]
                , text "Calculando..."
                ]

             else
                [ i [ class iconoCongelado ] []
                , text "Empezar a saldar deudas"
                ]
            )
        ]


viewAvisosParaActivar : List Moneda -> Int -> Html Msg
viewAvisosParaActivar monedasSinTasa gastosInvalidos =
    div []
        [ if List.isEmpty monedasSinTasa then
            text ""

          else
            Bs.alert Bs.AlertWarning
                [ class "mb-3" ]
                [ text <|
                    (if List.length monedasSinTasa == 1 then
                        "Para empezar a saldar deudas falta la tasa de cambio de "

                     else
                        "Para empezar a saldar deudas faltan las tasas de cambio de "
                    )
                        ++ (monedasSinTasa |> List.map Moneda.nombre |> String.join ", ")
                        ++ ". Cargalas en Ajustes del grupo."
                ]
        , if gastosInvalidos == 0 then
            text ""

          else
            Bs.alert Bs.AlertWarning
                [ class "mb-3" ]
                [ text <|
                    if gastosInvalidos == 1 then
                        "Para empezar a saldar deudas hay que arreglar 1 gasto inválido: no se cuenta para las deudas."

                    else
                        "Para empezar a saldar deudas hay que arreglar "
                            ++ String.fromInt gastosInvalidos
                            ++ " gastos inválidos: no se cuentan para las deudas."
                ]
        ]


{-| La card de previsualización del diseño. El plan de transferencias lo calcula
`minimizeTransferencias` en el backend y hoy solo corre al congelar el grupo, así
que no hay de dónde leerlo sin congelar: el botón queda deshabilitado
hasta que exista el endpoint que lo devuelva.
-}
viewPrevisualizar : Html Msg
viewPrevisualizar =
    Bs.card [ class "mt-4" ]
        [ Bs.cardBody []
            [ h6 [ class "text-uppercase text-muted small fw-bold" ]
                [ text "Previsualizar envíos de dinero" ]
            , p [ class "mb-3" ]
                [ text "Observá cómo quedarían calculadas las transferencias antes de empezar a saldar deudas." ]
            , div [ class "text-center" ]
                [ Bs.btn Bs.Primary [ Attr.disabled True ] [ text "Previsualizar" ] ]
            , p [ class "form-text text-center mb-0" ]
                [ text "Todavía no está disponible." ]
            ]
        ]


{-| El progreso sobre el plan que propuso la app, no sobre la lista entera: las
que alguien registró a mano antes de congelar aparecen igual como filas, pero no
son algo que quede por hacer y contarlas daba un progreso arrancado.
-}
viewContador : Grupo -> List Transferencia -> Html Msg
viewContador grupo todas =
    let
        progreso =
            Transferencia.progresoDelPlan grupo todas
    in
    span [ class "d-flex align-items-center gap-2 small text-muted" ]
        [ text "Realizadas"

        -- Ámbar mientras quede algo por hacer y verde cuando no: el contador
        -- usa los mismos colores que los estados de las filas que cuenta.
        , Bs.badge
            (if progreso.completadas == progreso.total then
                "text-bg-success"

             else
                "text-bg-warning"
            )
            []
            [ text (String.fromInt progreso.completadas ++ " / " ++ String.fromInt progreso.total) ]
        ]


{-| Qué se puede hacer con una fila, que depende de en qué estado está el grupo:
congelado hay un plan que se va marcando, descongelado las que
quedan son plata que ya se movió y lo único que tiene sentido es borrar la que
nunca pasó.
-}
type AccionDeFila
    = CambiarEstado
    | Borrar


buscarConfirmacion :
    Maybe Confirmacion
    -> List Transferencia
    -> Maybe ( Confirmacion, Transferencia )
buscarConfirmacion confirmando filas =
    confirmando
        |> Maybe.andThen
            (\confirmacion ->
                filas
                    |> List.filter (\t -> t.id == idConfirmado confirmacion)
                    |> List.head
                    |> Maybe.map (Tuple.pair confirmacion)
            )


{-| Una fila de la lista: el estado, cuándo se marcó, quién le transfiere a
quién y cuánto, y un menú con lo único que se le puede hacer. Sin distinguir
entre "tuyas" y "ajenas": esta pantalla es para ver y corregir el plan entero.
-}
viewTransferencia : Zone -> Posix -> Grupo -> AccionDeFila -> Transferencia -> Html Msg
viewTransferencia zone ahora grupo accion t =
    let
        estado =
            Transferencia.estado t
    in
    div [ class "list-group-item py-3 d-flex align-items-start gap-2" ]
        [ div [ class "flex-grow-1" ]
            [ div [ class "d-flex align-items-center gap-2 mb-2" ]
                [ case estado of
                    Transferencia.Pendiente ->
                        Bs.badge "text-bg-warning" [] [ text "Pendiente" ]

                    Transferencia.Hecha _ ->
                        Bs.badge "text-bg-success" [] [ text "Realizada" ]
                , case estado of
                    Transferencia.Pendiente ->
                        text ""

                    Transferencia.Hecha saldadaAt ->
                        span
                            [ class "text-muted small text-nowrap"
                            , Attr.title (Posix.toString zone saldadaAt)
                            ]
                            [ text (Posix.relativo ahora saldadaAt) ]
                ]
            , div [] (Transferencia.conFlechas grupo t)
            ]
        , viewMenuDeFila accion estado t
        ]


{-| El menú de los tres puntos. Lleva una sola cosa, la que corresponde según de
qué lista venga la fila: con el grupo congelado, cambiar el estado; sin congelar, las
que quedan son plata que ya se movió y lo único que tiene sentido es borrar la
que nunca pasó.
-}
viewMenuDeFila : AccionDeFila -> Transferencia.Estado -> Transferencia -> Html Msg
viewMenuDeFila accion estado t =
    let
        -- Un `button` y no un `a href="#"`: el ítem no navega a ningún lado, y
        -- con el ancla el `#` se le pega a la URL igual.
        item icono etiqueta extraClass confirmacion =
            Html.li []
                [ Html.button
                    [ Attr.type_ "button"
                    , class ("dropdown-item " ++ extraClass)
                    , onClick (PedirConfirmacion confirmacion)
                    ]
                    [ i [ class (icono ++ " me-2") ] []
                    , text etiqueta
                    ]
                ]
    in
    div [ class "dropdown flex-shrink-0" ]
        [ Html.button
            [ Attr.type_ "button"
            , class "btn btn-sm border-0 text-body-secondary"
            , Attr.attribute "data-bs-toggle" "dropdown"
            , Attr.attribute "aria-expanded" "false"
            , Attr.attribute "aria-label" "Acciones de la transferencia"
            ]
            [ i [ class "bi bi-three-dots-vertical" ] [] ]
        , ul [ class "dropdown-menu dropdown-menu-end shadow" ]
            [ case ( accion, estado ) of
                ( CambiarEstado, Transferencia.Pendiente ) ->
                    item "bi bi-check2" "Marcar como Realizada" "" (CambiarEstadoDe t.id)

                ( CambiarEstado, Transferencia.Hecha _ ) ->
                    item "bi bi-arrow-counterclockwise" "Marcar como Pendiente" "" (CambiarEstadoDe t.id)

                ( Borrar, _ ) ->
                    item "bi bi-trash" "Borrar" "text-danger" (BorrarA t.id)
            ]
        ]


viewActivacionModal : Model -> Html Msg
viewActivacionModal model =
    let
        activando =
            RemoteData.isLoading model.congelarGrupoRequest
    in
    Bs.modal
        { isOpen = model.mostrarModalDeConfirmacion
        , onClose = CancelarModalDeCongelarGrupo
        , title = "Empezar a saldar deudas"
        , centered = True
        , body =
            [ div [ Bs.textoCongelado, class "text-center mb-4" ]
                [ i [ class iconoCongelado, Attr.style "font-size" "4rem" ] [] ]
            , p []
                [ text "Es un "
                , strong [] [ text "cambio de estado" ]
                , text " del grupo que ayudará a que los participantes puedan "
                , strong [] [ text "saldar los gastos" ]
                , text ". Se calcularán todas las transferencias que deben realizar los participantes para quedar saldados."
                ]
            , p [ class "fw-bold mb-2" ] [ text "Cambios para todos los participantes:" ]
            , ul [ class "mb-3" ] viewCambiosAlCongelar
            , p [ class "text-muted small mb-0" ]
                [ text "Este cambio de estado se podrá revertir desde "
                , em [] [ text "Ajustes" ]
                , text " del grupo, pero tiene implicancias."
                ]
            ]
        , footer =
            [ Bs.btn Bs.Congelado
                [ class "w-100 py-2 d-flex align-items-center justify-content-center gap-2"
                , onClick CongelarGrupo
                , Attr.disabled activando
                ]
                (if activando then
                    [ Bs.spinner
                        [ Attr.style "width" "1.25rem"
                        , Attr.style "height" "1.25rem"
                        , Attr.style "border-width" "0.2em"
                        , Attr.style "flex" "0 0 auto"
                        , class "text-white"
                        ]
                    , text "Calculando..."
                    ]

                 else
                    [ text "Confirmar" ]
                )
            ]
        }


{-| El cambio de estado se relee antes de aplicarlo. El texto sale del estado
actual: se confirma pasar a realizada, o volver a pendiente.
-}
viewConfirmacionModal : Grupo -> Maybe ( Confirmacion, Transferencia ) -> Html Msg
viewConfirmacionModal grupo confirmando =
    let
        ( titulo, cuerpo, accion ) =
            case confirmando |> Maybe.map (\( c, t ) -> ( c, Transferencia.estado t, t )) of
                Just ( CambiarEstadoDe transferenciaId, Transferencia.Pendiente, t ) ->
                    ( "Marcar como Realizada"
                    , Transferencia.frase grupo t ++ [ text ". ¿Ya pasó?" ]
                    , Just ( Bs.Primary, SaldarTransferencia transferenciaId )
                    )

                Just ( CambiarEstadoDe transferenciaId, Transferencia.Hecha _, t ) ->
                    ( "Marcar como Pendiente"
                    , Transferencia.frase grupo t
                        ++ [ text ". Vuelve a la lista como pendiente." ]
                    , Just ( Bs.Primary, DesmarcarTransferencia transferenciaId )
                    )

                Just ( BorrarA transferenciaId, _, t ) ->
                    ( "Borrar la transferencia"
                    , Transferencia.frase grupo t
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


viewNuevaTransferencia : Maybe ULID -> Grupo -> Model -> Html Msg
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


viewNuevaTransferenciaModal : ULID -> Grupo -> Maybe (Form CustomFormError NuevaTransferenciaParams) -> Html Msg
viewNuevaTransferenciaModal from grupo nuevaForm =
    let
        campos =
            case nuevaForm of
                Nothing ->
                    []

                Just form ->
                    let
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
                        [ text "Queda registrada como realizada y cuenta en los netos del grupo." ]
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
