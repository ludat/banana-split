module Pages.Grupos.Id_ exposing (Model, Msg, Tab, page)

import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.Bootstrap as Bs
import Components.PagoDetalleModal as PagoDetalleModal
import Components.ResumenGasto as ResumenGasto
import Date
import Effect exposing (Effect)
import Generated.Api as Api exposing (Grupo, Moneda, Netos, ShallowPago, ULID)
import Html exposing (Html, a, button, div, i, li, p, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes as Attr exposing (class, classList, style, type_)
import Html.Events exposing (onClick)
import Http
import Layouts
import Models.Grupo exposing (GrupoLike, estaCongelado, lookupNombreParticipante)
import Models.Moneda as Moneda
import Models.Monto as Monto
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Models.Transferencia as Transferencia
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Route.Path as Path
import Shared
import Svg
import Svg.Attributes as SvgAttr
import Time exposing (Zone)
import Utils.Day
import Utils.Posix as Posix exposing (Posix)
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts
import View exposing (View)


page : Shared.Model -> Route { id : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route shared.store
        , update = update shared.store (PagoDetalleModal.context shared route)
        , subscriptions = subscriptions
        , view = view shared.store shared.timezone shared.now (Shared.currentParticipante shared route.params.id)
        }
        |> Page.withLayout (\_ -> Layouts.Default_Grupo {})
        |> Page.withOnUrlChanged (PagoModalMsg << PagoDetalleModal.onUrlChanged)


type alias Model =
    { grupoId : String
    , tabSeleccionado : Maybe Tab
    , pagoModal : PagoDetalleModal.Model
    , confirmando : Maybe Api.Transferencia
    }


init : Route { id : String } -> Store -> ( Model, Effect Msg )
init route store =
    let
        grupoId =
            route.params.id

        ( pagoModal, modalEffect ) =
            PagoDetalleModal.init route
    in
    ( { grupoId = grupoId
      , tabSeleccionado = Nothing
      , pagoModal = pagoModal
      , confirmando = Nothing
      }
    , Effect.batch
        [ Store.ensureResumen grupoId store
        , Store.ensureGrupo grupoId store
        , -- Los gastos los pide Shared cuando resuelve el participante, que es
          -- lo que este mensaje dispara.
          Effect.getCurrentUser grupoId
        , Effect.setUnsavedChangesWarning False
        , Effect.map PagoModalMsg modalEffect
        ]
    )


type Tab
    = TabMoneda Moneda
    | TabTotal


tabActivo : Maybe Tab -> Tab
tabActivo seleccionado =
    seleccionado |> Maybe.withDefault TabTotal


type Msg
    = SelectTab Tab
    | PagoModalMsg PagoDetalleModal.Msg
    | PedirConfirmacion Api.Transferencia
    | CancelarConfirmacion
    | SaldarTransferencia ULID
    | TransferenciaResponse String (Result Http.Error ULID)


update : Store -> PagoDetalleModal.Context -> Msg -> Model -> ( Model, Effect Msg )
update store ctx msg model =
    case msg of
        SelectTab tab ->
            ( { model | tabSeleccionado = Just tab }
            , Effect.none
            )

        PedirConfirmacion confirmacion ->
            ( { model | confirmando = Just confirmacion }
            , Effect.none
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

        TransferenciaResponse mensaje (Ok _) ->
            ( model
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess mensaje
                ]
            )

        TransferenciaResponse _ (Err _) ->
            ( model
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Toasts.pushToast Toasts.ToastDanger "No se pudo cambiar el estado de la transferencia"
                ]
            )

        PagoModalMsg subMsg ->
            let
                ( pagoModal, eff ) =
                    PagoDetalleModal.update ctx store subMsg model.pagoModal
            in
            ( { model | pagoModal = pagoModal }, Effect.map PagoModalMsg eff )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Store -> Zone -> Posix -> Maybe String -> Model -> View Msg
view store zone ahora userId model =
    case store |> Store.getGrupo model.grupoId of
        NotAsked ->
            { title = "Loading...", body = [] }

        Loading ->
            { title = "Cargando"
            , body = [ div [ class "container-fluid py-4 text-muted" ] [ text "Cargando..." ] ]
            }

        Failure _ ->
            { title = "Fallo", body = [] }

        Success grupo ->
            { title = grupo.nombre
            , body =
                [ if List.isEmpty grupo.participantes then
                    div [ class "container-fluid py-3" ]
                        [ p [] [ text "Tu grupo todavía no tiene participantes!" ]
                        , p []
                            [ text "Agregalos "
                            , a [ Path.href <| Path.Grupos_GrupoId__Participantes { grupoId = grupo.id } ]
                                [ text "acá" ]
                            ]
                        ]

                  else
                    div [ class "container-fluid py-3" ]
                        [ div [ class "row g-4" ]
                            [ div [ class "col-lg-8" ]
                                [ viewLeftColumn store zone userId model grupo ]
                            , div [ class "col-lg-4" ]
                                [ viewRightColumn store zone ahora userId model grupo ]
                            ]
                        ]
                , Html.map PagoModalMsg (PagoDetalleModal.view store grupo model.pagoModal)
                , viewConfirmacionModal userId grupo model.confirmando
                ]
            }


{-| La columna angosta acompaña a la de al lado: con el grupo abierto lo que
está pasando son gastos, y congelado son transferencias.
-}
viewRightColumn : Store -> Zone -> Posix -> Maybe String -> Model -> Grupo -> Html Msg
viewRightColumn store zone ahora userId model grupo =
    if estaCongelado grupo then
        case store |> Store.getResumen model.grupoId of
            Success (Api.GrupoCongelado resumen) ->
                viewUltimasTransferenciasCard zone ahora grupo resumen.transferencias

            _ ->
                text ""

    else
        viewUltimosPagosCard userId store model grupo


viewLeftColumn : Store -> Zone -> Maybe String -> Model -> Grupo -> Html Msg
viewLeftColumn store zone userId model grupo =
    case store |> Store.getResumen model.grupoId of
        NotAsked ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Loading ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Failure _ ->
            Bs.alert Bs.AlertDanger [] [ text "Error cargando los datos del grupo." ]

        Success (Api.GrupoCongelado resumen) ->
            viewGrupoCongelado zone userId grupo resumen

        Success (Api.GrupoAbierto resumen) ->
            if resumen.cantidadPagos == 0 then
                Bs.alert Bs.AlertInfo
                    []
                    [ text "Todavía no hay gastos registrados. "
                    , a [ PagoDetalleModal.hrefNuevoGasto <| Path.Grupos_Id_ { id = grupo.id } ]
                        [ text "¡Agregá el primer gasto para empezar a dividir!" ]
                    ]

            else
                let
                    monedasDisponibles : List Moneda
                    monedasDisponibles =
                        resumen.netos
                            |> List.map Tuple.first
                            |> List.filter (\m -> m /= grupo.monedaPorDefecto)
                            |> (::) grupo.monedaPorDefecto

                    tabActual : Tab
                    tabActual =
                        tabActivo model.tabSeleccionado

                    ( monedaMostrada, netosMostrados ) =
                        case tabActual of
                            TabTotal ->
                                ( resumen.consolidado.moneda
                                , Just resumen.consolidado.netos
                                )

                            TabMoneda moneda ->
                                ( moneda
                                , resumen.netos
                                    |> List.filter (\( m, _ ) -> m == moneda)
                                    |> List.head
                                    |> Maybe.map Tuple.second
                                )
                in
                div []
                    [ if resumen.cantidadPagosInvalidos > 0 then
                        Bs.alert Bs.AlertDanger
                            [ class "mb-3" ]
                            [ text <|
                                if resumen.cantidadPagosInvalidos == 1 then
                                    "Tenés 1 gasto inválido, ese no se cuenta para las deudas."

                                else
                                    "Tenés "
                                        ++ String.fromInt resumen.cantidadPagosInvalidos
                                        ++ " gastos inválidos, esos no se cuentan para las deudas."
                            ]

                      else
                        text ""
                    , if List.length monedasDisponibles > 1 then
                        viewTabs userId resumen monedasDisponibles grupo.monedaPorDefecto tabActual

                      else
                        text ""
                    , case tabActual of
                        TabTotal ->
                            viewAvisoMonedasSinTasa grupo resumen

                        TabMoneda _ ->
                            text ""
                    , div [ class "pt-4 mb-4" ]
                        [ div [ class "mb-4" ]
                            [ div [ class "fw-bold mb-3" ] [ text "Netos" ]
                            , case netosMostrados of
                                Just netos ->
                                    div [ class "row g-3" ]
                                        [ div [ class "col-12 col-md-4" ]
                                            [ viewTuEstadoCard userId netos grupo grupo.monedaPorDefecto monedaMostrada ]
                                        , div [ class "col-6 col-md-4" ]
                                            [ viewNetoCard "Mayor pagador"
                                                (netos |> List.sortBy (\( _, m ) -> Monto.toFloat m) |> List.reverse |> List.head)
                                                grupo
                                                grupo.monedaPorDefecto
                                                monedaMostrada
                                                False
                                            ]
                                        , div [ class "col-6 col-md-4" ]
                                            [ viewNetoCard "Mayor deudor"
                                                (netos |> List.sortBy (\( _, m ) -> Monto.toFloat m) |> List.head)
                                                grupo
                                                grupo.monedaPorDefecto
                                                monedaMostrada
                                                False
                                            ]
                                        ]

                                Nothing ->
                                    text ""
                            ]
                        , div [ class "fw-bold mb-3" ] [ text "Estado del grupo" ]
                        , case netosMostrados of
                            Just netos ->
                                viewNetosBarras grupo netos

                            Nothing ->
                                text ""
                        ]
                    ]


{-| De qué lado de la transferencia estás: la plata viene hacia vos o sale de
vos. Con los netos minimizados una persona cae siempre de un solo lado, pero
las transferencias cargadas a mano pueden ponerte en los dos.
-}
type Rol
    = Cobro
    | Pago


{-| Todo lo que cambia entre cobrar y pagar es el texto, así que vive junto en
un solo lugar en vez de repartido en ifs por toda la vista.
-}
textosDe : Rol -> { etiquetaHero : String, colorHero : String, titulo : String, ayuda : String, columna : String, contador : String, accion : String, hecho : String }
textosDe rol =
    case rol of
        Cobro ->
            { etiquetaHero = "Te tienen que transferir"
            , colorHero = "text-success"
            , titulo = "Que debés recibir"
            , ayuda = "Colaborá con el grupo marcando las transferencias de dinero que recibiste, se reflejarán en los demás participantes y la cuenta general del grupo."
            , columna = "Origen e importe"
            , contador = "Recibidas"
            , accion = "Ya la recibí"
            , hecho = "Recibida"
            }

        Pago ->
            { etiquetaHero = "Te falta transferir"
            , colorHero = "text-danger"
            , titulo = "Que debés realizar"
            , ayuda = "Colaborá con el grupo marcando las transferencias de dinero que realizaste, se reflejarán en los demás participantes y la cuenta general del grupo."
            , columna = "Destino e importe"
            , contador = "Realizadas"
            , accion = "Ya la realicé"
            , hecho = "Realizada"
            }


{-| El otro extremo de la transferencia: de quién la cobrás o a quién le pagás.
-}
contraparte : Rol -> Api.Transferencia -> String
contraparte rol t =
    case rol of
        Cobro ->
            t.from

        Pago ->
            t.to


{-| Tu extremo de la transferencia, el que decide si la transferencia es tuya.
-}
miLado : Rol -> Api.Transferencia -> String
miLado rol t =
    case rol of
        Cobro ->
            t.to

        Pago ->
            t.from


{-| Cuándo pasó algo, para ordenar el feed. Las hechas tienen fecha propia; las
pendientes no guardan ninguna, pero se crean todas juntas al congelar el grupo,
así que la fecha de congelamiento es la que les corresponde.
-}
cuandoPaso : Grupo -> Api.Transferencia -> Maybe Posix
cuandoPaso grupo t =
    case t.saldadaAt of
        Just saldadaAt ->
            Just saldadaAt

        Nothing ->
            grupo.congeladoAt


{-| El resumen de un grupo congelado, en el lugar donde un grupo abierto muestra
los netos. Las deudas ya están decididas y los netos no se mueven más, así que
lo que va acá es cuánta plata te falta mover y con quién. Lo que viene haciendo
el resto está en la columna de al lado.
-}
viewGrupoCongelado : Zone -> Maybe String -> Grupo -> Api.ResumenCongelado -> Html Msg
viewGrupoCongelado zone userId grupo resumen =
    div [ class "d-flex flex-column gap-3" ]
        (case userId of
            Nothing ->
                [ Bs.alert Bs.AlertInfo
                    []
                    [ text "Seleccioná tu usuario para ver qué transferencias te tocan." ]
                ]

            Just uid ->
                let
                    mios : Rol -> List Api.Transferencia
                    mios rol =
                        resumen.transferencias
                            |> List.filter (\t -> miLado rol t == uid)
                            -- Por id, que no cambia cuando la marcás: la fila se
                            -- queda donde estaba en vez de saltar de lugar justo
                            -- cuando le acabás de dar al botón.
                            |> List.sortBy .id
                in
                viewSaldoPendiente grupo (mios Cobro) (mios Pago)
                    :: ([ Cobro, Pago ]
                            |> List.map (\rol -> ( rol, mios rol ))
                            |> List.filter (\( _, ms ) -> not (List.isEmpty ms))
                            |> List.map (\( rol, ms ) -> viewMisTransferenciasCard zone grupo rol ms)
                       )
        )


{-| Lo primero que se ve: cuánta plata falta que se mueva de tu lado. Es la suma
de lo pendiente, no el neto, porque una vez congelado el grupo cada
transferencia se marca entera y por separado.
-}
viewSaldoPendiente : Grupo -> List Api.Transferencia -> List Api.Transferencia -> Html Msg
viewSaldoPendiente grupo aCobrar aPagar =
    let
        lineas : List ( Rol, List ( Moneda, Api.Monto ) )
        lineas =
            [ ( Cobro, totalPendiente aCobrar ), ( Pago, totalPendiente aPagar ) ]
                |> List.filter (\( _, totales ) -> not (List.isEmpty totales))
    in
    if List.isEmpty lineas then
        Bs.alert Bs.AlertSuccess
            [ class "mb-0" ]
            [ i [ class "bi bi-check2-circle me-2" ] []
            , text "Estás al día: no te queda ninguna transferencia pendiente."
            ]

    else
        div [ class "card border-2 border-dark-subtle" ]
            [ div [ class "card-body d-flex justify-content-center py-3" ]
                [ div [ class "position-relative text-center" ]
                    (viewIconoTransferencia
                        :: (lineas |> List.map (viewLineaSaldo grupo))
                    )
                ]
            ]


viewIconoTransferencia : Html Msg
viewIconoTransferencia =
    Svg.svg
        [ SvgAttr.class "position-absolute top-50 end-100 translate-middle-y me-3 text-body-secondary"
        , SvgAttr.width "25"
        , SvgAttr.height "24"
        , Attr.attribute "aria-hidden" "true"
        ]
        [ Svg.use [ SvgAttr.xlinkHref "/transferencia.svg#transferencia" ] [] ]


viewLineaSaldo : Grupo -> ( Rol, List ( Moneda, Api.Monto ) ) -> Html Msg
viewLineaSaldo grupo ( rol, totales ) =
    let
        textos =
            textosDe rol
    in
    div []
        (viewEtiqueta [] [ text textos.etiquetaHero ]
            :: (totales
                    |> List.map
                        (\( moneda, monto ) ->
                            div [ class ("fs-4 fw-bold " ++ textos.colorHero) ]
                                [ text (Moneda.simbolo grupo.monedaPorDefecto moneda)
                                , text " "
                                , text (Monto.toString monto)
                                ]
                        )
               )
        )


{-| Suma de lo que todavía no se marcó, una entrada por moneda y en el orden en
que aparecieron. Un grupo congelado suele tener una sola, pero nada lo obliga.
-}
totalPendiente : List Api.Transferencia -> List ( Moneda, Api.Monto )
totalPendiente transferencias =
    transferencias
        |> List.filter (not << Transferencia.estaHecha)
        |> List.foldl
            (\t acumulado ->
                if acumulado |> List.any (\( moneda, _ ) -> moneda == t.moneda) then
                    acumulado
                        |> List.map
                            (\( moneda, total ) ->
                                if moneda == t.moneda then
                                    ( moneda, Monto.add total t.monto )

                                else
                                    ( moneda, total )
                            )

                else
                    acumulado ++ [ ( t.moneda, t.monto ) ]
            )
            []


botonDe : Rol -> Bs.BtnVariant
botonDe rol =
    case rol of
        Pago ->
            Bs.Primary

        Cobro ->
            Bs.SecondarySolid


{-| El contador dice cómo venís sin leer la tabla: verde si ya están todas
hechas, rojo si falta que transfieras vos, amarillo si lo único que falta es que
te transfieran —eso no depende de vos, así que no es para alarmarse—.
-}
colorDelContador : Rol -> List Api.Transferencia -> String
colorDelContador rol transferencias =
    if transferencias |> List.all Transferencia.estaHecha then
        "text-bg-success"

    else
        case rol of
            Pago ->
                "text-bg-danger"

            Cobro ->
                "text-bg-warning"


{-| Las transferencias de un lado tuyo, con el botón para marcarlas. Pendientes
y hechas van en la misma lista —y en el mismo orden siempre— para que marcar una
no reacomode las de abajo justo cuando estás tocando la pantalla.
-}
viewMisTransferenciasCard : Zone -> Grupo -> Rol -> List Api.Transferencia -> Html Msg
viewMisTransferenciasCard zone grupo rol transferencias =
    let
        textos =
            textosDe rol

        hechas =
            transferencias |> List.filter Transferencia.estaHecha |> List.length
    in
    Bs.card []
        [ Bs.cardBody []
            [ div [ class "d-flex justify-content-between align-items-center gap-2" ]
                [ viewEtiqueta [] [ text "Tus transferencias" ]
                , div [ class "d-flex align-items-center gap-2" ]
                    [ span [ class "text-body-secondary small" ] [ text textos.contador ]
                    , Bs.badge (colorDelContador rol transferencias ++ " fw-normal")
                        []
                        [ text (String.fromInt hechas ++ " / " ++ String.fromInt (List.length transferencias)) ]
                    ]
                ]
            , div [ class "fs-5 fw-bold" ] [ text textos.titulo ]
            , p [ class "text-body-secondary small mb-0" ] [ text textos.ayuda ]
            , table [ class "table align-middle mb-0" ]
                [ thead []
                    [ tr []
                        [ th [ class "fw-normal text-body-secondary small" ] [ text textos.columna ]
                        , th [ class "fw-normal text-body-secondary small text-end" ] [ text "Acción" ]
                        ]
                    ]
                , tbody []
                    (transferencias
                        |> List.map (viewFilaTransferencia zone grupo rol)
                    )
                ]
            ]
        ]


viewFilaTransferencia : Zone -> Grupo -> Rol -> Api.Transferencia -> Html Msg
viewFilaTransferencia zone grupo rol t =
    let
        textos =
            textosDe rol
    in
    tr []
        [ td []
            [ div [ class "fw-semibold text-truncate" ]
                [ text (lookupNombreParticipante grupo (contraparte rol t)) ]
            , div [ class "text-body-secondary" ]
                [ text (Moneda.simbolo grupo.monedaPorDefecto t.moneda)
                , text " "
                , text (Monto.toString t.monto)
                ]
            ]
        , td [ class "text-end" ]
            [ case Transferencia.estado t of
                Transferencia.Pendiente ->
                    Bs.btn (botonDe rol)
                        [ class "btn-sm text-nowrap"
                        , onClick (PedirConfirmacion t)
                        ]
                        [ text textos.accion ]

                Transferencia.Hecha saldadaAt ->
                    span
                        [ class "text-success small"
                        , Attr.title (Posix.toString zone saldadaAt)
                        ]
                        [ text textos.hecho ]
            ]
        ]


{-| El feed del grupo entero, tuyo y ajeno. Sirve para saber si la cosa se está
moviendo sin tener que entrar a la pantalla de transferencias. Muestra solo las
últimas y no lleva a la lista completa: desde el resumen lo único que hay para
hacer es marcar lo tuyo, y editar las transferencias del grupo es otra cosa.
Ocupa el lugar —y la forma— que tiene "Ultimos gastos" con el grupo abierto.
-}
viewUltimasTransferenciasCard : Zone -> Posix -> Grupo -> List Api.Transferencia -> Html Msg
viewUltimasTransferenciasCard zone ahora grupo transferencias =
    if List.isEmpty transferencias then
        text ""

    else
        let
            ultimas =
                transferencias
                    |> List.sortBy
                        (\t ->
                            cuandoPaso grupo t
                                |> Maybe.map Time.posixToMillis
                                |> Maybe.withDefault 0
                        )
                    |> List.reverse
                    |> List.take 5
        in
        Bs.card []
            [ Bs.cardHeader [] [ text "Ultimas transferencias" ]
            , Bs.listGroup [ class "list-group-flush" ]
                (ultimas |> List.map (viewActualizacion zone ahora grupo))
            ]


viewActualizacion : Zone -> Posix -> Grupo -> Api.Transferencia -> Html Msg
viewActualizacion zone ahora grupo t =
    Bs.listGroupItem [ class "px-3 py-2" ]
        [ div [ class "d-flex align-items-center gap-2 mb-1" ]
            [ case Transferencia.estado t of
                Transferencia.Hecha _ ->
                    Bs.badge "text-bg-success-subtle text-success-emphasis fw-normal" [] [ text "Realizada" ]

                Transferencia.Pendiente ->
                    Bs.badge "text-bg-secondary-subtle text-secondary-emphasis fw-normal" [] [ text "Pendiente" ]
            , case cuandoPaso grupo t of
                Just instante ->
                    span
                        [ class "text-body-secondary small"
                        , Attr.title (Posix.toString zone instante)
                        ]
                        [ text (Posix.relativo ahora instante) ]

                Nothing ->
                    text ""
            ]
        , div [ class "small" ]
            (Transferencia.frase grupo t)
        ]


{-| El rótulo chiquito en mayúsculas que encabeza cada tarjeta.
-}
viewEtiqueta : List (Html.Attribute Msg) -> List (Html Msg) -> Html Msg
viewEtiqueta attrs children =
    div
        (class "text-body-secondary text-uppercase fw-semibold"
            :: style "font-size" "0.7rem"
            :: style "letter-spacing" "0.05em"
            :: attrs
        )
        children


{-| Marcar una transferencia dice que la plata ya se movió, y el resto del grupo
lo ve como hecho, así que primero se relee en voz alta quién le transfirió qué a
quién.
-}
viewConfirmacionModal : Maybe String -> Grupo -> Maybe Api.Transferencia -> Html Msg
viewConfirmacionModal userId grupo confirmando =
    let
        ( pregunta, accion ) =
            case confirmando of
                Just t ->
                    ( if userId == Just t.from then
                        [ text "¿Le transferiste "
                        , Transferencia.monto grupo.monedaPorDefecto t
                        , text " a "
                        , Transferencia.participante grupo t.to
                        , text "?"
                        ]

                      else
                        [ text "¿Recibiste "
                        , Transferencia.monto grupo.monedaPorDefecto t
                        , text " de "
                        , Transferencia.participante grupo t.from
                        , text "?"
                        ]
                    , Just (SaldarTransferencia t.id)
                    )

                Nothing ->
                    ( [], Nothing )
    in
    Bs.modal
        { isOpen = confirmando /= Nothing
        , onClose = CancelarConfirmacion
        , title = "Confirmar transferencia"
        , centered = True
        , body = [ p [] pregunta ]
        , footer =
            [ Bs.btn Bs.Secondary
                [ onClick CancelarConfirmacion ]
                [ text "Todavía no" ]
            , case accion of
                Just msg ->
                    Bs.btn Bs.Primary [ onClick msg ] [ text "Sí, ya está" ]

                Nothing ->
                    text ""
            ]
        }


{-| Un neto mostrado como delta: el símbolo de la moneda apagado (para que no
compita) seguido del monto con signo y color (verde/rojo).
-}
viewMontoDelta : String -> Api.Monto -> Html Msg
viewMontoDelta simbolo monto =
    div [ class "small" ]
        [ span [ class "text-muted me-1" ] [ text simbolo ]
        , span [ class "fw-semibold" ] [ Monto.asDeltaHtml monto ]
        ]


viewAvisoMonedasSinTasa : Grupo -> Api.ResumenAbierto -> Html Msg
viewAvisoMonedasSinTasa grupo resumen =
    case resumen.consolidado.monedasSinTasa of
        [] ->
            text ""

        monedas ->
            Bs.alert Bs.AlertWarning
                [ class "mt-3 mb-0" ]
                [ text <|
                    (if List.length monedas == 1 then
                        "Falta la tasa de cambio de "

                     else
                        "Faltan las tasas de cambio de "
                    )
                        ++ (monedas |> List.map Moneda.nombre |> String.join ", ")
                        ++ ", así que esas deudas no entran en el total. Cargalas "
                , a [ Path.href <| Path.Grupos_GrupoId__Settings { grupoId = grupo.id } ]
                    [ text "en los ajustes del grupo" ]
                , text "."
                ]


viewTabs : Maybe String -> Api.ResumenAbierto -> List Moneda -> Moneda -> Tab -> Html Msg
viewTabs userId resumen monedas monedaPorDefecto tabActual =
    let
        netoDe : Netos Api.Monto -> Maybe Api.Monto
        netoDe netos =
            userId
                |> Maybe.andThen
                    (\uid -> netos |> List.filter (\( id, _ ) -> id == uid) |> List.head)
                |> Maybe.map Tuple.second

        tabDeMoneda : Moneda -> Html Msg
        tabDeMoneda moneda =
            viewTab (TabMoneda moneda)
                tabActual
                (Moneda.nombre moneda)
                (Moneda.simbolo monedaPorDefecto moneda)
                (resumen.netos
                    |> List.filter (\( m, _ ) -> m == moneda)
                    |> List.head
                    |> Maybe.map Tuple.second
                    |> Maybe.andThen netoDe
                )

        tabDelTotal : Html Msg
        tabDelTotal =
            viewTab TabTotal
                tabActual
                "Total"
                (Moneda.simbolo monedaPorDefecto resumen.consolidado.moneda)
                (netoDe resumen.consolidado.netos)
    in
    -- On desktop these are plain nav-tabs. On mobile `.moneda-tabs` (see
    -- styles.css) makes them fill the width and scroll horizontally instead
    -- of wrapping.
    ul
        [ class "nav nav-tabs moneda-tabs" ]
        (tabDelTotal :: (monedas |> List.map tabDeMoneda))


viewTab : Tab -> Tab -> String -> String -> Maybe Api.Monto -> Html Msg
viewTab tab tabActual etiqueta simbolo netoUsuario =
    li [ class "nav-item" ]
        [ button
            [ type_ "button"
            , classList [ ( "nav-link", True ), ( "active", tab == tabActual ) ]
            , class "text-nowrap"
            , onClick (SelectTab tab)
            ]
            [ div [] [ text etiqueta ]
            , case netoUsuario of
                Just monto ->
                    viewMontoDelta simbolo monto

                Nothing ->
                    text ""
            ]
        ]


viewUltimosPagosCard : Maybe ULID -> Store -> Model -> Grupo -> Html Msg
viewUltimosPagosCard participanteId store model grupo =
    case store |> Store.getPagos model.grupoId of
        Success pagos ->
            let
                ultimosPagos =
                    pagos
                        |> List.sortWith (\p1 p2 -> compare p2.pagoId p1.pagoId)
                        |> List.take 5
            in
            Bs.card []
                [ Bs.cardHeader [] [ text "Ultimos gastos" ]
                , Bs.listGroup [ class "list-group-flush" ]
                    (ultimosPagos |> List.map (viewUltimoPago participanteId grupo.id grupo.monedaPorDefecto))
                ]

        _ ->
            text ""


viewUltimoPago : Maybe ULID -> ULID -> Moneda -> ShallowPago -> Html Msg
viewUltimoPago participanteId grupoId monedaPorDefecto pago =
    Bs.listGroupItem
        [ class "list-group-item-action p-0" ]
        [ a
            [ class "d-flex align-items-center gap-3 p-3 text-reset text-decoration-none"
            , PagoDetalleModal.hrefPago (Path.Grupos_Id_ { id = grupoId }) pago.pagoId
            ]
            [ div
                [ class "text-center border rounded px-2 py-1 flex-shrink-0"
                , style "min-width" "2.5rem"
                ]
                [ div [ class "text-muted text-uppercase lh-1", style "font-size" "0.6em" ]
                    [ text (Utils.Day.mesAbreviado pago.fecha) ]
                , div [ class "fw-bold lh-1" ] [ text (String.fromInt (Date.day pago.fecha)) ]
                ]
            , ResumenGasto.viewFila participanteId monedaPorDefecto pago
            , ResumenGasto.viewIconoInvalido pago
            , ResumenGasto.viewBadgeRepartija pago
            ]
        ]


viewNetoCard : String -> Maybe ( String, Api.Monto ) -> GrupoLike g -> Moneda -> Moneda -> Bool -> Html Msg
viewNetoCard label maybeEntry grupo monedaPorDefecto monedaSeleccionada isCurrentUser =
    div
        [ class "card h-100"
        , if isCurrentUser then
            style "border-color" "var(--bs-primary)"

          else
            style "" ""
        ]
        [ div [ class "card-body d-flex flex-column justify-content-between p-3" ]
            [ div [ class "text-muted text-uppercase fw-semibold", style "font-size" "0.65rem", style "letter-spacing" "0.05em" ] [ text label ]
            , case maybeEntry of
                Just ( participanteId, monto ) ->
                    div []
                        [ div [ class "fw-semibold text-truncate" ]
                            [ text (lookupNombreParticipante grupo participanteId) ]
                        , viewMontoDelta (Moneda.simbolo monedaPorDefecto monedaSeleccionada) monto
                        ]

                Nothing ->
                    text ""
            ]
        ]


viewTuEstadoCard : Maybe String -> Api.Netos Api.Monto -> GrupoLike g -> Moneda -> Moneda -> Html Msg
viewTuEstadoCard userId netos grupo monedaPorDefecto monedaSeleccionada =
    let
        maybeEntry =
            userId
                |> Maybe.andThen
                    (\uid ->
                        netos |> List.filter (\( id, _ ) -> id == uid) |> List.head
                    )
    in
    div [ class "card h-100", style "border-color" "var(--bs-primary)" ]
        [ div [ class "card-body d-flex flex-column justify-content-between p-3" ]
            [ div [ class "text-muted text-uppercase fw-semibold", style "font-size" "0.65rem", style "letter-spacing" "0.05em" ] [ text "Tu estado actual" ]
            , case maybeEntry of
                Just ( _, monto ) ->
                    div []
                        [ viewMontoDelta (Moneda.simbolo monedaPorDefecto monedaSeleccionada) monto
                        , if monto.valor < 0 then
                            a
                                [ Path.href <| Path.Grupos_GrupoId__Transferencias { grupoId = grupo.id }
                                , class "small text-muted"
                                ]
                                [ text "Saldar deudas" ]

                          else
                            text ""
                        ]

                Nothing ->
                    div [ class "text-muted small" ] [ text "Seleccioná tu usuario" ]
            ]
        ]
