module Pages.Grupos.GrupoId_.Gastos exposing (Model, Msg, page)

import Components.Bootstrap as Bs
import Components.PagoDetalleModal as PagoDetalleModal
import Components.ResumenGasto as ResumenGasto
import Date
import Effect exposing (Effect)
import Generated.Api exposing (Grupo, Moneda, ShallowPago, ULID)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, style)
import Layouts
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Route.Path as Path
import Shared
import Utils.Day
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route shared.store
        , update = update (PagoDetalleModal.context shared route) shared.store
        , subscriptions = subscriptions
        , view = view (Shared.currentParticipante shared route.params.grupoId) shared.store
        }
        |> Page.withLayout (\_ -> Layouts.Default_Grupo {})
        |> Page.withOnUrlChanged (PagoModalMsg << PagoDetalleModal.onUrlChanged)


type alias Model =
    { grupoId : String
    , pagoModal : PagoDetalleModal.Model
    }


init : Route { grupoId : String } -> Store -> ( Model, Effect Msg )
init route store =
    let
        grupoId =
            route.params.grupoId

        ( pagoModal, modalEffect ) =
            PagoDetalleModal.init route
    in
    ( { grupoId = grupoId, pagoModal = pagoModal }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , -- Los gastos los pide Shared cuando resuelve el participante, que
          -- es lo que este mensaje dispara.
          Effect.getCurrentUser grupoId
        , Effect.map PagoModalMsg modalEffect
        ]
    )


type Msg
    = NoOp
    | OpenPago ULID
    | PagoModalMsg PagoDetalleModal.Msg


update : PagoDetalleModal.Context -> Store -> Msg -> Model -> ( Model, Effect Msg )
update ctx store msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        OpenPago pagoId ->
            let
                ( pagoModal, eff ) =
                    PagoDetalleModal.open ctx pagoId
            in
            ( { model | pagoModal = pagoModal }, Effect.map PagoModalMsg eff )

        PagoModalMsg subMsg ->
            let
                ( pagoModal, eff ) =
                    PagoDetalleModal.update ctx store subMsg model.pagoModal
            in
            ( { model | pagoModal = pagoModal }, Effect.map PagoModalMsg eff )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Maybe ULID -> Store -> Model -> View Msg
view participanteId store model =
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
                [ div [ class "container-fluid py-3" ]
                    [ viewPagos participanteId store model grupo ]
                , Html.map PagoModalMsg (PagoDetalleModal.view store grupo model.pagoModal)
                ]
            }


viewPagos : Maybe ULID -> Store -> Model -> Grupo -> Html Msg
viewPagos participanteId store model grupo =
    case store |> Store.getPagos model.grupoId of
        NotAsked ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Loading ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Failure _ ->
            Bs.alert Bs.AlertDanger [] [ text "Error cargando los gastos." ]

        Success pagos ->
            if List.isEmpty pagos then
                Bs.alert Bs.AlertInfo
                    []
                    [ text "Todavía no hay gastos registrados. "
                    , a [ PagoDetalleModal.hrefNuevoGasto <| Path.Grupos_GrupoId__Gastos { grupoId = grupo.id } ]
                        [ text "¡Agregá el primer gasto para empezar a dividir!" ]
                    ]

            else
                Bs.card []
                    [ Bs.listGroupKeyed [ class "list-group-flush" ]
                        (pagos
                            |> List.sortWith (\a b -> Date.compare b.fecha a.fecha)
                            |> List.map
                                (\pago ->
                                    ( pago.pagoId, viewPago participanteId grupo.id grupo.monedaPorDefecto pago )
                                )
                        )
                    ]


viewPago : Maybe ULID -> ULID -> Moneda -> ShallowPago -> Html Msg
viewPago participanteId grupoId monedaPorDefecto pago =
    Bs.listGroupItem
        [ class "list-group-item-action p-0" ]
        [ a
            (class "d-flex align-items-center gap-3 p-3 text-reset text-decoration-none"
                :: PagoDetalleModal.linkAlPago
                    (Path.Grupos_GrupoId__Gastos { grupoId = grupoId })
                    pago.pagoId
                    { abrir = OpenPago pago.pagoId, ignorar = NoOp }
            )
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
