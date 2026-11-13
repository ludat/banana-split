module Pages.Grupos.GrupoId_.Gastos exposing (Model, Msg, page)

import Components.Bootstrap as Bs
import Components.Movimientos as Movimientos
import Components.PagoDetalleModal as PagoDetalleModal
import Effect exposing (Effect)
import Generated.Api exposing (Grupo, ULID)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, id)
import Layouts
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Route.Path as Path
import Shared
import Time exposing (Zone)
import Utils.Telemetry as Telemetry
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route shared.store
        , update = update
        , subscriptions = subscriptions
        , view = view shared.timezone (Shared.currentParticipante shared route.params.grupoId) shared.store
        }
        |> Page.withLayout (\_ -> Layouts.Default_Grupo {})


type alias Model =
    { grupoId : String
    }


type Msg
    = AbrirPopup PagoDetalleModal.Ruta


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        AbrirPopup ruta ->
            ( model, Effect.replaceRoute ruta )


init : Route { grupoId : String } -> Store -> ( Model, Effect Msg )
init route store =
    let
        grupoId =
            route.params.grupoId
    in
    ( { grupoId = grupoId }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Store.ensureTransferencias grupoId store
        , -- Los gastos los pide Shared cuando resuelve el participante, que
          -- es lo que este mensaje dispara.
          Effect.getCurrentUser grupoId
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Zone -> Maybe ULID -> Store -> Model -> View Msg
view zone participanteId store model =
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
                [ div [ id "gastos", class "container-fluid py-3" ]
                    [ viewMovimientos zone participanteId store model grupo ]
                ]
            }


viewMovimientos : Zone -> Maybe ULID -> Store -> Model -> Grupo -> Html Msg
viewMovimientos zone participanteId store model grupo =
    case
        RemoteData.map2 (\pagos transferencias -> Movimientos.mkMovimientos zone pagos transferencias |> Movimientos.cronologico)
            (store |> Store.getPagos model.grupoId)
            (store |> Store.getTransferencias model.grupoId)
    of
        NotAsked ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Loading ->
            div [ class "text-muted" ] [ text "Cargando..." ]

        Failure _ ->
            Bs.alert Bs.AlertDanger [] [ text "Error cargando los gastos." ]

        Success movimientos ->
            if List.isEmpty movimientos then
                Bs.alert Bs.AlertInfo
                    []
                    [ text "Todavía no hay gastos registrados. "
                    , a
                        (Telemetry.click "gastos.empty_new_gasto"
                            :: PagoDetalleModal.linkANuevoGasto AbrirPopup (Path.Grupos_GrupoId__Gastos { grupoId = grupo.id })
                        )
                        [ text "¡Agregá el primer gasto para empezar a dividir!" ]
                    ]

            else
                Bs.card [ id "gastos-lista" ]
                    [ Bs.listGroupKeyed [ class "list-group-flush" ]
                        (movimientos
                            |> List.map
                                (Movimientos.viewMovimiento
                                    { abrirPopup = AbrirPopup
                                    , desde = Path.Grupos_GrupoId__Gastos { grupoId = grupo.id }
                                    , telemetria = "gastos"
                                    , participanteId = participanteId
                                    , grupo = grupo
                                    }
                                )
                        )
                    ]
