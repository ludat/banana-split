module Pages.Grupos.GrupoId_.Gastos exposing (Model, Msg, page)

import Components.Bootstrap as Bs
import Components.ErrorDePagina as ErrorDePagina
import Components.Movimientos as Movimientos
import Components.PagoDetalleModal as PagoDetalleModal
import Dict
import Effect exposing (Effect)
import Generated.Api exposing (Grupo, ULID)
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, id)
import Http
import Layouts
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Route.Path as Path
import Shared
import Time exposing (Zone)
import Utils.Http exposing (viewHttpError)
import Utils.Telemetry as Telemetry
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route shared
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


init : Route { grupoId : String } -> Shared.Model -> ( Model, Effect Msg )
init route shared =
    let
        grupoId =
            route.params.grupoId
    in
    ( { grupoId = grupoId }
    , Effect.batch
        [ Store.ensureGrupo grupoId shared.store
        , Store.ensureTransferencias grupoId
        , Store.ensurePagos grupoId shared.store
        , Shared.currentParticipante shared grupoId
            |> Maybe.map (\participanteId -> Store.ensurePagadoYConsumido grupoId participanteId shared.store)
            |> Maybe.withDefault Effect.none
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Zone -> Maybe ULID -> Store -> Model -> View Msg
view zone participanteId store model =
    case store |> Store.getGrupo model.grupoId of
        NotAsked ->
            { title = "Cargando"
            , body = [ div [ class "container-fluid py-4 text-muted" ] [ text "Cargando..." ] ]
            }

        Loading ->
            { title = "Cargando"
            , body = [ div [ class "container-fluid py-4 text-muted" ] [ text "Cargando..." ] ]
            }

        Failure (Http.BadStatus 404) ->
            { title = "Grupo inexistente"
            , body =
                [ ErrorDePagina.view
                    { tipo = ErrorDePagina.NoExiste
                    , titulo = "Este grupo no existe"
                    , detalle = [ text "Revisá que el link esté bien." ]
                    }
                ]
            }

        Failure e ->
            { title = "Error"
            , body =
                [ ErrorDePagina.view
                    { tipo = ErrorDePagina.Fallo
                    , titulo = "No pudimos cargar los gastos del grupo"
                    , detalle = [ text "Recargá la página para reintentar.", viewHttpError e ]
                    }
                ]
            }

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

        Failure e ->
            Bs.alert Bs.AlertDanger [] [ text "No pudimos cargar los gastos.", viewHttpError e ]

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
                                    , pagadoYConsumido =
                                        participanteId
                                            |> Maybe.map (\participante -> Store.getPagadoYConsumido model.grupoId participante store |> RemoteData.withDefault Dict.empty)
                                            |> Maybe.withDefault Dict.empty
                                    , grupo = grupo
                                    }
                                )
                        )
                    ]
