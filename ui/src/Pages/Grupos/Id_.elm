module Pages.Grupos.Id_ exposing (Model, Msg, page)

import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.NavBar as NavBar exposing (modelFromShared)
import Effect exposing (Effect)
import FeatherIcons as Icons
import Generated.Api as Api exposing (Grupo, Netos, Pago, Parte(..), ResumenGrupo, Transaccion, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Layouts
import Models.Grupo exposing (GrupoLike, lookupNombreParticipante)
import Models.Monto as Monto
import Models.Pago as Pago
import Models.Store as Store
import Models.Store.Types as Store exposing (Store)
import Numeric.Decimal as Decimal
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts
import Utils.Ulid exposing (emptyUlid)
import View exposing (View)


page : Shared.Model -> Route { id : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.id shared.store
        , update = update
        , subscriptions = subscriptions
        , view = view shared.store
        }
        |> Page.withLayout
            (\m ->
                Layouts.Default
                    { navBarContent = Just <| NavBar.navBar (modelFromShared shared route.params.id) shared.store route.path
                    , grupo = Store.getGrupo m.grupoId shared.store
                    }
            )


type alias Model =
    { grupoId : String
    }


init : ULID -> Store -> ( Model, Effect Msg )
init grupoId store =
    ( { grupoId = grupoId }
    , Effect.batch
        [ Store.ensureResumen grupoId store
        , Store.ensureGrupo grupoId store
        , Effect.getCurrentUser grupoId
        ]
    )


type Msg
    = NoOp
    | CrearPago Pago
    | AddedPagoResponse (Result Http.Error Pago)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        AddedPagoResponse (Ok pago) ->
            ( model
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshGrupo model.grupoId
                , Store.refreshPagos model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Se completó el pago"
                ]
            )

        AddedPagoResponse (Err error) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "No se pudo completar el pago"
            )

        CrearPago pago ->
            ( model
            , Effect.sendCmd <|
                Api.postGrupoByIdPagos
                    model.grupoId
                    pago
                    AddedPagoResponse
            )


pagoFromTransaccion : Transaccion -> Pago
pagoFromTransaccion transaction =
    { pagoId = emptyUlid
    , isValid = False
    , nombre = "Pago saldado"
    , monto = transaction.transaccionMonto
    , pagadores =
        { id = emptyUlid
        , tipo =
            Api.TipoDistribucionMontosEspecificos <|
                { id = emptyUlid
                , montos =
                    [ { id = emptyUlid
                      , participante = transaction.transaccionFrom
                      , monto = transaction.transaccionMonto
                      }
                    ]
                }
        }
    , deudores =
        { id = emptyUlid
        , tipo =
            Api.TipoDistribucionMontosEspecificos <|
                { id = emptyUlid
                , montos =
                    [ { id = emptyUlid
                      , participante = transaction.transaccionTo
                      , monto = transaction.transaccionMonto
                      }
                    ]
                }
        }
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Store -> Model -> View Msg
view store model =
    case store |> Store.getGrupo model.grupoId of
        NotAsked ->
            { title = "Impossible"
            , body = []
            }

        Loading ->
            { title = "Cargando"
            , body =
                [ div [ class "container" ]
                    [ section [ class "section" ]
                        [ text "Cargando..."
                        ]
                    ]
                ]
            }

        Failure e ->
            { title = "Fallo"
            , body = []
            }

        Success grupo ->
            { title = grupo.nombre
            , body =
                [ div [ class "container columns is-flex-direction-column is-align-items-center" ]
                    (if grupo.participantes == [] then
                        [ p [] [ text "Tu grupo todavía no tiene participantes!" ]
                        , p []
                            [ text "Agregalos "
                            , a
                                [ Path.href <| Path.Grupos_GrupoId__Participantes { grupoId = grupo.id }
                                ]
                                [ text "acá" ]
                            ]
                        ]

                     else
                        case store |> Store.getResumen model.grupoId of
                            Success resumen ->
                                [ if resumen.cantidadPagosInvalidos > 0 then
                                    div [ class "notification is-danger" ]
                                        [ text <|
                                            "Tenés "
                                                ++ String.fromInt resumen.cantidadPagosInvalidos
                                                ++ " pagos invalidos, esos no se cuentan para las deudas."
                                        ]

                                  else
                                    text ""
                                , div [ class "column is-two-thirds mb-6" ]
                                    [ viewNetosBarras grupo resumen.netos ]
                                , viewTransferencias grupo resumen
                                ]

                            NotAsked ->
                                [ text "Carganding" ]

                            Loading ->
                                [ text "Carganding" ]

                            Failure e ->
                                [ text "Error cargando los netos" ]
                    )
                ]
            }


viewTransferencias : GrupoLike g -> ResumenGrupo -> Html Msg
viewTransferencias grupo resumen =
    div [ class "column is-two-thirds" ]
        (resumen.transaccionesParaSaldar
            |> List.map
                (\t ->
                    div [ class "fixed-grid has-11-cols mb-2" ]
                        [ div [ class "grid" ]
                            [ div [ class "cell is-col-span-5 has-text-right" ]
                                [ p [ class "" ]
                                    [ text <| lookupNombreParticipante grupo t.transaccionFrom
                                    , p [ class "has-text-danger is-size-6-5" ]
                                        [ text "$"
                                        , text <| Monto.toString t.transaccionMonto
                                        ]
                                    ]
                                ]
                            , div [ class "cell is-col-span-1 is-flex is-justify-content-center is-align-items-center is-clickable" ]
                                [ span [ class "arrow-container" ] [ Icons.toHtml [ onClick <| CrearPago <| pagoFromTransaccion t ] Icons.arrowRight ] ]
                            , div [ class "cell is-col-span-5 is-flex is-align-items-center" ]
                                [ text <| lookupNombreParticipante grupo t.transaccionTo
                                ]
                            ]
                        ]
                )
        )
