module Pages.Grupos.Id_ exposing (Model, Msg, page)

import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.NavBar as NavBar exposing (modelFromShared)
import Components.Ui5 as Ui5
import Effect exposing (Effect)
import Generated.Api as Api exposing (Pago, ResumenGrupo, ShallowGrupo, Transaccion, ULID)
import Html exposing (Html, a, div, p, section, span, text)
import Html.Attributes as Attr exposing (class, style)
import Html.Events exposing (onClick)
import Http
import Json.Encode as Encode
import Layouts
import Models.Grupo exposing (GrupoLike, lookupNombreParticipante)
import Models.Monto as Monto
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
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
        , Effect.setUnsavedChangesWarning False
        ]
    )


type Msg
    = CrearPago Pago
    | AddedPagoResponse (Result Http.Error Pago)
    | FreezeGrupo
    | FreezeGrupoResponse (Result Http.Error ShallowGrupo)
    | UnfreezeGrupo
    | UnfreezeGrupoResponse (Result Http.Error ShallowGrupo)
    | SaldarTransaccion ULID Pago
    | SaldadaTransaccionResponse (Result Http.Error Pago)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        AddedPagoResponse (Ok _) ->
            ( model
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshGrupo model.grupoId
                , Store.refreshPagos model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Se completó el pago"
                ]
            )

        AddedPagoResponse (Err _) ->
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

        SaldarTransaccion transaccionId pago ->
            ( model
            , Effect.sendCmd <|
                Api.postGrupoByIdTransaccionescongeladasByTransaccionIdSaldar
                    model.grupoId
                    transaccionId
                    pago
                    SaldadaTransaccionResponse
            )

        SaldadaTransaccionResponse (Ok _) ->
            ( model
            , Effect.batch
                [ Store.refreshResumen model.grupoId
                , Store.refreshGrupo model.grupoId
                , Store.refreshPagos model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Se completó el pago"
                ]
            )

        SaldadaTransaccionResponse (Err _) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "No se pudo completar el pago"
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
subscriptions _ =
    Sub.none


view : Store -> Model -> View Msg
view store model =
    case store |> Store.getGrupo model.grupoId of
        NotAsked ->
            { title = "Loading..."
            , body = []
            }

        Loading ->
            { title = "Cargando"
            , body =
                [ div [ class "container" ]
                    [ section [ class "section" ]
                        [ Ui5.text "Cargando..."
                        ]
                    ]
                ]
            }

        Failure _ ->
            { title = "Fallo"
            , body = []
            }

        Success grupo ->
            { title = grupo.nombre
            , body =
                [ div
                    [ style "max-width" "var(--sapBreakpoint_L_Min)"
                    , style "display" "flex"
                    , style "justify-content" "center"
                    , style "flex-direction" "column"
                    , style "margin-left" "auto"
                    , style "margin-right" "auto"
                    ]
                    (if List.isEmpty grupo.participantes then
                        [ p [] [ Ui5.text "Tu grupo todavía no tiene participantes!" ]
                        , p []
                            [ Ui5.text "Agregalos "
                            , a
                                [ Path.href <| Path.Grupos_GrupoId__Participantes { grupoId = grupo.id }
                                ]
                                [ text "acá" ]
                            ]
                        ]

                     else
                        case store |> Store.getResumen model.grupoId of
                            Success resumen ->
                                if resumen.cantidadPagos == 0 then
                                    [ Ui5.messageStrip
                                        [ Attr.attribute "design" "Information", style "text-align" "center" ]
                                        [ text "Todavía no hay pagos registrados. "
                                        , a [ Path.href <| Path.Grupos_GrupoId__Pagos_New { grupoId = grupo.id } ]
                                            [ text "¡Agregá el primer pago para empezar a dividir gastos!" ]
                                        ]
                                    ]

                                else
                                    [ if resumen.cantidadPagosInvalidos > 0 then
                                        Ui5.messageStrip
                                            [ Attr.attribute "design" "Negative", style "margin-bottom" "1rem" ]
                                            [ text <|
                                                if resumen.cantidadPagosInvalidos == 1 then
                                                    "Tenés 1 pago inválido, ese no se cuenta para las deudas."

                                                else
                                                    "Tenés "
                                                        ++ String.fromInt resumen.cantidadPagosInvalidos
                                                        ++ " pagos inválidos, esos no se cuentan para las deudas."
                                            ]

                                      else
                                        text ""
                                    , if resumen.isFrozen then
                                        Ui5.messageStrip
                                            [ Attr.attribute "design" "Warning"
                                            , style "margin-bottom" "1rem"
                                            , Attr.property "hideCloseButton" (Encode.bool True)
                                            ]
                                            [ text "Este grupo está congelado. Las deudas están fijas y no se pueden agregar, editar ni eliminar pagos." ]

                                      else
                                        text ""
                                    , div [ style "width" "100%", style "margin-bottom" "1.5rem" ]
                                        [ viewNetosBarras grupo resumen.netos ]
                                    , viewTransferencias grupo resumen
                                    , viewFreezeActions grupo resumen
                                    ]

                            NotAsked ->
                                [ Ui5.text "Carganding" ]

                            Loading ->
                                [ Ui5.text "Carganding" ]

                            Failure _ ->
                                [ Ui5.text "Error cargando los netos" ]
                    )
                ]
            }


viewFreezeActions : GrupoLike g -> ResumenGrupo -> Html Msg
viewFreezeActions grupo resumen =
    div [ style "display" "flex", style "justify-content" "flex-end", style "margin-top" "1rem" ]
        [ if resumen.isFrozen then
            Ui5.button
                [ Attr.attribute "design" "Default"
                , Attr.attribute "icon" "unlocked"
                , onClick UnfreezeGrupo
                ]
                [ text "Descongelar" ]

          else if resumen.cantidadPagos > 0 then
            Ui5.button
                [ Attr.attribute "design" "Default"
                , Attr.attribute "icon" "locked"
                , onClick FreezeGrupo
                ]
                [ text "Congelar" ]

          else
            text ""
        ]


viewTransferencias : GrupoLike g -> ResumenGrupo -> Html Msg
viewTransferencias grupo resumen =
    div []
        (if List.isEmpty resumen.transaccionesParaSaldar then
            [ Ui5.messageStrip
                [ Attr.attribute "design" "Positive"
                , style "text-align" "center"
                , Attr.property "hideCloseButton" (Encode.bool True)
                ]
                [ text "¡No hay deudas pendientes! Todos están al día." ]
            ]

         else
            resumen.transaccionesParaSaldar
                |> List.map
                    (\t ->
                        div [ style "display" "grid", style "grid-template-columns" "1fr auto 1fr", style "align-items" "center", style "margin-bottom" "0.5rem" ]
                            [ div [ style "text-align" "right" ]
                                [ div [] [ Ui5.text <| lookupNombreParticipante grupo t.transaccionFrom ]
                                , div [ style "color" "var(--sapNegativeTextColor)" ]
                                    [ text "$"
                                    , text <| Monto.toString t.transaccionMonto
                                    ]
                                ]
                            , Ui5.button
                                [ Attr.attribute "design" "Transparent"
                                , Attr.attribute "icon" "arrow-right"
                                , Attr.attribute "tooltip" "Crear pago para saldar esta deuda"
                                , onClick <|
                                    case t.transaccionId of
                                        Just transaccionId ->
                                            SaldarTransaccion transaccionId (pagoFromTransaccion t)

                                        Nothing ->
                                            CrearPago (pagoFromTransaccion t)
                                , style "margin" "0 0.5rem"
                                ]
                                []
                            , span []
                                [ Ui5.text <| lookupNombreParticipante grupo t.transaccionTo
                                ]
                            ]
                    )
        )
