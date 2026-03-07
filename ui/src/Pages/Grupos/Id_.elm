module Pages.Grupos.Id_ exposing (Model, Msg, page)

import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.NavBar as NavBar exposing (modelFromShared)
import Components.Ui5 as Ui5
import Effect exposing (Effect)
import Generated.Api as Api exposing (Pago, ResumenGrupo, ShallowGrupo, ShallowPago, Transaccion, ULID)
import Html exposing (Html, a, div, p, section, span, strong, text)
import Html.Attributes as Attr exposing (class, style)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode
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
        , update = update shared.store
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
    , deletingPagoId : Maybe ULID
    }


init : ULID -> Store -> ( Model, Effect Msg )
init grupoId store =
    ( { grupoId = grupoId
      , deletingPagoId = Nothing
      }
    , Effect.batch
        [ Store.ensureResumen grupoId store
        , Store.ensureGrupo grupoId store
        , Store.ensurePagos grupoId store
        , Effect.getCurrentUser grupoId
        , Effect.setUnsavedChangesWarning False
        ]
    )


type Msg
    = CrearPago Pago
    | AddedPagoResponse (Result Http.Error Pago)
    | SaldarTransaccion ULID Pago
    | SaldadaTransaccionResponse (Result Http.Error Pago)
    | Navigate Path.Path
    | DeletePago ULID
    | ConfirmDeletePago ULID
    | CancelDeletePago
    | DeletePagoResponse (Result Http.Error ULID)


update : Store -> Msg -> Model -> ( Model, Effect Msg )
update store msg model =
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

        Navigate path ->
            ( model, Effect.pushRoutePath path )

        DeletePago pagoId ->
            ( { model | deletingPagoId = Just pagoId }
            , Effect.none
            )

        ConfirmDeletePago pagoId ->
            case store |> Store.getGrupo model.grupoId of
                Success grupo ->
                    ( { model | deletingPagoId = Nothing }
                    , Effect.sendCmd <| Api.deleteGrupoByIdPagosByPagoId grupo.id pagoId DeletePagoResponse
                    )

                _ ->
                    ( model, Effect.none )

        CancelDeletePago ->
            ( { model | deletingPagoId = Nothing }
            , Effect.none
            )

        DeletePagoResponse (Ok _) ->
            ( model
            , Effect.batch
                [ Store.refreshGrupo model.grupoId
                , Store.refreshResumen model.grupoId
                , Store.refreshPagos model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Pago borrado"
                ]
            )

        DeletePagoResponse (Err _) ->
            ( model, Toasts.pushToast Toasts.ToastDanger "Falle al borrar el pago" )


pagoFromTransaccion : Transaccion -> Pago
pagoFromTransaccion transaction =
    { pagoId = emptyUlid
    , isValid = False
    , nombre = "Pago saldado"
    , monto = transaction.monto
    , pagadores =
        { id = emptyUlid
        , tipo =
            Api.TipoDistribucionMontosEspecificos <|
                { id = emptyUlid
                , montos =
                    [ { id = emptyUlid
                      , participante = transaction.from
                      , monto = transaction.monto
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
                      , participante = transaction.to
                      , monto = transaction.monto
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
                        [ viewResumenPanel store model grupo
                        , viewPagosPanel store model
                        ]
                    )
                ]
            }


viewResumenPanel : Store -> Model -> ShallowGrupo -> Html Msg
viewResumenPanel store model grupo =
    Ui5.panel
        [ Attr.attribute "header-text" "Resumen"
        , Attr.attribute "fixed" ""
        , style "margin-bottom" "1rem"
        ]
        (case store |> Store.getResumen model.grupoId of
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
                    ]

            NotAsked ->
                [ Ui5.text "Cargando..." ]

            Loading ->
                [ Ui5.text "Cargando..." ]

            Failure _ ->
                [ Ui5.text "Error cargando los netos" ]
        )


viewPagosPanel : Store -> Model -> Html Msg
viewPagosPanel store model =
    case store |> Store.getPagos model.grupoId of
        Success pagos ->
            let
                pagoBeingDeleted =
                    model.deletingPagoId
                        |> Maybe.andThen
                            (\pagoId ->
                                pagos
                                    |> List.filter (\pg -> pg.pagoId == pagoId)
                                    |> List.head
                            )
            in
            div []
                [ Ui5.list
                    [ Attr.attribute "selection-mode" "Delete"
                    , on "item-delete"
                        (Json.Decode.at [ "detail", "item", "dataset", "id" ] Json.Decode.string
                            |> Json.Decode.map DeletePago
                        )
                    ]
                    (div
                        [ Ui5.slot "header"
                        , style "display" "flex"
                        , style "align-items" "center"
                        , style "justify-content" "space-between"
                        , style "width" "100%"
                        ]
                        [ Ui5.title [ Attr.attribute "level" "H4" ] [ text "Pagos" ]
                        , Ui5.button
                            [ Attr.attribute "design" "Emphasized"
                            , Attr.attribute "icon" "add"
                            , onClick (Navigate <| Path.Grupos_GrupoId__Pagos_New { grupoId = model.grupoId })
                            ]
                            [ text "Agregar pago" ]
                        ]
                        :: (pagos
                                |> List.map
                                    (\pago ->
                                        Ui5.li
                                            [ Attr.attribute "data-id" pago.pagoId
                                            , Attr.attribute "additional-text" ("$ " ++ Monto.toString pago.monto)
                                            , Attr.attribute "type" "Navigation"
                                            , Attr.attribute "icon"
                                                (if pago.isValid then
                                                    ""

                                                 else
                                                    "alert"
                                                )
                                            , onClick (Navigate <| Path.Grupos_GrupoId__Pagos_PagoId_ { grupoId = model.grupoId, pagoId = pago.pagoId })
                                            ]
                                            [ text pago.nombre ]
                                    )
                           )
                    )
                , deleteConfirmationDialog model.deletingPagoId pagoBeingDeleted
                ]

        _ ->
            text ""


deleteConfirmationDialog : Maybe ULID -> Maybe ShallowPago -> Html Msg
deleteConfirmationDialog deletingPagoId maybePago =
    Ui5.dialog
        [ Attr.attribute "header-text" "Confirmar"
        , Attr.attribute "state" "Negative"
        , if deletingPagoId /= Nothing then
            Attr.attribute "open" ""

          else
            class ""
        , on "close" (Json.Decode.succeed CancelDeletePago)
        ]
        [ p []
            [ Ui5.text "¿Estás seguro que querés eliminar "
            , case maybePago of
                Just pago ->
                    strong [] [ Ui5.text ("\"" ++ pago.nombre ++ "\"") ]

                Nothing ->
                    Ui5.text "este pago"
            , Ui5.text "?"
            ]
        , p [ style "margin-top" "0.75rem" ]
            [ Ui5.text "Esta acción no se puede deshacer." ]
        , div [ Ui5.slot "footer", style "display" "flex", style "gap" "0.5rem", style "justify-content" "end", style "width" "100%", style "padding" "0.25rem 0" ]
            [ Ui5.button
                [ Attr.attribute "design" "Transparent"
                , onClick CancelDeletePago
                ]
                [ text "Cancelar" ]
            , Ui5.button
                [ Attr.attribute "design" "Negative"
                , onClick <|
                    case deletingPagoId of
                        Just pagoId ->
                            ConfirmDeletePago pagoId

                        Nothing ->
                            Navigate (Path.Grupos_Id_ { id = "" })
                ]
                [ text "Eliminar" ]
            ]
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
                                [ div [] [ Ui5.text <| lookupNombreParticipante grupo t.from ]
                                , div [ style "color" "var(--sapNegativeTextColor)" ]
                                    [ text "$"
                                    , text <| Monto.toString t.monto
                                    ]
                                ]
                            , Ui5.button
                                [ Attr.attribute "design" "Transparent"
                                , Attr.attribute "icon" "arrow-right"
                                , Attr.attribute "tooltip" "Crear pago para saldar esta deuda"
                                , onClick <|
                                    case t.id of
                                        Just transaccionId ->
                                            SaldarTransaccion transaccionId (pagoFromTransaccion t)

                                        Nothing ->
                                            CrearPago (pagoFromTransaccion t)
                                , style "margin" "0 0.5rem"
                                ]
                                []
                            , span []
                                [ Ui5.text <| lookupNombreParticipante grupo t.to
                                ]
                            ]
                    )
        )
