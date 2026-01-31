module Pages.Grupos.GrupoId_.Pagos exposing (Model, Msg, page)

import Components.NavBar as NavBar
import Effect exposing (Effect)
import FeatherIcons as Icons
import Generated.Api as Api exposing (Distribucion, Grupo, Monto, Netos, Pago, Parte(..), Participante, ParticipanteId, ShallowGrupo, ShallowPago, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Layouts
import Models.Grupo exposing (lookupNombreParticipante)
import Models.Monto as Monto
import Models.Pago as Pago
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Utils.Toasts exposing (pushToast)
import Utils.Toasts.Types exposing (ToastLevel(..))
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.grupoId shared.store
        , update = update shared.store shared.userId
        , subscriptions = subscriptions
        , view = view shared.store
        }
        |> Page.withLayout
            (\m ->
                Layouts.Default
                    { navBarContent = Just <| NavBar.navBar (NavBar.modelFromShared shared route.params.grupoId) shared.store route.path
                    , grupo = Store.getGrupo m.grupoId shared.store
                    }
            )



-- INIT


type Msg
    = NoOp
    | DeletePago ULID
    | ConfirmDeletePago ULID
    | CancelDeletePago
    | DeletePagoResponse (Result Http.Error ULID)


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
        [ Store.ensureGrupo grupoId store
        , Store.ensurePagos grupoId store
        , Effect.getCurrentUser grupoId
        , Effect.setUnsavedChangesWarning False
        ]
    )



-- UPDATE


update : Store -> Maybe ULID -> Msg -> Model -> ( Model, Effect Msg )
update store userId msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        DeletePago pagoId ->
            ( { model | deletingPagoId = Just pagoId }
            , Effect.none
            )

        ConfirmDeletePago pagoId ->
            case store |> Store.getGrupo model.grupoId of
                NotAsked ->
                    ( model, Effect.none )

                Loading ->
                    ( model, Effect.none )

                Failure _ ->
                    ( model, Effect.none )

                Success grupo ->
                    ( { model | deletingPagoId = Nothing }
                    , Effect.batch
                        [ Effect.sendCmd <| Api.deleteGrupoByIdPagosByPagoId grupo.id pagoId DeletePagoResponse
                        ]
                    )

        CancelDeletePago ->
            ( { model | deletingPagoId = Nothing }
            , Effect.none
            )

        DeletePagoResponse result ->
            case result of
                Ok pagoBorradoId ->
                    ( model
                    , Effect.batch
                        [ Store.refreshGrupo model.grupoId
                        , Store.refreshResumen model.grupoId
                        , Store.refreshPagos model.grupoId
                        , pushToast ToastSuccess "Pago borrado"
                        ]
                    )

                Err e ->
                    ( model, pushToast ToastDanger "Falle al borrar el pago" )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Store -> Model -> View Msg
view store model =
    case ( store |> Store.getGrupo model.grupoId, store |> Store.getPagos model.grupoId ) of
        ( Success grupo, Success pagos ) ->
            let
                pagoBeingDeleted =
                    case model.deletingPagoId of
                        Just pagoId ->
                            pagos
                                |> List.filter (\p -> p.pagoId == pagoId)
                                |> List.head

                        Nothing ->
                            Nothing
            in
            { title = grupo.nombre
            , body =
                [ div [ class "container columns is-mobile is-justify-content-end px-4 pt-2 pb-1 m-0" ]
                    [ a [ class "button mx-3", Path.href <| Path.Grupos_GrupoId__Pagos_New { grupoId = model.grupoId } ] [ text "Agregar pago" ]
                    ]
                , div
                    [ class "container columns is-flex-wrap-wrap px-4 pb-4 pt-1" ]
                    (pagos
                        |> List.map
                            (\pago ->
                                div [ class "column is-one-third" ]
                                    [ div [ class "card" ]
                                        [ header [ class "card-header" ]
                                            [ p [ class "card-header-title py-2 px-4" ]
                                                [ text pago.nombre ]
                                            , if pago.isValid then
                                                text ""

                                              else
                                                button
                                                    [ class "card-header-icon"
                                                    , attribute "aria-label" "more options"
                                                    ]
                                                    [ span
                                                        [ class "icon has-tooltip-multiline has-tooltip-danger has-text-danger"
                                                        , attribute "data-tooltip" "Este pago es invalido asi que no se cuenta para las deudas."
                                                        ]
                                                        [ Icons.toHtml [] Icons.alertCircle
                                                        ]
                                                    ]
                                            ]
                                        , div [ class "card-content" ]
                                            [ p [ class "title is-3 m-0" ]
                                                [ text "$ "
                                                , text <| Monto.toString pago.monto
                                                ]
                                            , p [ style "display" "none" ]
                                                [ let
                                                    pagador2Text pagador =
                                                        pagador
                                                            |> lookupNombreParticipante grupo
                                                  in
                                                  case Pago.getPagadores pago of
                                                    [] ->
                                                        text <| "pagado por nadie!"

                                                    [ pagador ] ->
                                                        text <| "pagado por " ++ pagador2Text pagador

                                                    [ pagador1, pagador2 ] ->
                                                        text <| ("pagado  por " ++ pagador2Text pagador1 ++ " y " ++ pagador2Text pagador2)

                                                    [ pagador1, pagador2, pagador3 ] ->
                                                        text <| ("pagado por " ++ pagador2Text pagador1 ++ ", " ++ pagador2Text pagador2 ++ " y " ++ pagador2Text pagador3)

                                                    pagador1 :: pagador2 :: rest ->
                                                        text <| ("pagado por " ++ pagador2Text pagador1 ++ ", " ++ pagador2Text pagador2 ++ " y " ++ String.fromInt (List.length rest) ++ " personas mas")
                                                ]
                                            ]
                                        , footer [ class "card-footer" ]
                                            [ a
                                                [ class "button card-footer-item"
                                                , Path.href <| Path.Grupos_GrupoId__Pagos_PagoId_ { grupoId = model.grupoId, pagoId = pago.pagoId }
                                                ]
                                                [ Icons.toHtml [] Icons.edit
                                                ]
                                            , button [ class "card-footer-item", onClick <| DeletePago pago.pagoId ]
                                                [ Icons.toHtml [] Icons.trash2
                                                ]
                                            ]
                                        ]
                                    ]
                            )
                    )
                , deleteConfirmationModal model.deletingPagoId pagoBeingDeleted
                ]
            }

        ( _, _ ) ->
            { title = "Impossible"
            , body = []
            }


deleteConfirmationModal : Maybe ULID -> Maybe ShallowPago -> Html Msg
deleteConfirmationModal deletingPagoId maybePago =
    div
        (class "modal"
            :: (case deletingPagoId of
                    Nothing ->
                        []

                    Just _ ->
                        [ class "is-active" ]
               )
        )
        [ div
            [ class "modal-background"
            , onClick CancelDeletePago
            ]
            []
        , div
            [ class "modal-card"
            ]
            [ header
                [ class "modal-card-head"
                ]
                [ p
                    [ class "modal-card-title"
                    ]
                    [ text "Confirmar" ]
                , button
                    [ class "delete"
                    , attribute "aria-label" "close"
                    , onClick CancelDeletePago
                    ]
                    []
                ]
            , section
                [ class "modal-card-body"
                ]
                [ p []
                    [ text "¿Estás seguro que querés eliminar "
                    , case maybePago of
                        Just pago ->
                            strong [] [ text ("\"" ++ pago.nombre ++ "\"") ]

                        Nothing ->
                            text "este pago"
                    , text "?"
                    ]
                , p [ class "mt-3" ]
                    [ text "Esta acción no se puede deshacer." ]
                ]
            , footer
                [ class "modal-card-foot"
                ]
                [ div
                    [ class "buttons"
                    ]
                    [ button
                        [ class "button is-danger"
                        , onClick <|
                            case deletingPagoId of
                                Just pagoId ->
                                    ConfirmDeletePago pagoId

                                Nothing ->
                                    NoOp
                        ]
                        [ text "Eliminar" ]
                    , button
                        [ class "button"
                        , onClick CancelDeletePago
                        ]
                        [ text "Cancelar" ]
                    ]
                ]
            ]
        ]
