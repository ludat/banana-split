module Pages.Grupos.GrupoId_.Pagos exposing (Model, Msg, page)

import Components.NavBar as NavBar
import Components.Ui5 as Ui5
import Effect exposing (Effect)
import Generated.Api as Api exposing (ShallowPago, ULID)
import Html exposing (Html, div, p, strong, text)
import Html.Attributes as Attr exposing (class, style)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode
import Layouts
import Models.Monto as Monto
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
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
        , update = update shared.store
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
    | Navigate Path.Path
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


update : Store -> Msg -> Model -> ( Model, Effect Msg )
update store msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        Navigate path ->
            ( model, Effect.pushRoutePath path )

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
                Ok _ ->
                    ( model
                    , Effect.batch
                        [ Store.refreshGrupo model.grupoId
                        , Store.refreshResumen model.grupoId
                        , Store.refreshPagos model.grupoId
                        , pushToast ToastSuccess "Pago borrado"
                        ]
                    )

                Err _ ->
                    ( model, pushToast ToastDanger "Falle al borrar el pago" )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Store -> Model -> View Msg
view store model =
    case ( store |> Store.getGrupo model.grupoId, store |> Store.getPagos model.grupoId ) of
        ( Success grupo, Success pagos ) ->
            let
                pagoBeingDeleted =
                    model.deletingPagoId
                        |> Maybe.andThen
                            (\pagoId ->
                                pagos
                                    |> List.filter (\p -> p.pagoId == pagoId)
                                    |> List.head
                            )
            in
            { title = grupo.nombre
            , body =
                [ Ui5.list
                    [ Attr.attribute "header-text" "Pagos"
                    , Attr.attribute "selection-mode" "Delete"
                    , on "item-delete"
                        (Json.Decode.at [ "detail", "item", "dataset", "id" ] Json.Decode.string
                            |> Json.Decode.map DeletePago
                        )
                    ]
                    (pagos
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
                , Ui5.button
                    [ Attr.attribute "design" "Emphasized"
                    , Attr.attribute "icon" "add"
                    , onClick (Navigate <| Path.Grupos_GrupoId__Pagos_New { grupoId = model.grupoId })
                    ]
                    [ text "Agregar pago" ]
                , deleteConfirmationDialog model.deletingPagoId pagoBeingDeleted
                ]
            }

        _ ->
            { title = "Impossible"
            , body = []
            }


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
            [ text "¿Estás seguro que querés eliminar "
            , case maybePago of
                Just pago ->
                    strong [] [ text ("\"" ++ pago.nombre ++ "\"") ]

                Nothing ->
                    text "este pago"
            , text "?"
            ]
        , p [ style "margin-top" "0.75rem" ]
            [ text "Esta acción no se puede deshacer." ]
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
                            NoOp
                ]
                [ text "Eliminar" ]
            ]
        ]
