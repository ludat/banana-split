module Pages.Grupos.GrupoId_.Settings exposing (Model, Msg, page)

import Components.NavBar as NavBar
import Components.Ui5 as Ui5
import Effect exposing (Effect)
import Generated.Api as Api exposing (ResumenGrupo, ShallowGrupo, ULID)
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (style)
import Html.Events exposing (onClick)
import Http
import Layouts
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init shared.store route.params.grupoId
        , update = update
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


type alias Model =
    { grupoId : String
    }


type Msg
    = FreezeGrupo
    | FreezeGrupoResponse (Result Http.Error ShallowGrupo)
    | UnfreezeGrupo
    | UnfreezeGrupoResponse (Result Http.Error ShallowGrupo)


init : Store -> ULID -> ( Model, Effect Msg )
init store grupoId =
    ( { grupoId = grupoId }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Store.ensureResumen grupoId store
        , Effect.getCurrentUser grupoId
        , Effect.setUnsavedChangesWarning False
        ]
    )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
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
                [ div [] [ Ui5.text "Cargando..." ]
                ]
            }

        Failure _ ->
            { title = "Fallo"
            , body = []
            }

        Success grupo ->
            { title = grupo.nombre ++ " - Configuración"
            , body =
                [ div
                    [ style "max-width" "var(--sapBreakpoint_L_Min)"
                    , style "margin-left" "auto"
                    , style "margin-right" "auto"
                    ]
                    [ viewFreezeSection store model
                    ]
                ]
            }


viewFreezeSection : Store -> Model -> Html Msg
viewFreezeSection store model =
    case store |> Store.getResumen model.grupoId of
        Success resumen ->
            div []
                [ Ui5.title [ Attr.attribute "level" "H4", style "margin-bottom" "1rem" ] [ text "Congelar grupo" ]
                , div [ style "margin-bottom" "1rem" ]
                    [ Ui5.text <|
                        if resumen.isFrozen then
                            "Este grupo está congelado. Las deudas están fijas y no se pueden agregar, editar ni eliminar pagos."

                        else
                            "Congelar el grupo fija las deudas actuales. No se podrán agregar, editar ni eliminar pagos mientras esté congelado."
                    ]
                , viewFreezeButton resumen
                ]

        Loading ->
            div [] [ Ui5.text "Cargando..." ]

        NotAsked ->
            div [] [ Ui5.text "Cargando..." ]

        Failure _ ->
            div [] [ Ui5.text "Error cargando los datos del grupo" ]


viewFreezeButton : ResumenGrupo -> Html Msg
viewFreezeButton resumen =
    if resumen.isFrozen then
        Ui5.button
            [ Attr.attribute "design" "Default"
            , Attr.attribute "icon" "unlocked"
            , onClick UnfreezeGrupo
            ]
            [ text "Descongelar" ]

    else
        Ui5.button
            [ Attr.attribute "design" "Emphasized"
            , Attr.attribute "icon" "locked"
            , onClick FreezeGrupo
            ]
            [ text "Congelar" ]
