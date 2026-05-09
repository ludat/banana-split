module Pages.Grupos.GrupoId_.Settings exposing (Model, Msg, page)

import Components.NavBar as NavBar
import Components.Ui5 as Ui5
import Effect exposing (Effect)
import Form exposing (Form)
import Form.Init as Form
import Form.Validate as V exposing (Validation)
import Generated.Api as Api exposing (ResumenGrupo, ShallowGrupo, ULID, UpdateGrupoParams)
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (disabled, style)
import Html.Events exposing (onClick)
import Http
import Layouts
import Models.Moneda as Moneda
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import Process
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared
import Task
import Utils.Form exposing (CustomFormError)
import Utils.Toasts as Toasts
import Utils.Toasts.Types as Toasts
import View exposing (View)


page : Shared.Model -> Route { grupoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init shared.store route.params.grupoId
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


type alias Model =
    { grupoId : String
    , ajustesForm : Form CustomFormError UpdateGrupoParams
    , ajustesFormDirty : Bool
    }


type Msg
    = FreezeGrupo
    | FreezeGrupoResponse (Result Http.Error ShallowGrupo)
    | UnfreezeGrupo
    | UnfreezeGrupoResponse (Result Http.Error ShallowGrupo)
    | AjustesForm Form.Msg
    | UpdateGrupoResponse (Result Http.Error ShallowGrupo)
    | CheckIfGrupoIsPresent


init : Store -> ULID -> ( Model, Effect Msg )
init store grupoId =
    ( { grupoId = grupoId
      , ajustesForm = Form.initial [] validateUpdateGrupoParams
      , ajustesFormDirty = False
      }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Store.ensureResumen grupoId store
        , Effect.getCurrentUser grupoId
        , Effect.setUnsavedChangesWarning False
        , waitAndCheckGrupo
        ]
    )


validateUpdateGrupoParams : Validation CustomFormError UpdateGrupoParams
validateUpdateGrupoParams =
    V.succeed UpdateGrupoParams
        |> V.andMap (V.field "nombre" (V.string |> V.andThen V.nonEmpty))
        |> V.andMap (V.field "moneda" Moneda.validate)


waitAndCheckGrupo : Effect Msg
waitAndCheckGrupo =
    Effect.sendCmd <| Task.perform (\_ -> CheckIfGrupoIsPresent) (Process.sleep 100)


seedAjustesForm : ShallowGrupo -> Form CustomFormError UpdateGrupoParams
seedAjustesForm grupo =
    Form.initial
        [ Form.setString "nombre" grupo.nombre
        , Form.setString "moneda" (Moneda.toString grupo.monedaPorDefecto)
        ]
        validateUpdateGrupoParams


update : Store -> Msg -> Model -> ( Model, Effect Msg )
update store msg model =
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

        AjustesForm Form.Submit ->
            case Form.getOutput model.ajustesForm of
                Just params ->
                    ( { model
                        | ajustesForm = Form.update validateUpdateGrupoParams Form.Submit model.ajustesForm
                      }
                    , Effect.sendCmd <|
                        Api.putGrupoById model.grupoId params UpdateGrupoResponse
                    )

                Nothing ->
                    ( { model
                        | ajustesForm = Form.update validateUpdateGrupoParams Form.Submit model.ajustesForm
                      }
                    , Effect.none
                    )

        AjustesForm formMsg ->
            ( { model
                | ajustesForm = Form.update validateUpdateGrupoParams formMsg model.ajustesForm
                , ajustesFormDirty = True
              }
            , Effect.none
            )

        UpdateGrupoResponse (Ok _) ->
            ( { model | ajustesFormDirty = False }
            , Effect.batch
                [ Store.refreshGrupo model.grupoId
                , Toasts.pushToast Toasts.ToastSuccess "Grupo actualizado"
                ]
            )

        UpdateGrupoResponse (Err _) ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "No se pudo actualizar el grupo"
            )

        CheckIfGrupoIsPresent ->
            case Store.getGrupo model.grupoId store of
                Success grupo ->
                    ( { model
                        | ajustesForm = seedAjustesForm grupo
                        , ajustesFormDirty = False
                      }
                    , Effect.none
                    )

                NotAsked ->
                    ( model, waitAndCheckGrupo )

                Loading ->
                    ( model, waitAndCheckGrupo )

                Failure _ ->
                    ( model, Effect.none )


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
                    [ viewAjustesSection model.ajustesFormDirty model.ajustesForm
                    , viewFreezeSection store model
                    ]
                ]
            }


viewAjustesSection : Bool -> Form CustomFormError UpdateGrupoParams -> Html Msg
viewAjustesSection dirty form =
    let
        nombreField =
            Form.getFieldAsString "nombre" form

        monedaField =
            Form.getFieldAsString "moneda" form
    in
    div [ style "margin-bottom" "2rem" ]
        [ Ui5.title [ Attr.attribute "level" "H4", style "margin-bottom" "1rem" ] [ text "Ajustes generales" ]
        , Ui5.form (always <| AjustesForm Form.Submit)
            [ Attr.attribute "label-span" "S12 M12 L12 XL12" ]
            [ Html.map AjustesForm <|
                Ui5.textFormItem nombreField
                    { label = "Nombre"
                    , placeholder = Just "Mi grupo"
                    , required = True
                    }
            , Html.map AjustesForm <|
                Ui5.formSelectItem monedaField
                    { label = "Moneda por defecto"
                    , required = True
                    , options = Moneda.todas |> List.map (\m -> ( Moneda.toString m, Moneda.nombre m ))
                    }
            , Ui5.button
                [ Attr.attribute "design" "Emphasized"
                , disabled (not dirty || Form.getOutput form == Nothing)
                , onClick (AjustesForm Form.Submit)
                ]
                [ text "Guardar" ]
            ]
        ]


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
