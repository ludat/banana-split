module Pages.Home_ exposing (Model, Msg, page)

import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.Bootstrap as Bs
import Effect exposing (Effect, pushRoutePath)
import Form exposing (Form, Msg(..))
import Form.Field
import Form.Validate exposing (Validation, andMap, andThen, field, nonEmpty, string, succeed)
import Generated.Api as Api exposing (Netos, ShallowGrupo)
import Html exposing (Html, a, div, input, label, text)
import Html.Attributes as Attr exposing (class, classList, for, id, placeholder, required, type_)
import Html.Events exposing (onClick)
import Layouts
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Route.Path as Path
import Shared
import Utils.Form exposing (CustomFormError, errorForField, hasErrorField)
import View exposing (View)


{-| The id of the singleton global group. On the backend it's the `GrupoGlobal`
sentinel, serialized as the literal string "global", so it works with every
regular grupo endpoint and page.
-}
globalGrupoId : String
globalGrupoId =
    "global"


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init shared
        , update = update shared
        , subscriptions = subscriptions
        , view = view shared
        }
        |> Page.withLayout (\_ -> Layouts.Default {})


{-| What the crear-grupo form produces. For an anonymous creator the first
participante's name is asked in the form; a signed-in creator gets one derived
from their account instead ('participante' is Nothing), via the dedicated
endpoint.
-}
type alias CrearGrupoForm =
    { nombre : String
    , participante : Maybe String
    }


type alias Model =
    { form : Form CustomFormError CrearGrupoForm
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { form = Form.initial [] (validate (isLoggedIn shared))
      }
    , Effect.batch
        [ Effect.setUnsavedChangesWarning False

        -- Loaded unconditionally so the panel has data whenever the async
        -- /api/me resolves the session to logged-in (init can't react to that).
        , Store.ensureGrupo globalGrupoId shared.store
        , Store.ensureResumen globalGrupoId shared.store
        ]
    )


isLoggedIn : Shared.Model -> Bool
isLoggedIn shared =
    RemoteData.isSuccess shared.currentUser


type Msg
    = NoOp
    | UpdateForm Form.Msg
    | GrupoCreated Api.Grupo


validate : Bool -> Validation CustomFormError CrearGrupoForm
validate loggedIn =
    succeed CrearGrupoForm
        |> andMap (field "nombre" (string |> andThen nonEmpty))
        |> andMap
            (if loggedIn then
                succeed Nothing

             else
                field "participante" (string |> andThen nonEmpty |> Form.Validate.map Just)
            )


update : Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update shared msg model =
    let
        validation =
            validate (isLoggedIn shared)
    in
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        UpdateForm Form.Submit ->
            case Form.getOutput model.form of
                Just crearGrupo ->
                    let
                        toMsg r =
                            case r of
                                Ok grupo ->
                                    GrupoCreated grupo

                                Err _ ->
                                    NoOp
                    in
                    ( { model | form = Form.update validation Form.Submit model.form }
                    , Effect.sendCmd <|
                        case crearGrupo.participante of
                            Just participante ->
                                Api.postGrupo
                                    { grupoName = crearGrupo.nombre, grupoParticipante = participante }
                                    toMsg

                            Nothing ->
                                Api.postMeGrupos { grupoName = crearGrupo.nombre } toMsg
                    )

                Nothing ->
                    ( { model | form = Form.update validation Form.Submit model.form }
                    , Effect.none
                    )

        UpdateForm formMsg ->
            ( { model | form = Form.update validation formMsg model.form }
            , Effect.none
            )

        GrupoCreated grupo ->
            ( { model | form = Form.initial [] validation }
            , Effect.batch
                [ -- Automatically select the first participante as current user
                  case grupo.participantes of
                    participante :: _ ->
                        Effect.saveCurrentUser grupo.id participante.id

                    [] ->
                        Effect.none
                , pushRoutePath <| Path.Grupos_Id_ { id = grupo.id }
                ]
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Banana split"
    , body =
        [ div [ class "container py-4" ]
            [ div [ class "row justify-content-center g-4" ] <|
                case shared.currentUser of
                    Success _ ->
                        [ div [ class "col-12 col-md-8 col-lg-6" ] [ viewGlobalPanel shared.store ]
                        , div [ class "col-12 col-md-8 col-lg-6" ] [ viewCrearGrupo shared model ]
                        ]

                    _ ->
                        [ div [ class "col-12 col-md-6" ] [ viewCrearGrupo shared model ] ]
            ]
        ]
    }


{-| The global group summary shown to logged-in users: balances plus entry
points into the existing (reused) grupo pages for the global group.
-}
viewGlobalPanel : Store -> Html Msg
viewGlobalPanel store =
    Bs.card []
        [ Bs.cardHeader [] [ text "Grupo global" ]
        , Bs.cardBody []
            [ case ( Store.getGrupo globalGrupoId store, Store.getResumen globalGrupoId store ) of
                ( Success grupo, Success resumen ) ->
                    viewGlobalContent grupo resumen

                ( Failure _, _ ) ->
                    Bs.alert Bs.AlertDanger [] [ text "Error cargando el grupo global." ]

                ( _, Failure _ ) ->
                    Bs.alert Bs.AlertDanger [] [ text "Error cargando el grupo global." ]

                _ ->
                    div [ class "text-muted" ] [ text "Cargando..." ]
            ]
        ]


viewGlobalContent : ShallowGrupo -> Api.ResumenGrupo -> Html Msg
viewGlobalContent grupo resumen =
    let
        netosDefault : Maybe (Netos Api.Monto)
        netosDefault =
            resumen.netos
                |> List.filter (\( m, _ ) -> m == grupo.monedaPorDefecto)
                |> List.head
                |> Maybe.map Tuple.second
    in
    div []
        [ if resumen.cantidadPagos == 0 then
            Bs.alert Bs.AlertInfo
                []
                [ text "Todavía no hay pagos en el grupo global. "
                , a [ Path.href <| Path.Grupos_GrupoId__Pagos_New { grupoId = globalGrupoId } ]
                    [ text "¡Registrá el primero!" ]
                ]

          else
            case netosDefault of
                Just netos ->
                    div [ class "mb-3" ] [ viewNetosBarras grupo netos ]

                Nothing ->
                    text ""
        , div [ class "d-flex flex-wrap gap-2" ]
            [ linkButton "btn-primary" (Path.Grupos_GrupoId__Pagos_New { grupoId = globalGrupoId }) "Registrar pago"
            , linkButton "btn-outline-secondary" (Path.Grupos_GrupoId__Liquidaciones { grupoId = globalGrupoId }) "Saldar deudas"
            , linkButton "btn-outline-secondary" (Path.Grupos_GrupoId__Pagos { grupoId = globalGrupoId }) "Ver pagos"
            , linkButton "btn-outline-secondary" (Path.Grupos_GrupoId__Participantes { grupoId = globalGrupoId }) "Participantes"
            ]
        ]


linkButton : String -> Path.Path -> String -> Html Msg
linkButton variant path label =
    a [ class ("btn " ++ variant), Path.href path ] [ text label ]


viewCrearGrupo : Shared.Model -> Model -> Html Msg
viewCrearGrupo shared model =
    let
        nombreField =
            Form.getFieldAsString "nombre" model.form

        participanteField =
            Form.getFieldAsString "participante" model.form
    in
    Bs.card []
        [ Bs.cardHeader [] [ text "Crear grupo" ]
        , Bs.cardBody []
            [ div [ class "mb-3" ]
                [ label [ for "nombre", class "form-label" ] [ text "Nombre" ]
                , Html.map UpdateForm <|
                    input
                        [ id "nombre"
                        , type_ "text"
                        , classList [ ( "form-control", True ), ( "is-invalid", hasErrorField nombreField ) ]
                        , placeholder "After del viernes, Vacaciones a Calamuchita"
                        , required True
                        , Attr.value (Maybe.withDefault "" nombreField.value)
                        , Html.Events.onInput (Form.Input nombreField.path Form.Text << Form.Field.String)
                        ]
                        []
                , div [ class "invalid-feedback" ] [ Html.map UpdateForm <| errorForField nombreField ]
                ]
            , case shared.currentUser of
                Success user ->
                    div [ class "mb-3 form-text" ]
                        [ text ("Vas a participar del grupo como " ++ user.nombre ++ ".") ]

                _ ->
                    div [ class "mb-3" ]
                        [ label [ for "participante", class "form-label" ] [ text "Participante" ]
                        , Html.map UpdateForm <|
                            input
                                [ id "participante"
                                , type_ "text"
                                , classList [ ( "form-control", True ), ( "is-invalid", hasErrorField participanteField ) ]
                                , placeholder "Juan"
                                , required True
                                , Attr.value (Maybe.withDefault "" participanteField.value)
                                , Html.Events.onInput (Form.Input participanteField.path Form.Text << Form.Field.String)
                                ]
                                []
                        , div [ class "invalid-feedback" ] [ Html.map UpdateForm <| errorForField participanteField ]
                        ]
            , Bs.btn Bs.Primary
                [ onClick <| UpdateForm Submit ]
                [ text "Crear" ]
            ]
        ]
