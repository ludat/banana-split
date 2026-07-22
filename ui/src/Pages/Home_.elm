module Pages.Home_ exposing (CrearGrupoForm, Model, Msg, page)

import Components.Bootstrap as Bs
import Effect exposing (Effect, pushRoutePath)
import Form exposing (Form, Msg(..))
import Form.Field
import Form.Validate exposing (Validation, andMap, andThen, field, nonEmpty, string, succeed)
import Generated.Api as Api exposing (ShallowGrupo)
import Html exposing (Html, a, div, input, label, text)
import Html.Attributes as Attr exposing (class, classList, for, id, placeholder, required, type_)
import Html.Events exposing (onClick)
import Layouts
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Utils.Form exposing (CustomFormError, errorForField, hasErrorField)
import View exposing (View)


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
    , misGrupos : WebData (List ShallowGrupo)
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { form = Form.initial [] (validate (isLoggedIn shared))
      , misGrupos = RemoteData.Loading
      }
    , Effect.batch
        [ Effect.setUnsavedChangesWarning False

        -- Loaded unconditionally so the "Mis grupos" card has data whenever the
        -- async /api/me resolves the session to logged-in (init can't react to
        -- that).
        , Effect.sendCmd <| Api.getMeGrupos (RemoteData.fromResult >> GotMisGrupos)
        ]
    )


isLoggedIn : Shared.Model -> Bool
isLoggedIn shared =
    RemoteData.isSuccess shared.currentUser


type Msg
    = NoOp
    | UpdateForm Form.Msg
    | GrupoCreated Api.Grupo
    | GotMisGrupos (WebData (List ShallowGrupo))


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

        GotMisGrupos misGrupos ->
            ( { model | misGrupos = misGrupos }
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
                    Success user ->
                        [ div [ class "col-12 col-md-8 col-lg-6" ] [ viewMisGrupos user model.misGrupos ]
                        , div [ class "col-12 col-md-8 col-lg-6" ] [ viewCrearGrupo shared model ]
                        ]

                    _ ->
                        [ div [ class "col-12 col-md-6" ] [ viewCrearGrupo shared model ] ]
            ]
        ]
    }


{-| The grupos where the signed-in user claimed a participante, each linking to
its grupo page and showing which participante they are there.
-}
viewMisGrupos : Api.User -> WebData (List ShallowGrupo) -> Html Msg
viewMisGrupos user misGrupos =
    Bs.card []
        [ Bs.cardHeader [] [ text "Mis grupos" ]
        , case misGrupos of
            Success [] ->
                Bs.cardBody []
                    [ Bs.alert Bs.AlertInfo
                        []
                        [ text "Todavía no estás en ningún grupo. Creá uno o reclamá tu participante en un grupo existente." ]
                    ]

            Success grupos ->
                div [ class "list-group list-group-flush" ]
                    (List.map (viewMisGruposItem user) grupos)

            Failure _ ->
                Bs.cardBody []
                    [ Bs.alert Bs.AlertDanger [] [ text "Error cargando tus grupos." ] ]

            _ ->
                Bs.cardBody []
                    [ div [ class "text-muted" ] [ text "Cargando..." ] ]
        ]


viewMisGruposItem : Api.User -> ShallowGrupo -> Html Msg
viewMisGruposItem user grupo =
    let
        claimedNombre =
            grupo.participantes
                |> List.filter (\p -> p.user |> Maybe.map (\owner -> owner.id == user.id) |> Maybe.withDefault False)
                |> List.head
                |> Maybe.map .nombre
    in
    a
        [ class "list-group-item list-group-item-action d-flex justify-content-between align-items-center"
        , Path.href <| Path.Grupos_Id_ { id = grupo.id }
        ]
        [ text grupo.nombre
        , case claimedNombre of
            Just nombre ->
                Html.small [ class "text-muted" ] [ text ("como " ++ nombre) ]

            Nothing ->
                text ""
        ]


viewCrearGrupo : Shared.Model -> Model -> Html Msg
viewCrearGrupo shared model =
    let
        nombreField =
            Form.getFieldAsString "nombre" model.form
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
                    let
                        participanteField =
                            Form.getFieldAsString "participante" model.form
                    in
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
