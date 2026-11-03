module Pages.Home_ exposing (CrearGrupoForm, Model, Modo(..), Msg, config, page)

import Components.Bootstrap as Bs
import Components.Landing as Landing
import Effect exposing (Effect, pushRoutePath)
import Form exposing (Form, Msg(..))
import Form.Field
import Form.Validate exposing (Validation, andMap, andThen, field, nonEmpty, string, succeed)
import Generated.Api as Api exposing (EstadoParaUsuario(..), GrupoParaUsuario, Moneda)
import Html exposing (Html, a, div, i, input, label, span, text)
import Html.Attributes as Attr exposing (class, classList, for, id, placeholder, required, type_)
import Html.Events exposing (onClick, onSubmit)
import Layouts
import Models.Moneda as Moneda
import Models.Monto as Monto
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Utils.Form exposing (CustomFormError, errorForField, hasErrorField)
import Utils.Telemetry as Telemetry
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    -- elm-land reconoce el tipo de página buscando `Page.new` en `page`, así
    -- que no puede quedar escondido en `config`.
    Page.new (config SegunSesion shared)
        |> Page.withLayout (\_ -> Layouts.Default {})


{-| `/` muestra la landing a quien no inició sesión y la home a quien sí;
`/landing` (ver `Pages.Landing`) la muestra siempre. Es la misma página para
que el "Comenzá" de la landing cree el grupo igual que el formulario de la home.

El layout es siempre el Default: elm-land no vuelve a inicializar el layout
cuando cambia el Shared, así que no se puede elegir según la sesión, que se
resuelve después de cargar la página.

-}
type Modo
    = SegunSesion
    | SiempreLanding


config :
    Modo
    -> Shared.Model
    ->
        { init : () -> ( Model, Effect Msg )
        , update : Msg -> Model -> ( Model, Effect Msg )
        , subscriptions : Model -> Sub Msg
        , view : Model -> View Msg
        }
config modo shared =
    { init = init shared
    , update = update shared
    , subscriptions = subscriptions
    , view = view modo shared
    }


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
    , misGrupos : WebData (List GrupoParaUsuario)
    , crearGrupoAbierto : Bool
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared () =
    ( { form = Form.initial [] (validate (isLoggedIn shared))
      , misGrupos = RemoteData.Loading
      , crearGrupoAbierto = False
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
    | GotMisGrupos (WebData (List GrupoParaUsuario))
    | AbrirCrearGrupo
    | CerrarCrearGrupo


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

        AbrirCrearGrupo ->
            ( { model | crearGrupoAbierto = True }
            , Effect.none
            )

        CerrarCrearGrupo ->
            ( { model | crearGrupoAbierto = False }
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


view : Modo -> Shared.Model -> Model -> View Msg
view modo shared model =
    { title = "Banana split"
    , body =
        case ( modo, shared.currentUser ) of
            ( SiempreLanding, _ ) ->
                viewLanding shared model

            ( SegunSesion, Success _ ) ->
                [ div [ class "container py-4" ]
                    [ div [ class "row justify-content-center g-4" ]
                        [ div [ class "col-12 col-md-8 col-lg-6" ] [ viewMisGrupos model.misGrupos ]
                        , div [ class "col-12 col-md-8 col-lg-6" ] [ viewCrearGrupo shared model ]
                        ]
                    ]
                ]

            -- Mientras se resuelve la sesión no mostramos nada, así a quien ya
            -- inició sesión no le aparece la landing por un instante.
            ( SegunSesion, Loading ) ->
                [ div [ class "d-flex justify-content-center py-5" ] [ Bs.spinner [] ] ]

            ( SegunSesion, _ ) ->
                viewLanding shared model
    }


viewLanding : Shared.Model -> Model -> List (Html Msg)
viewLanding shared model =
    [ Landing.view { onComenzar = AbrirCrearGrupo }
    , Bs.modal
        { id = "landing-modal-crear-grupo"
        , isOpen = model.crearGrupoAbierto
        , onClose = CerrarCrearGrupo
        , title = "Crear grupo"
        , centered = True
        , body =
            [ Html.form [ id "landing-crear-grupo", onSubmit (UpdateForm Submit) ]
                (viewCrearGrupoCampos shared model)
            ]
        , footer =
            [ Bs.btn Bs.Transparent [ Telemetry.click "landing.cancel_create_grupo", onClick CerrarCrearGrupo ] [ text "Cancelar" ]
            , Bs.btn Bs.Primary
                [ id "landing-crear-grupo-submit"
                , Telemetry.click "landing.create_grupo"
                , type_ "submit"

                -- The button lives in the modal footer, outside the form.
                , Attr.attribute "form" "landing-crear-grupo"
                ]
                [ text "Crear" ]
            ]
        }
    ]


{-| The grupos where the signed-in user claimed a participante, each linking to
its grupo page and showing where the user stands in it.
-}
viewMisGrupos : WebData (List GrupoParaUsuario) -> Html Msg
viewMisGrupos misGrupos =
    Bs.card [ id "home-mis-grupos" ]
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
                    (List.map viewMisGruposItem grupos)

            Failure _ ->
                Bs.cardBody []
                    [ Bs.alert Bs.AlertDanger [] [ text "Error cargando tus grupos." ] ]

            _ ->
                Bs.cardBody []
                    [ div [ class "text-muted" ] [ text "Cargando..." ] ]
        ]


viewMisGruposItem : GrupoParaUsuario -> Html Msg
viewMisGruposItem grupo =
    a
        [ class "list-group-item list-group-item-action d-flex justify-content-between align-items-center gap-3"
        , Path.href <| Path.Grupos_Id_ { id = grupo.id }
        , Telemetry.click "home.open_grupo"
        ]
        [ text grupo.nombre
        , div [ class "small text-end" ] (viewEstado grupo)
        ]


{-| Lo mismo que la página del grupo muestra arriba de todo, en chico: abierto,
cómo venís; congelado, cuánto te falta transferir.
-}
viewEstado : GrupoParaUsuario -> List (Html Msg)
viewEstado grupo =
    case grupo.estado of
        EnGrupoAbierto abierto ->
            let
                lineas =
                    if abierto.cantidadPagos == 0 then
                        [ div [ class "text-muted" ] [ text "Sin gastos todavía" ] ]

                    else if List.isEmpty abierto.neto then
                        [ viewAlDia ]

                    else
                        abierto.neto
                            |> List.map
                                (\( moneda, monto ) ->
                                    if monto.valor < 0 then
                                        viewLinea "Debés" "text-danger" grupo moneda monto

                                    else
                                        viewLinea "Te deben" "text-success" grupo moneda monto
                                )
            in
            lineas ++ viewInvalidos abierto.cantidadPagosInvalidos

        EnGrupoCongelado congelado ->
            let
                lineas =
                    List.map (\( moneda, monto ) -> viewLinea "Te tienen que transferir" "text-success" grupo moneda monto) congelado.aCobrar
                        ++ List.map (\( moneda, monto ) -> viewLinea "Te falta transferir" "text-danger" grupo moneda monto) congelado.aPagar
            in
            if List.isEmpty lineas then
                [ viewAlDia ]

            else
                lineas


viewLinea : String -> String -> GrupoParaUsuario -> Moneda -> Api.Monto -> Html Msg
viewLinea etiqueta color grupo moneda monto =
    div []
        [ span [ class "text-muted me-1" ] [ text etiqueta ]
        , span [ class ("fw-semibold text-nowrap " ++ color) ]
            [ text (Moneda.simbolo grupo.monedaPorDefecto moneda ++ " " ++ Monto.toString (Monto.abs monto)) ]
        ]


viewAlDia : Html Msg
viewAlDia =
    div [ class "text-success" ] [ i [ class "bi bi-check2-circle me-1" ] [], text "Estás al día" ]


{-| Los inválidos no entran en el neto, así que el número de arriba puede
estar mintiendo: vale la pena avisarlo desde acá.
-}
viewInvalidos : Int -> List (Html Msg)
viewInvalidos cantidad =
    if cantidad == 0 then
        []

    else
        [ div [ class "text-warning-emphasis" ]
            [ i [ class "bi bi-exclamation-triangle me-1" ] []
            , text <|
                if cantidad == 1 then
                    "1 gasto inválido"

                else
                    String.fromInt cantidad ++ " gastos inválidos"
            ]
        ]


viewCrearGrupo : Shared.Model -> Model -> Html Msg
viewCrearGrupo shared model =
    Html.form [ id "home-crear-grupo", class "card", onSubmit (UpdateForm Submit) ]
        [ Bs.cardHeader [] [ text "Crear grupo" ]
        , Bs.cardBody [] <|
            viewCrearGrupoCampos shared model
                ++ [ Bs.btn Bs.Primary
                        [ id "crear-grupo"
                        , Telemetry.click "home.create_grupo"
                        , type_ "submit"
                        ]
                        [ text "Crear" ]
                   ]
        ]


viewCrearGrupoCampos : Shared.Model -> Model -> List (Html Msg)
viewCrearGrupoCampos shared model =
    let
        nombreField =
            Form.getFieldAsString "nombre" model.form
    in
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
    ]
