module Pages.Grupos.GrupoId_.Repartijas exposing (Model, Msg, page)

import Components.NavBar as NavBar
import Effect exposing (Effect)
import FeatherIcons as Icons
import Form exposing (Form)
import Form.Error as FormError
import Form.Field as Form
import Form.Input as FormInput
import Form.Validate as V
import Generated.Api as Api exposing (Grupo, Participante, Repartija, RepartijaItem, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit)
import Http
import Layouts
import Models.Monto exposing (validateMonto)
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Route.Path as Path
import Shared
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
                    }
            )



-- INIT


type alias Model =
    { grupoId : ULID
    , repartijaForm : Form CustomFormError Repartija
    , isNewRepartijaPopoverOpen : Bool
    }


initialRepartijaForm =
    Form.initial
        [ ( "items", Form.list [ Form.value Form.EmptyField ] )
        ]
        validateRepartija


init : Store -> ULID -> ( Model, Effect Msg )
init store grupoId =
    ( { grupoId = grupoId
      , repartijaForm = initialRepartijaForm
      , isNewRepartijaPopoverOpen = False
      }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Store.ensureRepartijas grupoId store
        ]
    )


validateRepartija : V.Validation CustomFormError Repartija
validateRepartija =
    V.succeed Repartija
        |> V.andMap (V.succeed "00000000000000000000000000")
        |> V.andMap (V.succeed "00000000000000000000000000")
        |> V.andMap (V.field "nombre" (V.string |> V.andThen V.nonEmpty))
        |> V.andMap (V.field "monto" validateMonto)
        |> V.andMap (V.field "items" (V.list validateRepartijaItem))
        |> V.andMap (V.field "claims" (V.succeed []))


validateRepartijaItem : V.Validation CustomFormError RepartijaItem
validateRepartijaItem =
    V.succeed RepartijaItem
        |> V.andMap (V.succeed "00000000000000000000000000")
        |> V.andMap (V.field "nombre" V.string)
        |> V.andMap (V.field "monto" validateMonto)
        |> V.andMap (V.field "cantidad" V.int)



-- UPDATE


type Msg
    = NoOp String
    | RepartijaForm Form.Msg
    | CreateRepartijaSuccess Repartija
    | CreateRepartijaFailed Http.Error
    | InitNewRepartija
    | CloseNewRepartijaPopover


update : Store -> Msg -> Model -> ( Model, Effect Msg )
update store msg model =
    case msg of
        NoOp s ->
            ( model
            , Effect.none
            )

        RepartijaForm Form.Submit ->
            case Form.getOutput model.repartijaForm of
                Just repartija ->
                    ( { model | repartijaForm = Form.update validateRepartija Form.Submit model.repartijaForm }
                    , Effect.sendCmd <|
                        Api.postGrupoByIdRepartijas model.grupoId
                            repartija
                            (\r ->
                                case r of
                                    Ok newRepartija ->
                                        CreateRepartijaSuccess newRepartija

                                    Err e ->
                                        CreateRepartijaFailed e
                            )
                    )

                Nothing ->
                    ( model
                    , Effect.none
                    )

        RepartijaForm formMsg ->
            ( { model
                | repartijaForm = Form.update validateRepartija formMsg model.repartijaForm
              }
            , Effect.none
            )

        CreateRepartijaSuccess repartija ->
            ( { model | isNewRepartijaPopoverOpen = False }
            , Effect.batch
                [ Toasts.pushToast Toasts.ToastSuccess "Repartija creada con éxito"
                , Store.refreshRepartijas model.grupoId
                ]
            )

        CreateRepartijaFailed e ->
            ( model
            , Toasts.pushToast Toasts.ToastDanger "Falló la creación de la repartija"
            )

        InitNewRepartija ->
            ( { model
                | isNewRepartijaPopoverOpen = True
                , repartijaForm = initialRepartijaForm
              }
            , Effect.none
            )

        CloseNewRepartijaPopover ->
            ( { model
                | isNewRepartijaPopoverOpen = False
              }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Store -> Model -> View Msg
view store model =
    case ( Store.getGrupo model.grupoId store, Store.getRepartijas model.grupoId store ) of
        ( Success grupo, Success repartijas ) ->
            { title = "Repartijas"
            , body =
                [ newRepartijaModal model
                , button [ class "button is-primary", onClick InitNewRepartija ] [ text "Crear repartija" ]
                , div []
                    (repartijas
                        |> List.map
                            (\repartija ->
                                p []
                                    [ a
                                        [ Path.href <|
                                            Path.Grupos_GrupoId__Repartijas_RepartijaId_
                                                { grupoId = model.grupoId
                                                , repartijaId = repartija.repartijaShallowId
                                                }
                                        ]
                                        [ text <| repartija.repartijaShallowNombre
                                        ]
                                    ]
                            )
                    )
                ]
            }

        _ ->
            { title = "Repartijas"
            , body = [ text "Cargando..." ]
            }


newRepartijaModal : Model -> Html Msg
newRepartijaModal model =
    div
        ([ class "modal"
         ]
            ++ (if model.isNewRepartijaPopoverOpen then
                    [ class "is-active" ]

                else
                    []
               )
        )
        [ div
            [ class "modal-background"
            , onClick <| CloseNewRepartijaPopover
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
                    [ text "Agregar repartija" ]
                , button
                    [ class "delete"
                    , attribute "aria-label" "close"
                    , onClick <| CloseNewRepartijaPopover
                    ]
                    []
                ]
            , section
                [ class "modal-card-body"
                ]
                [ repartijaForm model.repartijaForm
                ]
            , footer
                [ class "modal-card-foot"
                ]
                [ div
                    [ class "buttons"
                    ]
                    [ button
                        [ class "button is-primary"
                        , onClick <| RepartijaForm Form.Submit
                        ]
                        [ text "Crear repartija" ]
                    , button
                        [ class "button"
                        , onClick CloseNewRepartijaPopover
                        ]
                        [ text "Cancelar" ]
                    ]
                ]
            ]
        ]


repartijaForm : Form CustomFormError Repartija -> Html Msg
repartijaForm form =
    let
        errorFor field =
            case field.liveError of
                Just FormError.Empty ->
                    p [ class "help is-danger" ] [ text "No puede ser vacio" ]

                Just FormError.InvalidString ->
                    p [ class "help is-danger" ] [ text "String invalido" ]

                Just FormError.InvalidEmail ->
                    p [ class "help is-danger" ] [ text "Email invalido" ]

                Just FormError.InvalidFormat ->
                    p [ class "help is-danger" ] [ text "Formato invalido" ]

                Just FormError.InvalidInt ->
                    p [ class "help is-danger" ] [ text "Entero invalido" ]

                Just FormError.InvalidFloat ->
                    p [ class "help is-danger" ] [ text "Numero con coma invalido" ]

                Just FormError.InvalidBool ->
                    p [ class "help is-danger" ] [ text "Booleano invalido" ]

                Just (FormError.SmallerIntThan _) ->
                    p [ class "help is-danger" ] [ text "Mas chico que" ]

                Just (FormError.GreaterIntThan _) ->
                    p [ class "help is-danger" ] [ text "Mas grande que" ]

                Just (FormError.SmallerFloatThan _) ->
                    p [ class "help is-danger" ] [ text "Mas chico que" ]

                Just (FormError.GreaterFloatThan _) ->
                    p [ class "help is-danger" ] [ text "Mas grande que" ]

                Just (FormError.ShorterStringThan _) ->
                    p [ class "help is-danger" ] [ text "Mas corto que" ]

                Just (FormError.LongerStringThan _) ->
                    p [ class "help is-danger" ] [ text "Longer than" ]

                Just FormError.NotIncludedIn ->
                    p [ class "help is-danger" ] [ text "Not included in" ]

                Just (FormError.CustomError _) ->
                    p [ class "help is-danger" ] [ text "Jajan't" ]

                Nothing ->
                    text ""

        hasError field =
            case field.liveError of
                Just _ ->
                    True

                Nothing ->
                    False

        nombreField =
            Form.getFieldAsString "nombre" form

        montoField =
            Form.getFieldAsString "monto" form

        itemsIndexes =
            Form.getListIndexes "items" form
    in
    Html.form [ onSubmit <| RepartijaForm Form.Submit ]
        [ div [ class "field mb-5" ]
            [ label [ class "label" ]
                [ text "Nombre" ]
            , div [ class "control" ]
                [ Html.map RepartijaForm <|
                    FormInput.textInput nombreField
                        [ class "input"
                        , type_ "text"
                        , placeholder "El bar"
                        , classList [ ( "is-danger", hasError nombreField ) ]
                        ]
                , errorFor nombreField
                ]
            ]
        , div [ class "container" ]
            [ table [ class "table is-fullwidth" ]
                [ thead []
                    [ tr []
                        [ th [ class "pl-0" ] [ text "Item" ]
                        , th [] [ text "Monto total" ]
                        , th [] [ text "Cantidad" ]
                        , th [] []
                        ]
                    ]
                , tbody [] <|
                    List.map
                        (\i -> repartijaItemForm (List.length itemsIndexes > 1) i form)
                        itemsIndexes
                ]
            ]
        , div [ class "container mb-5" ] <|
            [ button
                [ class "button is-outlined is-primary is-align-items-center is-flex pr-5"
                , style "gap" ".5rem"
                , onClick <| RepartijaForm <| Form.Append "items"
                , type_ "button"
                ]
                [ Icons.toHtml [] Icons.plus
                , span []
                    [ text "Agregar item" ]
                ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ]
                [ text "Propina" ]
            , div [ class "control" ]
                [ Html.map RepartijaForm <|
                    FormInput.textInput montoField
                        [ class "input"
                        , type_ "text"
                        , placeholder "1000"
                        , classList [ ( "is-danger", hasError montoField ) ]
                        ]
                , errorFor montoField
                ]
            ]
        ]


repartijaItemForm : Bool -> Int -> Form CustomFormError Repartija -> Html Msg
repartijaItemForm shouldMostrarBorrar i form =
    let
        errorFor field =
            case field.liveError of
                Just FormError.Empty ->
                    p [ class "help is-danger" ] [ text "No puede ser vacio" ]

                Just _ ->
                    p [ class "help is-danger" ] [ text "Algo esta maloso" ]

                Nothing ->
                    text ""

        hasError field =
            case field.liveError of
                Just _ ->
                    True

                Nothing ->
                    False

        nombreField =
            Form.getFieldAsString ("items." ++ String.fromInt i ++ ".nombre") form

        montoField =
            Form.getFieldAsString ("items." ++ String.fromInt i ++ ".monto") form

        cantidadField =
            Form.getFieldAsString ("items." ++ String.fromInt i ++ ".cantidad") form
    in
    tr []
        [ td [ class "control pl-0" ]
            [ Html.map RepartijaForm <|
                FormInput.textInput nombreField
                    [ class "input"
                    , type_ "text"
                    , placeholder "Birrita"
                    , classList [ ( "is-danger", hasError nombreField ) ]
                    ]
            ]
        , td [ class "control" ]
            [ Html.map RepartijaForm <|
                FormInput.textInput montoField
                    [ class "input"
                    , type_ "text"
                    , placeholder "20000"
                    , classList [ ( "is-danger", hasError montoField ) ]
                    ]
            ]
        , td [ class "control" ]
            [ Html.map RepartijaForm <|
                FormInput.textInput cantidadField
                    [ class "input"
                    , type_ "text"
                    , placeholder "4"
                    , classList [ ( "is-danger", hasError cantidadField ) ]
                    ]
            ]
        , td [ class "control" ]
            [ if shouldMostrarBorrar then
                button [ class "button is-danger is-outlined", type_ "button", onClick <| RepartijaForm <| Form.RemoveItem "items" i ]
                    [ Icons.toHtml [] Icons.trash2
                    ]

              else
                text ""
            ]
        ]
