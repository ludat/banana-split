module Pages.Grupos.GrupoId_.Repartijas.RepartijaId_ exposing (Model, Msg, page)

import Components.NavBar as NavBar
import Effect exposing (Effect)
import Form exposing (Form)
import Form.Error as FormError
import Form.Input as FormInput
import Form.Validate as V
import Generated.Api as Api exposing (Grupo, Participante, Repartija, RepartijaClaim, RepartijaItem, ULID)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, placeholder, style, type_)
import Html.Events exposing (onClick, onSubmit)
import Layouts
import Models.Grupo exposing (lookupNombreParticipante)
import Models.Monto exposing (monto2Decimal, validateMonto)
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Numeric.Decimal as Decimal
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import Utils.Form exposing (CustomFormError)
import View exposing (View)


page : Shared.Model -> Route { grupoId : String, repartijaId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.grupoId route.params.repartijaId shared.store
        , update = update shared.store
        , subscriptions = subscriptions
        , view = view shared.store
        }
        |> Page.withLayout
            (\m ->
                Layouts.Default
                    { navBarContent = Just <| NavBar.navBar route.params.grupoId shared.store route.path
                    }
            )



-- INIT


type alias Model =
    { grupoId : ULID
    , repartijaId : ULID
    , isClaimModalOpen : Maybe RepartijaItem
    , claimForm : Form CustomFormError RepartijaClaim
    }


init : ULID -> ULID -> Store -> ( Model, Effect Msg )
init grupoId repartijaId store =
    ( { grupoId = grupoId
      , repartijaId = repartijaId
      , isClaimModalOpen = Nothing
      , claimForm = Form.initial [] validateClaim
      }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Store.ensureRepartija repartijaId store
        ]
    )


validateClaim : V.Validation CustomFormError RepartijaClaim
validateClaim =
    V.succeed RepartijaClaim
        |> V.andMap (V.succeed "00000000000000000000000000")
        |> V.andMap (V.field "participante" (V.string |> V.andThen V.nonEmpty))
        |> V.andMap (V.succeed "00000000000000000000000000")
        |> V.andMap (V.field "cantidad" (V.maybe V.int))



-- UPDATE


type Msg
    = NoOp
    | OpenClaimItemModal RepartijaItem
    | CloseClaimModal
    | ClaimFormMsg Form.Msg
    | CreateRepartijaReponded (WebData RepartijaClaim)


update : Store -> Msg -> Model -> ( Model, Effect Msg )
update store msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        OpenClaimItemModal item ->
            ( { model | isClaimModalOpen = Just item }
            , Effect.none
            )

        CloseClaimModal ->
            ( { model | isClaimModalOpen = Nothing }
            , Effect.none
            )

        ClaimFormMsg formMsg ->
            case formMsg of
                Form.Submit ->
                    case ( Form.getOutput model.claimForm, model.isClaimModalOpen ) of
                        ( Just claim, Just itemId ) ->
                            ( { model | isClaimModalOpen = Nothing }
                            , Effect.sendCmd <|
                                Api.putRepartijasByRepartijaId model.repartijaId
                                    { claim | repartijaClaimItemId = itemId.repartijaItemId }
                                    (RemoteData.fromResult >> CreateRepartijaReponded)
                            )

                        ( _, _ ) ->
                            ( model
                            , Effect.none
                            )

                _ ->
                    let
                        newForm =
                            Form.update validateClaim formMsg model.claimForm
                    in
                    ( { model
                        | claimForm = newForm
                      }
                    , Effect.none
                    )

        CreateRepartijaReponded newRepartijaClaim ->
            ( model
            , Effect.batch [ Store.refreshRepartija model.repartijaId store ]
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Store -> Model -> View Msg
view store model =
    { title = "Repartija"
    , body =
        case ( Store.getRepartija model.repartijaId store, Store.getGrupo model.grupoId store ) of
            ( Success repartija, Success grupo ) ->
                [ p [] [ text "Titulo: ", text repartija.repartijaNombre ]
                , p [] [ text "Extra: $", text <| Decimal.toString <| monto2Decimal repartija.repartijaExtra ]
                , div []
                    (repartija.repartijaItems
                        |> List.map
                            (\item ->
                                div []
                                    [ span [] [ text item.repartijaItemNombre ]
                                    , span [] [ text <| "[" ++ String.fromInt item.repartijaItemCantidad ++ "]" ]
                                    , span [] [ text <| "(" ++ (Decimal.toString <| monto2Decimal item.repartijaItemMonto) ++ ")" ]
                                    , div []
                                        (repartija.repartijaClaims
                                            |> List.filter (\claim -> claim.repartijaClaimItemId == item.repartijaItemId)
                                            |> List.map
                                                (\claim ->
                                                    viewClaimDetails grupo claim
                                                )
                                        )
                                    , button [ onClick <| OpenClaimItemModal item, class "button primary" ] [ text "claim" ]
                                    ]
                            )
                    )
                , viewClaimModal model repartija grupo.participantes
                ]

            ( Success repartija, _ ) ->
                [ text "cargando" ]

            ( NotAsked, _ ) ->
                [ text "ni idea" ]

            ( Loading, _ ) ->
                [ text "cargando" ]

            ( Failure e, _ ) ->
                [ text "falle" ]
    }


viewClaimDetails : Grupo -> RepartijaClaim -> Html Msg
viewClaimDetails grupo claim =
    div []
        [ text <| lookupNombreParticipante grupo claim.repartijaClaimParticipante
        , text "("
        , text <| Maybe.withDefault "Equitativo (?" <| Maybe.map String.fromInt claim.repartijaClaimCantidad
        , text ")"
        , button
            [ class "delete"
            , attribute "aria-label" "close"
            ]
            []
        ]


viewClaimModal : Model -> Repartija -> List Participante -> Html Msg
viewClaimModal model repartija participantes =
    let
        isOpen =
            case model.isClaimModalOpen of
                Just _ ->
                    True

                Nothing ->
                    False
    in
    div
        ([ class "modal"
         ]
            ++ (if isOpen then
                    [ class "is-active" ]

                else
                    []
               )
        )
        [ div
            [ class "modal-background"
            , onClick <| CloseClaimModal
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
                    , onClick <| CloseClaimModal
                    ]
                    []
                ]
            , section
                [ class "modal-card-body"
                ]
                [ pre [] [ text <| Debug.toString repartija ]
                , repartijaForm participantes model.claimForm
                , pre [] [ text <| Debug.toString <| Form.getOutput model.claimForm ]
                ]
            , footer
                [ class "modal-card-foot"
                ]
                [ div
                    [ class "buttons"
                    ]
                    [ button
                        [ class "button is-primary"
                        , onClick <| ClaimFormMsg Form.Submit
                        ]
                        [ text "Crear repartija" ]
                    , button
                        [ class "button"
                        , onClick CloseClaimModal
                        ]
                        [ text "Cancel" ]
                    ]
                ]
            ]
        ]


repartijaForm : List Participante -> Form CustomFormError RepartijaClaim -> Html Msg
repartijaForm participantes form =
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

        participanteField =
            Form.getFieldAsString "participante" form

        cantidadField =
            Form.getFieldAsString "cantidad" form
    in
    Html.form [ onSubmit <| ClaimFormMsg Form.Submit ]
        [ div [ class "field" ]
            [ label [ class "label" ]
                [ text "Participante" ]
            , div [ class "control" ]
                [ span [ class "select" ]
                    [ Html.map ClaimFormMsg <|
                        FormInput.selectInput
                            (( "", "Selecciona a un participante" ) :: List.map (\p -> ( p.participanteId, p.participanteNombre )) participantes)
                            participanteField
                            []
                    ]
                , errorFor participanteField
                ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ]
                [ text "Cantidad" ]
            , div [ class "control" ]
                [ Html.map ClaimFormMsg <|
                    FormInput.textInput cantidadField
                        [ class "input"
                        , type_ "text"
                        , placeholder "After del viernes"
                        , classList [ ( "is-danger", hasError cantidadField ) ]
                        ]
                , errorFor cantidadField
                ]
            ]
        ]
