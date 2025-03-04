module Pages.Grupos.GrupoId_.Repartijas.RepartijaId_ exposing (Model, Msg, page)

import Components.NavBar as NavBar
import Effect exposing (Effect)
import FeatherIcons
import Form exposing (Form)
import Form.Error as FormError
import Form.Input as FormInput
import Form.Validate as V
import Generated.Api as Api exposing (Grupo, Participante, Repartija, RepartijaClaim, RepartijaItem, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit)
import Layouts
import Models.Grupo exposing (lookupNombreParticipante)
import Models.Monto exposing (montoToDecimal)
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Numeric.Decimal as Decimal
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Shared
import Utils.Form exposing (CustomFormError)
import Utils.Toasts as Toast exposing (..)
import Utils.Toasts.Types as Toast exposing (..)
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
    | OpenDesdoblarItemModal RepartijaItem
    | CloseClaimModal
    | ClaimFormMsg Form.Msg
    | DeleteClaim RepartijaClaim
    | CreateRepartijaResponded (WebData RepartijaClaim)
    | DeleteRepartijaClaimResponded (WebData String)
    | CreatePago
    | CreatePagoResponded (WebData String)


update : Store -> Msg -> Model -> ( Model, Effect Msg )
update store msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        OpenClaimItemModal item ->
            ( { model
                | isClaimModalOpen = Just item
                , claimForm = Form.initial [] validateClaim
              }
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
                                    (RemoteData.fromResult >> CreateRepartijaResponded)
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

        CreateRepartijaResponded _ ->
            ( model
            , Effect.batch [ Store.refreshRepartija model.repartijaId ]
            )

        DeleteClaim repartijaClaim ->
            ( model
            , Effect.batch
                [ Effect.sendCmd <|
                    Api.deleteRepartijasClaimsByClaimId
                        repartijaClaim.repartijaClaimId
                        (RemoteData.fromResult >> DeleteRepartijaClaimResponded)
                ]
            )

        DeleteRepartijaClaimResponded _ ->
            ( model
            , Effect.batch
                [ Store.refreshRepartija model.repartijaId
                , pushToast ToastSuccess "Asignacion borrada."
                ]
            )

        CreatePago ->
            ( model
            , Effect.batch
                [ Effect.sendCmd <| Api.postRepartijasByRepartijaId model.repartijaId (RemoteData.fromResult >> CreatePagoResponded)
                ]
            )

        CreatePagoResponded reponse ->
            ( model
            , Effect.batch
                [ Store.refreshGrupo model.grupoId
                , Store.refreshNetos model.grupoId
                , if RemoteData.isSuccess reponse then
                    Toast.pushToast Toast.ToastSuccess "Pago creado."

                  else
                    Toast.pushToast Toast.ToastDanger "Fallo la creacion del pago."
                ]
            )

        OpenDesdoblarItemModal repartijaItem ->
            ( model
            , Effect.none
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
                [ div [] [ text "Titulo: ", text repartija.repartijaNombre ]
                , viewRepartijaItems grupo repartija
                , viewClaimModal model repartija grupo.participantes
                , div [] [ text "Extra: $", text <| Decimal.toString <| montoToDecimal repartija.repartijaExtra ]
                , button
                    [ class "button is-primary"
                    , onClick CreatePago
                    ]
                    [ text "Crear Pago" ]
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


viewRepartijaItems : Grupo -> Repartija -> Html Msg
viewRepartijaItems grupo repartija =
    table [ class "table is-striped is-hoverable" ] <|
        [ thead []
            [ tr []
                [ th [] [ text "Descripcion" ]
                , th [] [ text "Monto total" ]
                , th [] [ text "Cantidad" ]
                , th [] [ text "Repartido" ]
                , th [] []
                ]
            ]
        ]
            ++ [ tbody [] <|
                    (repartija.repartijaItems
                        |> List.map
                            (\item -> viewClaimsLine grupo repartija item)
                    )
               ]


viewClaimsLine : Grupo -> Repartija -> RepartijaItem -> Html Msg
viewClaimsLine grupo repartija item =
    let
        claimsForItem =
            repartija.repartijaClaims
                |> List.filter (\claim -> claim.repartijaClaimItemId == item.repartijaItemId)

        itemsClaimed : Claims
        itemsClaimed =
            claimsForItem
                |> List.foldl coso NoClaims

        coso : RepartijaClaim -> Claims -> Claims
        coso claim claimsState =
            case ( claimsState, claim.repartijaClaimCantidad ) of
                ( NoClaims, Just n ) ->
                    OnlyExactClaims [ ( n, claim ) ]

                ( NoClaims, Nothing ) ->
                    OnlyParticipationClaims [ claim ]

                ( MixedClaims claims, _ ) ->
                    MixedClaims <| claims ++ [ claim ]

                ( OnlyExactClaims claims, Nothing ) ->
                    MixedClaims (List.map Tuple.second claims ++ [ claim ])

                ( OnlyExactClaims claims, Just n ) ->
                    OnlyExactClaims (claims ++ [ ( n, claim ) ])

                ( OnlyParticipationClaims claims, Nothing ) ->
                    OnlyParticipationClaims <| claims ++ [ claim ]

                ( OnlyParticipationClaims claims, Just _ ) ->
                    MixedClaims (claims ++ [ claim ])
    in
    tr []
        [ td [] [ text <| item.repartijaItemNombre ]
        , td [ class "has-text-right" ]
            [ text <| "$" ++ Decimal.toString (montoToDecimal item.repartijaItemMonto)
            ]
        , td [ class "has-text-right" ] [ text <| String.fromInt item.repartijaItemCantidad ]
        , td [] [ viewClaimProgressAndDropdown grupo repartija item claimsForItem itemsClaimed ]
        , td []
            [ div [ class "buttons has-addons" ]
                [ button [ onClick <| OpenClaimItemModal item, class "button is-info" ] [ text "Claim" ]
                , button [ onClick <| OpenDesdoblarItemModal item, class "button" ] [ text "Desdoblar" ]
                ]
            ]
        ]


type Claims
    = MixedClaims (List RepartijaClaim)
    | OnlyExactClaims (List ( Int, RepartijaClaim ))
    | OnlyParticipationClaims (List RepartijaClaim)
    | NoClaims


viewClaimProgressAndDropdown : Grupo -> Repartija -> RepartijaItem -> List RepartijaClaim -> Claims -> Html Msg
viewClaimProgressAndDropdown grupo repartija item claimsForItem itemsClaimed =
    let
        textoDeConsumido =
            case itemsClaimed of
                MixedClaims _ ->
                    text <| "??/" ++ String.fromInt item.repartijaItemCantidad

                OnlyExactClaims exactClaims ->
                    let
                        cantidadClaimeado =
                            exactClaims |> List.map Tuple.first |> List.sum

                        cantidadFaltante =
                            item.repartijaItemCantidad - cantidadClaimeado
                    in
                    case compare 0 cantidadFaltante of
                        LT ->
                            text <| "Falta repartir " ++ String.fromInt cantidadFaltante

                        EQ ->
                            i [ class "icon" ] <| [ FeatherIcons.toHtml [] FeatherIcons.check ]

                        GT ->
                            text <| "Sobran " ++ String.fromInt (cantidadFaltante * -1)

                OnlyParticipationClaims repartijaClaims ->
                    text <| "Equitativo entre " ++ (String.fromInt <| List.length repartijaClaims)

                NoClaims ->
                    text "Sin repartir"
    in
    div [ class "is-flex" ]
        [ div
            [ class "dropdown is-hoverable"
            ]
            [ div
                [ class "dropdown-trigger"
                ]
                [ button [ class "button", attribute "aria-haspopup" "true" ]
                    [ textoDeConsumido
                    , FeatherIcons.chevronDown
                        |> FeatherIcons.toHtml []
                    ]
                ]
            , div
                [ class "dropdown-menu"
                , id "dropdown-menu3"
                , attribute "role" "menu"
                ]
                [ div
                    [ class "dropdown-content"
                    ]
                    (claimsForItem
                        |> List.map
                            (\claim ->
                                div [ class "dropdown-item" ]
                                    [ let
                                        nombre =
                                            lookupNombreParticipante grupo claim.repartijaClaimParticipante
                                      in
                                      case claim.repartijaClaimCantidad of
                                        Just cantidad ->
                                            text <| String.fromInt cantidad ++ " unidad para " ++ nombre

                                        Nothing ->
                                            text <| "parte para " ++ nombre
                                    , button
                                        [ class "delete"
                                        , attribute "aria-label" "close"
                                        , onClick <| DeleteClaim claim
                                        ]
                                        []
                                    ]
                            )
                    )
                ]
            ]
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
                    [ text "Reclamar un item" ]
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
                [ repartijaForm participantes model.claimForm
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
