module Pages.Grupos.GrupoId_.Repartijas.RepartijaId_ exposing (Model, Msg, page)

import Components.NavBar as NavBar
import Effect exposing (Effect)
import FeatherIcons
import Form exposing (Form)
import Form.Error as FormError
import Form.Field as Form
import Form.Input as FormInput
import Form.Validate as V
import Generated.Api as Api exposing (Grupo, Pago, Participante, ParticipanteId, Repartija, RepartijaClaim, RepartijaItem, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit)
import Layouts
import List.Extra exposing (find)
import Models.Grupo exposing (GrupoLike, lookupNombreParticipante)
import Models.Monto as Monto
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Numeric.Decimal as Decimal
import Numeric.Decimal.Rounding as Decimal
import Numeric.Nat as Nat
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Set
import Shared
import Utils.Form exposing (CustomFormError)
import Utils.Toasts as Toast exposing (..)
import Utils.Toasts.Types as Toast exposing (..)
import Utils.Ulid exposing (emptyUlid)
import View exposing (View)


page : Shared.Model -> Route { grupoId : String, repartijaId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.grupoId route.params.repartijaId shared.store
        , update = update shared.userId shared.store
        , subscriptions = subscriptions
        , view = view shared.userId shared.store
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
    , repartijaId : ULID
    , claimForm : Form CustomFormError RepartijaClaim
    , participanteClaimsModal : Maybe ParticipanteId
    }


init : ULID -> ULID -> Store -> ( Model, Effect Msg )
init grupoId repartijaId store =
    ( { grupoId = grupoId
      , repartijaId = repartijaId
      , claimForm = Form.initial [] validateClaim
      , participanteClaimsModal = Nothing
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
        |> V.andMap
            (V.field "cantidad"
                (V.oneOf
                    [ V.emptyString |> V.map (always Nothing)
                    , V.int |> V.map Just
                    ]
                )
            )



-- UPDATE


type Msg
    = NoOp
    | ChangeCurrentClaim RepartijaItem Int
    | JoinCurrentClaim RepartijaItem
    | LeaveCurrentClaim RepartijaClaim
    | OpenDesdoblarItemModal RepartijaItem
    | DeleteClaim RepartijaClaim
    | CreateRepartijaClaimResponded (WebData RepartijaClaim)
    | DeleteRepartijaClaimResponded (WebData String)
    | OpenParticipanteClaimsPopup ParticipanteId
    | CloseParticipanteClaimsPopup


update : Maybe ULID -> Store -> Msg -> Model -> ( Model, Effect Msg )
update maybeParticipanteId store msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        CreateRepartijaClaimResponded _ ->
            ( model
            , Effect.batch [ Store.refreshRepartija model.repartijaId ]
            )

        DeleteClaim repartijaClaim ->
            ( model
            , Effect.batch
                [ Effect.sendCmd <|
                    Api.deleteRepartijasClaimsByClaimId
                        repartijaClaim.id
                        (RemoteData.fromResult >> DeleteRepartijaClaimResponded)
                ]
            )

        DeleteRepartijaClaimResponded _ ->
            ( model
            , Effect.batch
                [ Store.refreshRepartija model.repartijaId
                ]
            )

        OpenDesdoblarItemModal repartijaItem ->
            ( model
            , Effect.none
            )

        OpenParticipanteClaimsPopup participanteId ->
            ( { model | participanteClaimsModal = Just participanteId }
            , Effect.none
            )

        CloseParticipanteClaimsPopup ->
            ( { model | participanteClaimsModal = Nothing }
            , Effect.none
            )

        ChangeCurrentClaim item deltaCantidad ->
            case store |> Store.getRepartija model.repartijaId |> RemoteData.toMaybe of
                Just repartija ->
                    case maybeParticipanteId of
                        Just participanteId ->
                            let
                                oldClaimFound =
                                    repartija.claims
                                        |> List.filter
                                            (\claim ->
                                                claim.participante == participanteId && claim.itemId == item.id
                                            )
                                        |> List.head
                            in
                            case oldClaimFound of
                                Just oldClaim ->
                                    let
                                        newCantidad =
                                            oldClaim
                                                |> .cantidad
                                                |> Maybe.withDefault 0
                                                |> (\x -> x + deltaCantidad)
                                                |> Basics.max 0
                                    in
                                    ( model
                                    , if newCantidad > 0 then
                                        Effect.sendCmd <|
                                            Api.putRepartijasByRepartijaId model.repartijaId
                                                { id = emptyUlid
                                                , participante = participanteId
                                                , itemId = item.id
                                                , cantidad = Just <| newCantidad
                                                }
                                                (RemoteData.fromResult >> CreateRepartijaClaimResponded)

                                      else
                                        Effect.sendCmd <|
                                            Api.deleteRepartijasClaimsByClaimId oldClaim.id
                                                (RemoteData.fromResult >> DeleteRepartijaClaimResponded)
                                    )

                                Nothing ->
                                    ( model
                                    , if deltaCantidad > 0 then
                                        Effect.sendCmd <|
                                            Api.putRepartijasByRepartijaId model.repartijaId
                                                { id = emptyUlid
                                                , participante = participanteId
                                                , itemId = item.id
                                                , cantidad = Just deltaCantidad
                                                }
                                                (RemoteData.fromResult >> CreateRepartijaClaimResponded)

                                      else
                                        Effect.none
                                    )

                        Nothing ->
                            ( model, Effect.none )

                Nothing ->
                    ( model, Effect.none )

        JoinCurrentClaim item ->
            case maybeParticipanteId of
                Just participanteId ->
                    ( model
                    , Effect.sendCmd <|
                        Api.putRepartijasByRepartijaId model.repartijaId
                            { id = emptyUlid
                            , participante = participanteId
                            , itemId = item.id
                            , cantidad = Nothing
                            }
                            (RemoteData.fromResult >> CreateRepartijaClaimResponded)
                    )

                Nothing ->
                    ( model, Effect.none )

        LeaveCurrentClaim item ->
            case maybeParticipanteId of
                Just participanteId ->
                    ( model
                    , Effect.sendCmd <|
                        Api.deleteRepartijasClaimsByClaimId item.id
                            (RemoteData.fromResult >> DeleteRepartijaClaimResponded)
                    )

                Nothing ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Maybe ULID -> Store -> Model -> View Msg
view userId store model =
    { title = "Repartija"
    , body =
        case ( Store.getRepartija model.repartijaId store, Store.getGrupo model.grupoId store ) of
            ( Success repartija, Success grupo ) ->
                [ viewParticipantes grupo repartija
                , viewRepartijaItems userId grupo repartija
                , viewParticipanteClaimsModal model grupo repartija

                -- , button
                --     [ class "button is-primary"
                --     , onClick CreatePago
                --     ]
                --     [ text "Crear Pago" ]
                ]

            ( Failure e, _ ) ->
                [ text "falle" ]

            ( _, Failure _ ) ->
                [ text "falle" ]

            ( _, _ ) ->
                [ text "cargando" ]
    }


viewParticipantes : GrupoLike g -> Repartija -> Html Msg
viewParticipantes grupo repartija =
    let
        participantesConClaims =
            repartija.claims
                |> List.map (\claim -> claim.participante)
                |> Set.fromList
    in
    div []
        [ div [ class "buttons has-addons my-5" ]
            (grupo.participantes
                |> List.map
                    (\participante ->
                        button
                            [ class "button is-success"
                            , if Set.member participante.participanteId participantesConClaims then
                                class "is-success is-light"

                              else
                                class "is-danger is-light"
                            , onClick <| OpenParticipanteClaimsPopup participante.participanteId
                            ]
                            [ text participante.participanteNombre
                            ]
                    )
            )
        ]


viewRepartijaItems : Maybe ULID -> GrupoLike g -> Repartija -> Html Msg
viewRepartijaItems userId grupo repartija =
    table [ class "table is-fullwidth is-striped is-hoverable" ] <|
        [ thead []
            [ tr []
                [ th [] [ text "Descripcion" ]
                , th [ class "has-text-right" ] [ text "Monto total" ]
                , th [ class "has-text-right" ] [ text "Cantidad" ]
                , th [] [ text "Repartido" ]
                , th [] []
                ]
            ]
        , tbody []
            (repartija.items
                |> List.map (\item -> viewClaimsLine userId grupo repartija item)
                |> (\x ->
                        List.append x
                            [ tr []
                                [ td [] [ text "Propina" ]
                                , td [ class "has-text-right" ] [ text "$", text <| Decimal.toString <| Monto.toDecimal repartija.extra ]
                                , td [] []
                                , td [] []
                                , td [] []
                                ]
                            , tr []
                                [ td [] [ text "Total" ]
                                , td [ class "has-text-right" ]
                                    [ text "$"
                                    , repartija.items
                                        |> List.map (\i -> Monto.toDecimal i.monto)
                                        |> List.foldl Decimal.add (Decimal.fromInt Decimal.RoundTowardsZero Nat.nat2 0)
                                        |> Decimal.add (Monto.toDecimal repartija.extra)
                                        |> Decimal.toString
                                        |> text
                                    ]
                                , td [] []
                                , td [] []
                                , td [] []
                                ]
                            ]
                   )
            )
        ]


interpretClaims : List RepartijaClaim -> ItemClaimsState
interpretClaims originalClaims =
    let
        folder : RepartijaClaim -> ItemClaimsState -> ItemClaimsState
        folder claim claimsState =
            case ( claimsState, claim.cantidad ) of
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
    List.foldl folder NoClaims originalClaims


viewClaimsLine : Maybe ULID -> GrupoLike g -> Repartija -> RepartijaItem -> Html Msg
viewClaimsLine userId grupo repartija item =
    let
        claimsForItem =
            repartija.claims
                |> List.filter (\claim -> claim.itemId == item.id)

        itemsClaimed : ItemClaimsState
        itemsClaimed =
            claimsForItem
                |> interpretClaims
    in
    tr []
        [ td [ class "is-vcentered" ] [ text <| item.nombre ]
        , td [ class "has-text-right is-vcentered" ]
            [ text <| "$" ++ Decimal.toString (Monto.toDecimal item.monto)
            ]
        , td [ class "has-text-right is-vcentered" ] [ text <| String.fromInt item.cantidad ]
        , td [] [ viewClaimProgressAndDropdown grupo repartija item claimsForItem itemsClaimed ]
        , td []
            [ case ( userId, itemsClaimed, claimsForItem |> find (\c -> Just c.participante == userId) ) of
                ( Nothing, _, _ ) ->
                    div [ class "buttons has-addons" ]
                        [ text "Selecciona tu nombre arriba a la derecha"
                        ]

                ( Just _, MixedClaims _, Just userClaim ) ->
                    div [ class "buttons has-addons" ]
                        [ button [ onClick <| ChangeCurrentClaim item 1, class "button is-link" ] [ text "+1" ]
                        , button [ onClick <| ChangeCurrentClaim item -1, class "button is-link" ] [ text "-1" ]
                        , button [ onClick <| JoinCurrentClaim item, class "button is-link" ] [ text "Participé" ]
                        , button [ onClick <| LeaveCurrentClaim userClaim, class "button is-link" ] [ text "Salirse" ]
                        ]

                ( Just _, MixedClaims _, Nothing ) ->
                    div [ class "buttons has-addons" ]
                        [ button [ onClick <| ChangeCurrentClaim item 1, class "button is-link" ] [ text "+1" ]
                        , button [ onClick <| JoinCurrentClaim item, class "button is-link" ] [ text "Participé" ]
                        ]

                ( Just _, OnlyExactClaims _, _ ) ->
                    div [ class "buttons has-addons" ]
                        [ button [ onClick <| ChangeCurrentClaim item 1, class "button is-link is-disabled" ] [ text "+1" ]
                        , button [ onClick <| ChangeCurrentClaim item -1, class "button is-link" ] [ text "-1" ]
                        ]

                ( Just _, OnlyParticipationClaims _, Just userClaim ) ->
                    div [ class "buttons has-addons" ]
                        [ button [ onClick <| LeaveCurrentClaim userClaim, class "button is-link" ] [ text "Salirse" ]
                        ]

                ( Just _, OnlyParticipationClaims _, Nothing ) ->
                    div [ class "buttons has-addons" ]
                        [ button [ onClick <| JoinCurrentClaim item, class "button is-link" ] [ text "Participé" ]
                        ]

                ( Just _, NoClaims, _ ) ->
                    div [ class "buttons has-addons" ]
                        [ button [ onClick <| ChangeCurrentClaim item 1, class "button is-link" ] [ text "+1" ]
                        , button [ onClick <| ChangeCurrentClaim item 1, class "button is-link", disabled True ] [ text "-1" ]
                        , button [ onClick <| JoinCurrentClaim item, class "button is-link" ] [ text "Participé" ]
                        ]
            ]
        ]


type ItemClaimsState
    = MixedClaims (List RepartijaClaim)
    | OnlyExactClaims (List ( Int, RepartijaClaim ))
    | OnlyParticipationClaims (List RepartijaClaim)
    | NoClaims


type ItemRepartidoState
    = SinRepartir
    | RepartidoIncorrectamente
    | RepartidoExactamente
    | RepartidoEquitativamenteEntre Int
    | FaltaRepartir Int
    | RepartidoDeMas Int


viewClaimProgressAndDropdown : GrupoLike g -> Repartija -> RepartijaItem -> List RepartijaClaim -> ItemClaimsState -> Html Msg
viewClaimProgressAndDropdown grupo repartija item claimsForItem itemsClaimed =
    let
        itemRepartidoState : ItemRepartidoState
        itemRepartidoState =
            case itemsClaimed of
                MixedClaims _ ->
                    RepartidoIncorrectamente

                OnlyExactClaims exactClaims ->
                    let
                        cantidadClaimeado =
                            exactClaims |> List.map Tuple.first |> List.sum

                        cantidadFaltante =
                            item.cantidad - cantidadClaimeado
                    in
                    case compare 0 cantidadFaltante of
                        LT ->
                            FaltaRepartir cantidadFaltante

                        EQ ->
                            RepartidoExactamente

                        GT ->
                            RepartidoDeMas (cantidadFaltante * -1)

                OnlyParticipationClaims repartijaClaims ->
                    RepartidoEquitativamenteEntre <| List.length repartijaClaims

                NoClaims ->
                    SinRepartir
    in
    div [ class "is-flex" ]
        [ div
            [ class "dropdown is-hoverable"
            ]
            [ div [ class "dropdown-trigger" ]
                [ viewRepartidoState itemRepartidoState
                ]
            , div
                [ class "dropdown-menu"
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
                                            lookupNombreParticipante grupo claim.participante
                                      in
                                      case claim.cantidad of
                                        Just cantidad ->
                                            text <| String.fromInt cantidad ++ " unidad para " ++ nombre

                                        Nothing ->
                                            text <| "parte para " ++ nombre
                                    , button
                                        [ class "delete ml-1"
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


viewRepartidoState : ItemRepartidoState -> Html Msg
viewRepartidoState itemRepartidoState =
    let
        classForRepartido : Attribute msg
        classForRepartido =
            case itemRepartidoState of
                SinRepartir ->
                    class "is-info"

                RepartidoIncorrectamente ->
                    class "is-danger"

                RepartidoExactamente ->
                    class "is-success"

                RepartidoEquitativamenteEntre _ ->
                    class "is-success"

                FaltaRepartir _ ->
                    class "is-warning"

                RepartidoDeMas _ ->
                    class "is-warning"
    in
    button
        [ class "button"
        , class "is-outlined"
        , classForRepartido
        , style "pointer-events" "none"
        , attribute "aria-haspopup" "true"
        ]
        [ case itemRepartidoState of
            SinRepartir ->
                text "Sin repartir"

            RepartidoIncorrectamente ->
                text "Mal repartido"

            RepartidoExactamente ->
                i [ class "icon" ] <| [ FeatherIcons.toHtml [] FeatherIcons.check ]

            RepartidoEquitativamenteEntre cantidadDeParticipantes ->
                text <| "Equitativo entre " ++ String.fromInt cantidadDeParticipantes

            FaltaRepartir restoARepartir ->
                text <| "Falta repartir " ++ String.fromInt restoARepartir

            RepartidoDeMas sobras ->
                text <| "Sobran " ++ String.fromInt sobras
        , FeatherIcons.chevronDown
            |> FeatherIcons.toHtml []
        ]


viewParticipanteClaimsModal : Model -> GrupoLike g -> Repartija -> Html Msg
viewParticipanteClaimsModal model grupo repartija =
    case model.participanteClaimsModal of
        Just participanteId ->
            let
                claims =
                    repartija.claims
                        |> List.filter (\c -> c.participante == participanteId)

                participanteNombre =
                    lookupNombreParticipante grupo participanteId

                lookupItemName : ULID -> String
                lookupItemName itemId =
                    repartija.items
                        |> List.filter (\item -> item.id == itemId)
                        |> List.head
                        |> Maybe.map .nombre
                        |> Maybe.withDefault "Item desconocido"
            in
            div
                [ class "modal is-active" ]
                [ div
                    [ class "modal-background"
                    , onClick CloseParticipanteClaimsPopup
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
                            [ text <| "Asignaciones para " ++ participanteNombre ]
                        , button
                            [ class "delete"
                            , attribute "aria-label" "close"
                            , onClick CloseParticipanteClaimsPopup
                            ]
                            []
                        ]
                    , section
                        [ class "modal-card-body"
                        ]
                        [ table [ class "table is-hoverable" ] <|
                            (tr []
                                [ th [] [ text "Nombre" ]
                                , th [] [ text "Cantidad" ]
                                ]
                                :: (claims
                                        |> List.map
                                            (\claim ->
                                                tr []
                                                    [ td [] [ text <| lookupItemName claim.itemId ]
                                                    , td []
                                                        [ text
                                                            (claim.cantidad
                                                                |> Maybe.map String.fromInt
                                                                |> Maybe.withDefault "Equitativo"
                                                            )
                                                        ]
                                                    ]
                                            )
                                   )
                            )
                        ]
                    ]
                ]

        Nothing ->
            div [] []
