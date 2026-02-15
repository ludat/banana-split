module Pages.Grupos.GrupoId_.Repartijas.RepartijaId_ exposing (Model, Msg, page)

import Components.NavBar as NavBar
import Effect exposing (Effect)
import Form exposing (Form)
import Form.Validate as V
import Generated.Api as Api exposing (ParticipanteId, Repartija, RepartijaClaim, RepartijaItem, ULID)
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class, id, style)
import Html.Events exposing (on, onClick)
import Json.Decode
import Json.Encode
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
import Utils.Http exposing (viewHttpError)
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
                    , grupo = Store.getGrupo m.grupoId shared.store
                    }
            )



-- INIT


type alias Model =
    { grupoId : ULID
    , repartijaId : ULID
    , claimForm : Form CustomFormError RepartijaClaim
    , participanteClaimsModal : Maybe ParticipanteId
    , pendingItemOperation : Maybe ULID
    , openPopoverItemId : Maybe ULID
    , pickSchemeItemId : Maybe ULID
    }


init : ULID -> ULID -> Store -> ( Model, Effect Msg )
init grupoId repartijaId store =
    ( { grupoId = grupoId
      , repartijaId = repartijaId
      , claimForm = Form.initial [] validateClaim
      , participanteClaimsModal = Nothing
      , pendingItemOperation = Nothing
      , openPopoverItemId = Nothing
      , pickSchemeItemId = Nothing
      }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Store.ensureRepartija repartijaId store
        , Effect.getCurrentUser grupoId
        , Effect.setUnsavedChangesWarning False
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
    | CreateRepartijaClaimResponded (WebData RepartijaClaim)
    | DeleteRepartijaClaimResponded ULID (WebData String)
    | OpenParticipanteClaimsPopup ParticipanteId
    | CloseParticipanteClaimsPopup
    | ToggleItemPopover ULID
    | CloseItemPopover
    | TogglePickScheme ULID
    | ClosePickScheme


update : Maybe ULID -> Store -> Msg -> Model -> ( Model, Effect Msg )
update maybeParticipanteId store msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        CreateRepartijaClaimResponded webDataClaim ->
            case ( webDataClaim, Store.getRepartija model.repartijaId store ) of
                ( Success newClaim, Success repartija ) ->
                    let
                        updatedRepartija =
                            { repartija
                                | claims =
                                    repartija.claims
                                        |> List.filter (\c -> not (c.itemId == newClaim.itemId && c.participante == newClaim.participante))
                                        |> (::) newClaim
                            }
                    in
                    ( { model | pendingItemOperation = Nothing }
                    , Store.updateRepartija model.repartijaId updatedRepartija
                    )

                _ ->
                    ( { model | pendingItemOperation = Nothing }
                    , Effect.batch [ Store.refreshRepartija model.repartijaId ]
                    )

        DeleteRepartijaClaimResponded claimId webDataResponse ->
            case ( webDataResponse, Store.getRepartija model.repartijaId store ) of
                ( Success _, Success repartija ) ->
                    let
                        updatedRepartija =
                            { repartija
                                | claims = repartija.claims |> List.filter (\c -> c.id /= claimId)
                            }
                    in
                    ( { model | pendingItemOperation = Nothing }
                    , Store.updateRepartija model.repartijaId updatedRepartija
                    )

                _ ->
                    ( { model | pendingItemOperation = Nothing }
                    , Effect.batch [ Store.refreshRepartija model.repartijaId ]
                    )

        OpenParticipanteClaimsPopup participanteId ->
            ( { model | participanteClaimsModal = Just participanteId }
            , Effect.none
            )

        CloseParticipanteClaimsPopup ->
            ( { model | participanteClaimsModal = Nothing }
            , Effect.none
            )

        ToggleItemPopover itemId ->
            ( { model
                | openPopoverItemId =
                    if model.openPopoverItemId == Just itemId then
                        Nothing

                    else
                        Just itemId
              }
            , Effect.none
            )

        CloseItemPopover ->
            ( { model | openPopoverItemId = Nothing }
            , Effect.none
            )

        TogglePickScheme itemId ->
            ( { model
                | pickSchemeItemId =
                    if model.pickSchemeItemId == Just itemId then
                        Nothing

                    else
                        Just itemId
              }
            , Effect.none
            )

        ClosePickScheme ->
            ( { model | pickSchemeItemId = Nothing }
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
                                    ( { model | pendingItemOperation = Just item.id, pickSchemeItemId = Nothing }
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
                                                (RemoteData.fromResult >> DeleteRepartijaClaimResponded oldClaim.id)
                                    )

                                Nothing ->
                                    ( { model | pendingItemOperation = Just item.id, pickSchemeItemId = Nothing }
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
                    ( { model | pendingItemOperation = Just item.id, pickSchemeItemId = Nothing }
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
                Just _ ->
                    ( { model | pendingItemOperation = Just item.itemId }
                    , Effect.sendCmd <|
                        Api.deleteRepartijasClaimsByClaimId item.id
                            (RemoteData.fromResult >> DeleteRepartijaClaimResponded item.id)
                    )

                Nothing ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Maybe ULID -> Store -> Model -> View Msg
view userId store model =
    case ( Store.getRepartija model.repartijaId store, Store.getGrupo model.grupoId store ) of
        ( Success repartija, Success grupo ) ->
            { title = grupo.nombre ++ ": " ++ repartija.nombre
            , body =
                [ viewParticipantes grupo repartija
                , viewRepartijaItems userId grupo repartija model.openPopoverItemId model.pickSchemeItemId
                , viewParticipanteClaimsModal model grupo repartija

                -- , button
                --     [ class "button is-primary"
                --     , onClick CreatePago
                --     ]
                --     [ text "Crear Pago" ]
                ]
            }

        ( Failure e1, Failure e2 ) ->
            { title = ""
            , body =
                [ text "falle"
                , viewHttpError e1
                , viewHttpError e2
                ]
            }

        ( Failure e, _ ) ->
            { title = ""
            , body =
                [ text "falle"
                , viewHttpError e
                ]
            }

        ( _, Failure e ) ->
            { title = ""
            , body =
                [ text "falle"
                , viewHttpError e
                ]
            }

        _ ->
            { title = ""
            , body =
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
        [ div [ style "display" "flex", style "gap" "0.5rem", style "flex-wrap" "wrap", style "margin" "1.25rem 0" ]
            (grupo.participantes
                |> List.map
                    (\participante ->
                        Html.node "ui5-button"
                            [ Attr.attribute "design"
                                (if Set.member participante.participanteId participantesConClaims then
                                    "Positive"

                                 else
                                    "Negative"
                                )
                            , id ("participante-btn-" ++ participante.participanteId)
                            , onClick <| OpenParticipanteClaimsPopup participante.participanteId
                            ]
                            [ text participante.participanteNombre
                            ]
                    )
            )
        ]


viewRepartijaItems : Maybe ULID -> GrupoLike g -> Repartija -> Maybe ULID -> Maybe ULID -> Html Msg
viewRepartijaItems userId grupo repartija openPopoverItemId pickSchemeItemId =
    Html.node "ui5-table"
        [ Attr.attribute "alternate-row-colors" ""
        , Attr.attribute "row-action-count" "5"
        ]
        (Html.node "ui5-table-header-row"
            [ Attr.attribute "slot" "headerRow" ]
            [ Html.node "ui5-table-header-cell" [] [ text "Descripcion" ]
            , Html.node "ui5-table-header-cell" [ Attr.attribute "horizontal-align" "End" ] [ text "Monto total" ]
            , Html.node "ui5-table-header-cell" [ Attr.attribute "horizontal-align" "End" ] [ text "Cantidad" ]
            , Html.node "ui5-table-header-cell" [ Attr.attribute "horizontal-align" "Center" ] [ text "Repartido" ]
            ]
            :: (repartija.items
                    |> List.map (\item -> viewClaimsLine userId grupo repartija item openPopoverItemId pickSchemeItemId)
               )
            ++ [ Html.node "ui5-table-row"
                    []
                    [ Html.node "ui5-table-cell" [] [ text "Propina" ]
                    , Html.node "ui5-table-cell" [] [ text "$", text <| Decimal.toString <| Monto.toDecimal repartija.extra ]
                    , Html.node "ui5-table-cell" [] []
                    , Html.node "ui5-table-cell" [] []
                    ]
               , Html.node "ui5-table-row"
                    []
                    [ Html.node "ui5-table-cell" [] [ text "Total" ]
                    , Html.node "ui5-table-cell"
                        []
                        [ text "$"
                        , repartija.items
                            |> List.map (\i -> Monto.toDecimal i.monto)
                            |> List.foldl Decimal.add (Decimal.fromInt Decimal.RoundTowardsZero Nat.nat2 0)
                            |> Decimal.add (Monto.toDecimal repartija.extra)
                            |> Decimal.toString
                            |> text
                        ]
                    , Html.node "ui5-table-cell" [] []
                    , Html.node "ui5-table-cell" [] []
                    ]
               ]
        )


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


viewClaimsLine : Maybe ULID -> GrupoLike g -> Repartija -> RepartijaItem -> Maybe ULID -> Maybe ULID -> Html Msg
viewClaimsLine userId grupo repartija item openPopoverItemId pickSchemeItemId =
    let
        claimsForItem =
            repartija.claims
                |> List.filter (\claim -> claim.itemId == item.id)

        itemsClaimed : ItemClaimsState
        itemsClaimed =
            interpretClaims claimsForItem

        itemRepartidoState : ItemRepartidoState
        itemRepartidoState =
            calculateItemRepartidoState item itemsClaimed

        userClaim =
            claimsForItem |> find (\c -> Just c.participante == userId)

        allHidden =
            { plus1 = False, minus1 = False, participe = False, salirse = False, repartir = False }

        visibility =
            case ( userId, itemRepartidoState, userClaim ) of
                ( Nothing, _, _ ) ->
                    allHidden

                ( Just _, RepartidoIncorrectamente, Just _ ) ->
                    { allHidden | plus1 = True, minus1 = True }

                ( Just _, RepartidoIncorrectamente, Nothing ) ->
                    { allHidden | plus1 = True }

                ( Just _, RepartidoExactamente _, Just _ ) ->
                    { allHidden | plus1 = True, minus1 = True }

                ( Just _, RepartidoExactamente _, Nothing ) ->
                    { allHidden | plus1 = True }

                ( Just _, RepartidoEquitativamenteEntre _, Nothing ) ->
                    { allHidden | participe = True }

                ( Just _, RepartidoEquitativamenteEntre _, Just _ ) ->
                    { allHidden | salirse = True }

                ( Just _, SinRepartir, _ ) ->
                    { allHidden | repartir = True }

        pickSchemeButtonId =
            "pick-scheme-" ++ item.id

        rowAction iconName textLabel maybeId msg isVisible =
            Html.node "ui5-table-row-action"
                [ Attr.attribute "slot" "actions"
                , Attr.attribute "icon" iconName
                , Attr.attribute "text" textLabel
                , Attr.attribute "tooltip" textLabel
                , on "click" (Json.Decode.succeed msg)
                , case maybeId of
                    Just id_ ->
                        id id_

                    Nothing ->
                        class ""
                , Attr.property "invisible" <|
                    Json.Encode.bool (not isVisible)
                ]
                []
    in
    Html.node "ui5-table-row"
        []
        [ Html.node "ui5-table-cell" [] [ text <| item.nombre ]
        , Html.node "ui5-table-cell" [] [ text <| "$" ++ Decimal.toString (Monto.toDecimal item.monto) ]
        , Html.node "ui5-table-cell" [] [ text <| String.fromInt item.cantidad ]
        , Html.node "ui5-table-cell"
            []
            [ viewClaimProgressAndDropdown grupo item claimsForItem itemRepartidoState openPopoverItemId
            , Html.node "ui5-responsive-popover"
                ([ Attr.attribute "opener" pickSchemeButtonId
                 , Attr.attribute "placement" "Bottom"
                 , on "close" (Json.Decode.succeed ClosePickScheme)
                 ]
                    ++ (if pickSchemeItemId == Just item.id then
                            [ Attr.attribute "open" "" ]

                        else
                            []
                       )
                )
                [ Html.node "ui5-list"
                    [ Attr.attribute "header-text" "Elegí cómo repartir" ]
                    [ Html.node "ui5-li"
                        [ Attr.attribute "icon" "add"
                        , Attr.attribute "description" "Indicás cuántas unidades consumió cada persona"
                        , on "click" (Json.Decode.succeed (ChangeCurrentClaim item 1))
                        ]
                        [ text "Por cantidad" ]
                    , Html.node "ui5-li"
                        [ Attr.attribute "icon" "accept"
                        , Attr.attribute "description" "Se divide en partes iguales entre los que participaron"
                        , on "click" (Json.Decode.succeed (JoinCurrentClaim item))
                        ]
                        [ text "Equitativo" ]
                    ]
                ]
            ]
        , rowAction "edit" "Repartir" (Just pickSchemeButtonId) (TogglePickScheme item.id) visibility.repartir
        , rowAction "add" "+1" Nothing (ChangeCurrentClaim item 1) visibility.plus1
        , rowAction "sys-minus" "-1" Nothing (ChangeCurrentClaim item -1) visibility.minus1
        , rowAction "accept" "Participé" Nothing (JoinCurrentClaim item) visibility.participe
        , rowAction "decline"
            "Salirse"
            Nothing
            (case userClaim of
                Just claim ->
                    LeaveCurrentClaim claim

                Nothing ->
                    NoOp
            )
            visibility.salirse
        ]


type Delta
    = SePasaPor Int
    | QuedaCortoPor Int
    | ExactamenteCero


compararConCero : Int -> Delta
compararConCero n =
    case compare n 0 of
        GT ->
            SePasaPor n

        EQ ->
            ExactamenteCero

        LT ->
            QuedaCortoPor <| negate n


type ItemClaimsState
    = MixedClaims (List RepartijaClaim)
    | OnlyExactClaims (List ( Int, RepartijaClaim ))
    | OnlyParticipationClaims (List RepartijaClaim)
    | NoClaims


type ItemRepartidoState
    = SinRepartir
    | RepartidoIncorrectamente
    | RepartidoExactamente { deltaDeCantidad : Int }
    | RepartidoEquitativamenteEntre { cantidadDeParticipantes : Int }


calculateItemRepartidoState : RepartijaItem -> ItemClaimsState -> ItemRepartidoState
calculateItemRepartidoState item itemsClaimed =
    case itemsClaimed of
        MixedClaims _ ->
            RepartidoIncorrectamente

        OnlyExactClaims exactClaims ->
            let
                cantidadClaimeado =
                    exactClaims |> List.map Tuple.first |> List.sum

                cantidadFaltante =
                    cantidadClaimeado - item.cantidad
            in
            RepartidoExactamente { deltaDeCantidad = cantidadFaltante }

        OnlyParticipationClaims repartijaClaims ->
            RepartidoEquitativamenteEntre { cantidadDeParticipantes = List.length repartijaClaims }

        NoClaims ->
            SinRepartir


viewClaimProgressAndDropdown : GrupoLike g -> RepartijaItem -> List RepartijaClaim -> ItemRepartidoState -> Maybe ULID -> Html Msg
viewClaimProgressAndDropdown grupo item claimsForItem itemRepartidoState openPopoverItemId =
    let
        statusButtonId =
            "repartido-status-" ++ item.id

        isPopoverOpen =
            openPopoverItemId == Just item.id
    in
    div [ style "display" "flex" ]
        [ viewRepartidoState itemRepartidoState statusButtonId item.id
        , Html.node "ui5-responsive-popover"
            ([ Attr.attribute "opener" statusButtonId
             , Attr.attribute "placement" "Bottom"
             , on "close" (Json.Decode.succeed CloseItemPopover)
             ]
                ++ (if isPopoverOpen then
                        [ Attr.attribute "open" "" ]

                    else
                        []
                   )
            )
            [ Html.node "ui5-list"
                []
                (claimsForItem
                    |> List.map
                        (\claim ->
                            Html.node "ui5-li"
                                [ Attr.attribute "description"
                                    (case claim.cantidad of
                                        Just cantidad ->
                                            String.fromInt cantidad ++ " unidad"

                                        Nothing ->
                                            "Equitativo"
                                    )
                                ]
                                [ text (lookupNombreParticipante grupo claim.participante)
                                ]
                        )
                )
            ]
        ]


viewRepartidoState : ItemRepartidoState -> String -> ULID -> Html Msg
viewRepartidoState itemRepartidoState buttonId itemId =
    let
        designForRepartido : String
        designForRepartido =
            case itemRepartidoState of
                SinRepartir ->
                    "Default"

                RepartidoIncorrectamente ->
                    "Negative"

                RepartidoExactamente { deltaDeCantidad } ->
                    if deltaDeCantidad == 0 then
                        "Positive"

                    else
                        "Attention"

                RepartidoEquitativamenteEntre _ ->
                    "Positive"
    in
    Html.node "ui5-button"
        [ Attr.attribute "design" designForRepartido
        , id buttonId
        , onClick (ToggleItemPopover itemId)
        ]
        [ case itemRepartidoState of
            SinRepartir ->
                text "Sin repartir"

            RepartidoIncorrectamente ->
                text "Mal repartido"

            RepartidoExactamente { deltaDeCantidad } ->
                case compararConCero deltaDeCantidad of
                    ExactamenteCero ->
                        text "✓"

                    QuedaCortoPor n ->
                        text <| "Falta repartir " ++ String.fromInt n

                    SePasaPor n ->
                        text <| "Sobran " ++ String.fromInt n

            RepartidoEquitativamenteEntre { cantidadDeParticipantes } ->
                text <| "Equitativo entre " ++ String.fromInt cantidadDeParticipantes
        ]


viewParticipanteClaimsModal : Model -> GrupoLike g -> Repartija -> Html Msg
viewParticipanteClaimsModal model grupo repartija =
    case model.participanteClaimsModal of
        Just participanteId ->
            let
                claims : List RepartijaClaim
                claims =
                    repartija.claims
                        |> List.filter (\c -> c.participante == participanteId)

                participanteNombre : String
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
            Html.node "ui5-responsive-popover"
                [ Attr.attribute "open" ""
                , Attr.attribute "opener" ("participante-btn-" ++ participanteId)
                , Attr.attribute "header-text" ("Asignaciones para " ++ participanteNombre)
                , Attr.attribute "placement" "Bottom"
                , on "close" (Json.Decode.succeed CloseParticipanteClaimsPopup)
                ]
                [ Html.node "ui5-list"
                    []
                    (claims
                        |> List.map
                            (\claim ->
                                Html.node "ui5-li"
                                    [ Attr.attribute "description"
                                        (claim.cantidad
                                            |> Maybe.map (\c -> String.fromInt c ++ " unidad")
                                            |> Maybe.withDefault "Equitativo"
                                        )
                                    ]
                                    [ text <| lookupItemName claim.itemId ]
                            )
                    )
                ]

        Nothing ->
            text ""
