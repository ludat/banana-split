module Pages.Grupos.GrupoId_.Repartijas.RepartijaId_ exposing (Model, Msg, page)

import Components.Bootstrap as Bs
import Effect exposing (Effect)
import Form exposing (Form)
import Form.Validate as V
import Generated.Api as Api exposing (Repartija, RepartijaClaim, RepartijaItem, ULID)
import Html exposing (Html, button, div, i, span, text)
import Html.Attributes as Attr exposing (class, disabled, id, style, type_)
import Html.Events exposing (onClick)
import Layouts
import List.Extra exposing (find)
import Models.Grupo exposing (GrupoLike, lookupNombreParticipante)
import Models.Monto as Monto
import Models.Store as Store
import Models.Store.Types exposing (Store)
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
        |> Page.withLayout (\_ -> Layouts.Default_Grupo {})



-- INIT


type alias Model =
    { grupoId : ULID
    , repartijaId : ULID
    , claimForm : Form CustomFormError RepartijaClaim
    , pendingItemOperation : Maybe ULID
    , repartirModalItem : Maybe RepartijaItem
    }


init : ULID -> ULID -> Store -> ( Model, Effect Msg )
init grupoId repartijaId store =
    ( { grupoId = grupoId
      , repartijaId = repartijaId
      , claimForm = Form.initial [] validateClaim
      , pendingItemOperation = Nothing
      , repartirModalItem = Nothing
      }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Store.refreshRepartija repartijaId
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
    | OpenRepartirModal RepartijaItem
    | CloseRepartirModal
    | ChangeCurrentClaim RepartijaItem Int
    | JoinCurrentClaim RepartijaItem
    | LeaveCurrentClaim RepartijaClaim
    | CreateRepartijaClaimResponded (WebData RepartijaClaim)
    | DeleteRepartijaClaimResponded ULID (WebData String)


update : Maybe ULID -> Store -> Msg -> Model -> ( Model, Effect Msg )
update maybeParticipanteId store msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        OpenRepartirModal item ->
            ( { model | repartirModalItem = Just item }
            , Effect.none
            )

        CloseRepartirModal ->
            ( { model | repartirModalItem = Nothing }
            , Effect.none
            )

        CreateRepartijaClaimResponded webDataClaim ->
            case ( webDataClaim, Store.getRepartija model.repartijaId store ) of
                ( Success newClaim, Success repartijaPage ) ->
                    let
                        repartija =
                            repartijaPage.repartija

                        updatedRepartija =
                            { repartija
                                | claims =
                                    repartija.claims
                                        |> List.filter (\c -> not (c.itemId == newClaim.itemId && c.participante == newClaim.participante))
                                        |> (::) newClaim
                            }
                    in
                    ( { model | pendingItemOperation = Nothing }
                    , Effect.batch
                        [ Store.updateRepartijaForFrontend model.repartijaId { repartijaPage | repartija = updatedRepartija }
                        , Store.invalidateResumen model.grupoId
                        , Store.invalidatePagos model.grupoId
                        , Store.refreshRepartija model.repartijaId
                        ]
                    )

                _ ->
                    ( { model | pendingItemOperation = Nothing }
                    , Effect.batch
                        [ Store.refreshRepartija model.repartijaId
                        ]
                    )

        DeleteRepartijaClaimResponded claimId webDataResponse ->
            case ( webDataResponse, Store.getRepartija model.repartijaId store ) of
                ( Success _, Success repartijaPage ) ->
                    let
                        repartija =
                            repartijaPage.repartija

                        updatedRepartija =
                            { repartija
                                | claims = repartija.claims |> List.filter (\c -> c.id /= claimId)
                            }
                    in
                    ( { model | pendingItemOperation = Nothing }
                    , Effect.batch
                        [ Store.updateRepartijaForFrontend model.repartijaId { repartijaPage | repartija = updatedRepartija }
                        , Store.invalidateResumen model.grupoId
                        , Store.invalidatePagos model.grupoId
                        , Store.refreshRepartija model.repartijaId
                        ]
                    )

                _ ->
                    ( { model | pendingItemOperation = Nothing }
                    , Effect.batch [ Store.refreshRepartija model.repartijaId ]
                    )

        ChangeCurrentClaim item deltaCantidad ->
            case store |> Store.getRepartija model.repartijaId |> RemoteData.toMaybe of
                Just repartijaPage ->
                    case maybeParticipanteId of
                        Just participanteId ->
                            let
                                repartija =
                                    repartijaPage.repartija

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
                                    ( { model | pendingItemOperation = Just item.id, repartirModalItem = Nothing }
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
                                    ( { model | pendingItemOperation = Just item.id, repartirModalItem = Nothing }
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
                    ( { model | pendingItemOperation = Just item.id, repartirModalItem = Nothing }
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
        ( Success repartijaPage, Success grupo ) ->
            let
                repartija =
                    repartijaPage.repartija
            in
            { title = grupo.nombre ++ ": " ++ repartija.nombre
            , body =
                [ viewParticipantes grupo repartija
                , viewRepartijaItems userId grupo repartija
                , viewRepartirModal model.repartirModalItem
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
                |> List.map .participante
                |> Set.fromList

        lookupItemName : ULID -> String
        lookupItemName itemId =
            repartija.items
                |> List.filter (\item -> item.id == itemId)
                |> List.head
                |> Maybe.map .nombre
                |> Maybe.withDefault "Item desconocido"
    in
    div [ class "d-flex flex-wrap gap-2 my-3" ]
        (grupo.participantes
            |> List.map
                (\participante ->
                    let
                        variantClass =
                            if Set.member participante.id participantesConClaims then
                                "btn-success"

                            else
                                "btn-outline-secondary"

                        claims =
                            repartija.claims
                                |> List.filter (\c -> c.participante == participante.id)

                        menuItems =
                            if List.isEmpty claims then
                                [ Html.li []
                                    [ span [ class "dropdown-item-text text-muted small" ] [ text "Sin asignaciones" ] ]
                                ]

                            else
                                claims
                                    |> List.map
                                        (\claim ->
                                            Html.li []
                                                [ div [ class "dropdown-item-text d-flex justify-content-between gap-3" ]
                                                    [ span [] [ text <| lookupItemName claim.itemId ]
                                                    , span [ class "text-muted small" ]
                                                        [ text
                                                            (claim.cantidad
                                                                |> Maybe.map (\c -> String.fromInt c ++ " unidad")
                                                                |> Maybe.withDefault "Equitativo"
                                                            )
                                                        ]
                                                    ]
                                                ]
                                        )
                    in
                    div [ class "dropdown" ]
                        [ button
                            [ type_ "button"
                            , class ("btn dropdown-toggle " ++ variantClass)
                            , id ("participante-btn-" ++ participante.id)
                            , Attr.attribute "data-bs-toggle" "dropdown"
                            , Attr.attribute "aria-expanded" "false"
                            ]
                            [ text participante.nombre ]
                        , Html.ul [ class "dropdown-menu shadow", style "min-width" "16rem" ] menuItems
                        ]
                )
        )


viewRepartijaItems : Maybe ULID -> GrupoLike g -> Repartija -> Html Msg
viewRepartijaItems userId grupo repartija =
    div [ class "table-responsive" ]
        [ Html.table [ class "table table-striped table-hover align-middle" ]
            [ Html.thead []
                [ Html.tr []
                    [ Html.th [ style "min-width" "12rem" ] [ text "Descripcion" ]
                    , Html.th [ class "text-end", style "min-width" "10rem" ] [ text "Monto total" ]
                    , Html.th [ class "text-end", style "min-width" "8rem" ] [ text "Cantidad" ]
                    , Html.th [ class "text-center", style "min-width" "14rem" ] [ text "Repartido" ]
                    , Html.th [ class "text-end", style "width" "8rem" ] []
                    ]
                ]
            , Html.tbody []
                ((repartija.items
                    |> List.map (\item -> viewClaimsLine userId grupo repartija item)
                 )
                    ++ [ Html.tr []
                            [ Html.td [] [ text "Propina" ]
                            , Html.td [ class "text-end" ] [ text <| "$" ++ Monto.toString repartija.extra ]
                            , Html.td [] []
                            , Html.td [] []
                            , Html.td [] []
                            ]
                       , Html.tr [ class "fw-bold" ]
                            [ Html.td [] [ text "Total" ]
                            , Html.td [ class "text-end" ]
                                [ text "$"
                                , repartija.items
                                    |> List.map .monto
                                    |> List.foldl Monto.add Monto.zero
                                    |> Monto.add repartija.extra
                                    |> Monto.toString
                                    |> text
                                ]
                            , Html.td [] []
                            , Html.td [] []
                            , Html.td [] []
                            ]
                       ]
                )
            ]
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


type CTAState
    = Visible
    | Disabled
    | Hidden


viewClaimsLine : Maybe ULID -> GrupoLike g -> Repartija -> RepartijaItem -> Html Msg
viewClaimsLine userId grupo repartija item =
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
            { plus1 = Hidden, minus1 = Hidden, participe = Hidden, salirse = Hidden, repartir = Hidden }

        buttonsState =
            case ( userId, itemRepartidoState, userClaim ) of
                ( Nothing, _, _ ) ->
                    allHidden

                ( Just _, RepartidoIncorrectamente, Just _ ) ->
                    { allHidden | plus1 = Visible, minus1 = Visible, participe = Visible, salirse = Visible }

                ( Just _, RepartidoIncorrectamente, Nothing ) ->
                    { allHidden | plus1 = Visible, minus1 = Visible, participe = Visible, salirse = Visible }

                ( Just _, RepartidoExactamente _, Just _ ) ->
                    { allHidden | plus1 = Visible, minus1 = Visible }

                ( Just _, RepartidoExactamente _, Nothing ) ->
                    { allHidden | plus1 = Visible, minus1 = Disabled }

                ( Just _, RepartidoEquitativamenteEntre _, Nothing ) ->
                    { allHidden | participe = Visible, salirse = Disabled }

                ( Just _, RepartidoEquitativamenteEntre _, Just _ ) ->
                    { allHidden | participe = Disabled, salirse = Visible }

                ( Just _, SinRepartir, _ ) ->
                    { allHidden | repartir = Visible }

        actionButton variant label msg state =
            case state of
                Hidden ->
                    []

                _ ->
                    [ button
                        [ type_ "button"
                        , class "btn"
                        , if state == Disabled then
                            class <| "btn-outline-" ++ variant

                          else
                            class <| "btn-" ++ variant
                        , disabled (state == Disabled)
                        , onClick msg
                        ]
                        [ text label ]
                    ]
    in
    Html.tr []
        [ Html.td [] [ text item.nombre ]
        , Html.td [ class "text-end" ] [ text <| "$" ++ Monto.toString item.monto ]
        , Html.td [ class "text-end" ] [ text <| String.fromInt item.cantidad ]
        , Html.td [ class "text-center" ]
            [ viewClaimProgressAndDropdown grupo claimsForItem itemRepartidoState
            ]
        , Html.td [ class "text-end" ]
            [ div [ class "d-inline-flex align-items-center gap-1" ]
                (List.concat
                    [ actionButton "primary" "Repartir" (OpenRepartirModal item) buttonsState.repartir
                    , actionButton "primary" "+1" (ChangeCurrentClaim item 1) buttonsState.plus1
                    , actionButton "danger" "-1" (ChangeCurrentClaim item -1) buttonsState.minus1
                    , actionButton "primary" "Participé" (JoinCurrentClaim item) buttonsState.participe
                    , actionButton "danger"
                        "Salirse"
                        (case userClaim of
                            Just claim ->
                                LeaveCurrentClaim claim

                            Nothing ->
                                NoOp
                        )
                        buttonsState.salirse
                    ]
                )
            ]
        ]


viewRepartirModal : Maybe RepartijaItem -> Html Msg
viewRepartirModal maybeItem =
    Bs.modal
        { isOpen = maybeItem /= Nothing
        , onClose = CloseRepartirModal
        , title = "Elegí cómo repartir"
        , body =
            case maybeItem of
                Just item ->
                    [ div [ class "list-group" ]
                        [ button
                            [ type_ "button"
                            , class "list-group-item list-group-item-action d-flex align-items-start gap-2"
                            , onClick (ChangeCurrentClaim item 1)
                            ]
                            [ i [ class "bi bi-plus-lg mt-1" ] []
                            , div []
                                [ div [ class "fw-semibold" ] [ text "Por cantidad" ]
                                , div [ class "small text-muted" ] [ text "Indicás cuántas unidades consumió cada persona" ]
                                ]
                            ]
                        , button
                            [ type_ "button"
                            , class "list-group-item list-group-item-action d-flex align-items-start gap-2"
                            , onClick (JoinCurrentClaim item)
                            ]
                            [ i [ class "bi bi-check-lg mt-1" ] []
                            , div []
                                [ div [ class "fw-semibold" ] [ text "Equitativo" ]
                                , div [ class "small text-muted" ] [ text "Se divide en partes iguales entre los que participaron" ]
                                ]
                            ]
                        ]
                    ]

                Nothing ->
                    []
        , footer =
            [ Bs.btn Bs.Transparent [ onClick CloseRepartirModal ] [ text "Cancelar" ] ]
        }


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


viewClaimProgressAndDropdown : GrupoLike g -> List RepartijaClaim -> ItemRepartidoState -> Html Msg
viewClaimProgressAndDropdown grupo claimsForItem itemRepartidoState =
    div [ class "dropdown d-inline-block" ]
        [ viewRepartidoState itemRepartidoState
        , Html.ul [ class "dropdown-menu shadow" ]
            (if List.isEmpty claimsForItem then
                [ Html.li [] [ span [ class "dropdown-item-text text-muted small" ] [ text "Sin reparto" ] ] ]

             else
                claimsForItem
                    |> List.map
                        (\claim ->
                            Html.li []
                                [ div [ class "dropdown-item-text d-flex justify-content-between gap-3" ]
                                    [ span [] [ text (lookupNombreParticipante grupo claim.participante) ]
                                    , span [ class "text-muted small" ]
                                        [ text
                                            (case claim.cantidad of
                                                Just cantidad ->
                                                    String.fromInt cantidad ++ " unidad"

                                                Nothing ->
                                                    "Equitativo"
                                            )
                                        ]
                                    ]
                                ]
                        )
            )
        ]


viewRepartidoState : ItemRepartidoState -> Html Msg
viewRepartidoState itemRepartidoState =
    let
        variantClass : String
        variantClass =
            case itemRepartidoState of
                SinRepartir ->
                    "btn-outline-secondary"

                RepartidoIncorrectamente ->
                    "btn-danger"

                RepartidoExactamente { deltaDeCantidad } ->
                    if deltaDeCantidad == 0 then
                        "btn-success"

                    else
                        "btn-warning"

                RepartidoEquitativamenteEntre { cantidadDeParticipantes } ->
                    if cantidadDeParticipantes >= 2 then
                        "btn-success"

                    else
                        "btn-warning"
    in
    button
        [ type_ "button"
        , class ("btn btn-sm " ++ variantClass)
        , Attr.attribute "data-bs-toggle" "dropdown"
        , Attr.attribute "aria-expanded" "false"
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
