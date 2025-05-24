module Pages.Grupos.GrupoId_.Repartijas.RepartijaId_ exposing (Model, Msg, page)

import Components.NavBar as NavBar
import Effect exposing (Effect)
import FeatherIcons
import Form exposing (Form)
import Form.Error as FormError
import Form.Field as Form
import Form.Input as FormInput
import Form.Validate as V
import Generated.Api as Api exposing (Grupo, Participante, ParticipanteId, Repartija, RepartijaClaim, RepartijaItem, ULID)
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
import Set
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
                    { navBarContent = Just <| NavBar.navBar (NavBar.modelFromShared shared route.params.grupoId) shared.store route.path
                    }
            )



-- INIT


type alias Model =
    { grupoId : ULID
    , repartijaId : ULID
    , isClaimModalOpen : Maybe RepartijaItem
    , claimForm : Form CustomFormError RepartijaClaim
    , participanteClaimsModal : Maybe ParticipanteId
    }


init : ULID -> ULID -> Store -> ( Model, Effect Msg )
init grupoId repartijaId store =
    ( { grupoId = grupoId
      , repartijaId = repartijaId
      , isClaimModalOpen = Nothing
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
    | OpenClaimItemModal RepartijaItem
    | OpenDesdoblarItemModal RepartijaItem
    | CloseClaimModal
    | ClaimFormMsg Form.Msg
    | DeleteClaim RepartijaClaim
    | CreateRepartijaResponded (WebData RepartijaClaim)
    | DeleteRepartijaClaimResponded (WebData String)
    | CreatePago
    | CreatePagoResponded (WebData String)
    | OpenParticipanteClaimsPopup ParticipanteId
    | CloseParticipanteClaimsPopup


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

                Form.Input "participante" Form.Select (Form.String participanteId) ->
                    let
                        cantidadAnterior =
                            case ( Store.getRepartija model.repartijaId store, model.isClaimModalOpen ) of
                                ( NotAsked, _ ) ->
                                    ""

                                ( Loading, _ ) ->
                                    ""

                                ( Failure e, _ ) ->
                                    ""

                                ( Success _, Nothing ) ->
                                    ""

                                ( Success repartija, Just item ) ->
                                    repartija.repartijaClaims
                                        |> List.filter (\claim -> claim.repartijaClaimItemId == item.repartijaItemId)
                                        |> interpretClaims
                                        |> (\x ->
                                                case x of
                                                    NoClaims ->
                                                        ""

                                                    MixedClaims repartijaClaims ->
                                                        "error"

                                                    OnlyExactClaims list ->
                                                        list
                                                            |> List.filter (\( n, c ) -> c.repartijaClaimParticipante == participanteId)
                                                            |> List.map Tuple.first
                                                            |> List.sum
                                                            |> String.fromInt

                                                    OnlyParticipationClaims repartijaClaims ->
                                                        ""
                                           )

                        newForm =
                            model.claimForm
                                |> Form.update validateClaim formMsg
                                |> Form.update validateClaim (Form.Input "cantidad" Form.Text (Form.String cantidadAnterior))
                    in
                    ( { model
                        | claimForm = newForm
                      }
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

        OpenParticipanteClaimsPopup participanteId ->
            ( { model | participanteClaimsModal = Just participanteId }
            , Effect.none
            )

        CloseParticipanteClaimsPopup ->
            ( { model | participanteClaimsModal = Nothing }
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
                [ div [ class "is-size-2 has-text-weight-bold" ] [ text repartija.repartijaNombre ]
                , viewParticipantes grupo repartija
                , viewRepartijaItems grupo repartija
                , viewClaimModal model repartija grupo.participantes
                , viewParticipanteClaimsModal model grupo repartija
                , button
                    [ class "button is-primary"
                    , onClick CreatePago
                    ]
                    [ text "Crear Pago" ]
                ]

            ( Failure e, _ ) ->
                [ text "falle" ]

            ( _, Failure _ ) ->
                [ text "falle" ]

            ( _, _ ) ->
                [ text "cargando" ]
    }


viewParticipantes : Grupo -> Repartija -> Html Msg
viewParticipantes grupo repartija =
    let
        participantesConClaims =
            repartija.repartijaClaims
                |> List.map (\claim -> claim.repartijaClaimParticipante)
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


viewRepartijaItems : Grupo -> Repartija -> Html Msg
viewRepartijaItems grupo repartija =
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
        ]
            ++ [ tbody [] <|
                    (repartija.repartijaItems
                        |> List.map
                            (\item -> viewClaimsLine grupo repartija item)
                    )
                        ++ [ tr []
                                [ td [] [ text "Propina" ]
                                , td [ class "has-text-right" ] [ text "$", text <| Decimal.toString <| montoToDecimal repartija.repartijaExtra ]
                                , td [] []
                                , td [] []
                                , td [] []
                                ]
                           ]
               ]


interpretClaims : List RepartijaClaim -> ItemClaimsState
interpretClaims originalClaims =
    let
        folder : RepartijaClaim -> ItemClaimsState -> ItemClaimsState
        folder claim claimsState =
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
    List.foldl folder NoClaims originalClaims


viewClaimsLine : Grupo -> Repartija -> RepartijaItem -> Html Msg
viewClaimsLine grupo repartija item =
    let
        claimsForItem =
            repartija.repartijaClaims
                |> List.filter (\claim -> claim.repartijaClaimItemId == item.repartijaItemId)

        itemsClaimed : ItemClaimsState
        itemsClaimed =
            claimsForItem
                |> interpretClaims
    in
    tr []
        [ td [ class "is-vcentered" ] [ text <| item.repartijaItemNombre ]
        , td [ class "has-text-right is-vcentered" ]
            [ text <| "$" ++ Decimal.toString (montoToDecimal item.repartijaItemMonto)
            ]
        , td [ class "has-text-right is-vcentered" ] [ text <| String.fromInt item.repartijaItemCantidad ]
        , td [] [ viewClaimProgressAndDropdown grupo repartija item claimsForItem itemsClaimed ]
        , td []
            [ div [ class "buttons has-addons" ]
                [ button [ onClick <| OpenClaimItemModal item, class "button is-link" ] [ text "Claim" ]

                --, button [ onClick <| OpenDesdoblarItemModal item, class "button" ] [ text "Desdoblar" ]
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


viewClaimProgressAndDropdown : Grupo -> Repartija -> RepartijaItem -> List RepartijaClaim -> ItemClaimsState -> Html Msg
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
                            item.repartijaItemCantidad - cantidadClaimeado
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
                                            lookupNombreParticipante grupo claim.repartijaClaimParticipante
                                      in
                                      case claim.repartijaClaimCantidad of
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


viewParticipanteClaimsModal : Model -> Grupo -> Repartija -> Html Msg
viewParticipanteClaimsModal model grupo repartija =
    case model.participanteClaimsModal of
        Just participanteId ->
            let
                claims =
                    repartija.repartijaClaims
                        |> List.filter (\c -> c.repartijaClaimParticipante == participanteId)

                participanteNombre =
                    lookupNombreParticipante grupo participanteId

                lookupItemName : ULID -> String
                lookupItemName itemId =
                    repartija.repartijaItems
                        |> List.filter (\item -> item.repartijaItemId == itemId)
                        |> List.head
                        |> Maybe.map .repartijaItemNombre
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
                                                    [ td [] [ text <| lookupItemName claim.repartijaClaimItemId ]
                                                    , td []
                                                        [ text
                                                            (claim.repartijaClaimCantidad
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


viewClaimModal : Model -> Repartija -> List Participante -> Html Msg
viewClaimModal model repartija participantes =
    case model.isClaimModalOpen of
        Just item ->
            let
                claims =
                    repartija.repartijaClaims
                        |> List.filter (\c -> c.repartijaClaimItemId == item.repartijaItemId)
                        |> interpretClaims
            in
            div
                [ class "modal is-active" ]
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
                        [ repartijaClaimForm participantes claims model.claimForm
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
                                [ text "Reclamar item" ]
                            , button
                                [ class "button"
                                , onClick CloseClaimModal
                                ]
                                [ text "Cancel" ]
                            ]
                        ]
                    ]
                ]

        Nothing ->
            div [] []


repartijaClaimForm : List Participante -> ItemClaimsState -> Form CustomFormError RepartijaClaim -> Html Msg
repartijaClaimForm participantes claims form =
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

        shouldShowCantidad =
            case claims of
                MixedClaims _ ->
                    False

                OnlyExactClaims _ ->
                    True

                OnlyParticipationClaims _ ->
                    False

                NoClaims ->
                    True

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
        , div
            [ class "field"
            , if shouldShowCantidad then
                class ""

              else
                class "is-hidden"
            ]
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
