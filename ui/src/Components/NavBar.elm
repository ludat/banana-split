module Components.NavBar exposing (NavBarModel, modelFromShared, navBar)

import Generated.Api exposing (Distribucion, Pago, TipoDistribucion(..), ULID)
import Html exposing (Attribute, Html, button, div, i, li, small, span, text, ul)
import Html.Attributes exposing (class, classList, type_)
import Html.Events as Events
import Json.Decode as Decode
import Layouts.Default as Layout exposing (ShouldHideNavbar(..), viewGlobalUserSelector)
import Models.Store as Store
import Models.Store.Types exposing (Store)
import RemoteData exposing (RemoteData(..))
import Route.Path exposing (Path(..))
import Shared.Model as Shared
import Shared.Msg as Shared


type alias NavBarModel =
    { grupoId : ULID
    , userId : Maybe ULID
    }


modelFromShared : Shared.Model -> ULID -> NavBarModel
modelFromShared shared grupoId =
    { grupoId = grupoId, userId = shared.userId }


navBar : NavBarModel -> Store -> Path -> Bool -> Html Layout.Msg
navBar navBarModel store path _ =
    let
        grupoLabel =
            case store |> Store.getGrupo navBarModel.grupoId of
                NotAsked ->
                    "Cargando..."

                Loading ->
                    "Cargando..."

                Failure _ ->
                    "Error"

                Success grupo ->
                    grupo.nombre
    in
    div [ class "d-flex flex-column h-100" ]
        [ div [ class "px-2 pb-2 mb-2 border-bottom" ]
            [ small [ class "text-muted d-block" ] [ text "Grupo" ]
            , span [ class "fw-bold" ] [ text grupoLabel ]
            ]
        , ul [ class "nav nav-pills flex-column" ]
            [ navBarItem
                { currentPath = path
                , path = Grupos_Id_ { id = navBarModel.grupoId }
                , icon = Just "bi-bar-chart"
                , text = "Resumen"
                , attrs = []
                }
            , pagoNavSection navBarModel store path
            , navBarItem
                { currentPath = path
                , path = Grupos_GrupoId__Participantes { grupoId = navBarModel.grupoId }
                , icon = Just "bi-person-gear"
                , text = "Participantes"
                , attrs = []
                }
            , navBarItem
                { currentPath = path
                , path = Grupos_GrupoId__Settings { grupoId = navBarModel.grupoId }
                , icon = Just "bi-gear"
                , text = "Ajustes"
                , attrs = []
                }
            ]
        , case Store.getGrupo navBarModel.grupoId store |> RemoteData.toMaybe of
            Just grupo ->
                div [ class "mt-auto pt-3 border-top" ]
                    [ small [ class "text-muted d-block mb-1 px-2" ] [ text "Ver como:" ]
                    , viewGlobalUserSelector navBarModel.userId grupo
                    ]

            Nothing ->
                text ""
        ]


navLinkAttrs :
    { active : Bool
    , path : Path
    , extra : List (Attribute Layout.Msg)
    }
    -> List (Attribute Layout.Msg)
navLinkAttrs opts =
    [ type_ "button"
    , classList
        [ ( "nav-link text-start w-100 d-flex align-items-center gap-2", True )
        , ( "active", opts.active )
        ]
    , Events.stopPropagationOn "click"
        (Decode.succeed
            ( Layout.ForwardSharedMessage HideNavbarAfterEvent <| Shared.NavigateTo opts.path
            , True
            )
        )
    ]
        ++ opts.extra


iconSpan : Maybe String -> Html msg
iconSpan maybeIcon =
    case maybeIcon of
        Just iconClass ->
            i [ class ("bi " ++ iconClass) ] []

        Nothing ->
            text ""


navBarItem :
    { path : Path
    , currentPath : Path
    , attrs : List (Attribute Layout.Msg)
    , text : String
    , icon : Maybe String
    }
    -> Html Layout.Msg
navBarItem props =
    li [ class "nav-item" ]
        [ button
            (navLinkAttrs
                { active = props.currentPath == props.path
                , path = props.path
                , extra = props.attrs
                }
            )
            [ iconSpan props.icon
            , span [] [ text props.text ]
            ]
        ]


pagoNavSection : NavBarModel -> Store -> Path -> Html Layout.Msg
pagoNavSection navBarModel store path =
    let
        nuevoPagoItem =
            navBarItem
                { currentPath = path
                , path = Grupos_GrupoId__Pagos_New { grupoId = navBarModel.grupoId }
                , icon = Just "bi-plus-lg"
                , text = "Nuevo Pago"
                , attrs = []
                }

        pagoItem pagoPath pagoNombre repartijaChildren =
            li [ class "nav-item" ]
                [ button
                    (navLinkAttrs
                        { active = path == pagoPath
                        , path = pagoPath
                        , extra = []
                        }
                    )
                    [ iconSpan (Just "bi-receipt")
                    , span [] [ text pagoNombre ]
                    ]
                , if List.isEmpty repartijaChildren then
                    text ""

                  else
                    ul [ class "nav nav-pills flex-column ms-3" ] repartijaChildren
                ]

        currentPagoItems =
            case path of
                Grupos_GrupoId__Pagos_PagoId_ params ->
                    case Store.getPago params.pagoId store |> RemoteData.toMaybe of
                        Just pago ->
                            [ pagoItem path pago.nombre (repartijaSubItemsFromPago navBarModel path pago) ]

                        Nothing ->
                            []

                Grupos_GrupoId__Repartijas_RepartijaId_ params ->
                    case Store.getRepartija params.repartijaId store |> RemoteData.toMaybe of
                        Just repartijaForFrontend ->
                            let
                                pagoPath =
                                    Grupos_GrupoId__Pagos_PagoId_ { grupoId = navBarModel.grupoId, pagoId = repartijaForFrontend.pagoId }
                            in
                            [ pagoItem pagoPath
                                repartijaForFrontend.pagoNombre
                                [ navBarSubItem
                                    { path = path
                                    , currentPath = path
                                    , attrs = []
                                    , text = "Repartija"
                                    , icon = Just "bi-list-check"
                                    }
                                ]
                            ]

                        Nothing ->
                            []

                _ ->
                    []
    in
    li [ class "nav-item mt-2" ]
        [ div [ class "px-2 pb-1 text-uppercase small text-muted d-flex align-items-center gap-2" ]
            [ i [ class "bi bi-cash-stack" ] []
            , span [] [ text "Pagos" ]
            ]
        , ul [ class "nav nav-pills flex-column" ] (currentPagoItems ++ [ nuevoPagoItem ])
        ]


repartijaSubItemsFromPago : NavBarModel -> Path -> Pago -> List (Html Layout.Msg)
repartijaSubItemsFromPago navBarModel currentPath pago =
    let
        extractRepartija : Distribucion -> Maybe (Html Layout.Msg)
        extractRepartija dist =
            case dist.tipo of
                TipoDistribucionRepartija repartija ->
                    Just
                        (navBarSubItem
                            { path = Grupos_GrupoId__Repartijas_RepartijaId_ { grupoId = navBarModel.grupoId, repartijaId = repartija.id }
                            , currentPath = currentPath
                            , attrs = []
                            , text = "Repartija"
                            , icon = Just "bi-list-check"
                            }
                        )

                _ ->
                    Nothing
    in
    List.filterMap extractRepartija [ pago.pagadores, pago.deudores ]


navBarSubItem :
    { path : Path
    , currentPath : Path
    , attrs : List (Attribute Layout.Msg)
    , text : String
    , icon : Maybe String
    }
    -> Html Layout.Msg
navBarSubItem props =
    li [ class "nav-item" ]
        [ button
            (navLinkAttrs
                { active = props.currentPath == props.path
                , path = props.path
                , extra = props.attrs
                }
            )
            [ iconSpan props.icon
            , span [] [ text props.text ]
            ]
        ]
