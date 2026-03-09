module Components.NavBar exposing (NavBarModel, modelFromShared, navBar)

import Components.Ui5 as Ui5
import Generated.Api exposing (Distribucion, Pago, TipoDistribucion(..), ULID)
import Html exposing (Attribute, Html, text)
import Html.Attributes as Attr exposing (class)
import Html.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode
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
    Ui5.sideNavigation
        [ Ui5.slot "sideContent"
        ]
        [ Ui5.sideNavigationItem
            [ Attr.attribute "text" <|
                case store |> Store.getGrupo navBarModel.grupoId of
                    NotAsked ->
                        "Cargando..."

                    Loading ->
                        "Cargando..."

                    Failure _ ->
                        "Error"

                    Success grupo ->
                        grupo.nombre
            , Attr.attribute "icon" "home"
            , Attr.property "unselectable" (Encode.bool True)
            , Ui5.slot "header"
            ]
            []
        , navBarItem
            { currentPath = path
            , path = Grupos_Id_ { id = navBarModel.grupoId }
            , icon = Just "activity-2"
            , text = "Resumen"
            , attrs = []
            }
            []
        , pagoNavSection navBarModel store path
        , navBarItem
            { currentPath = path
            , path = Grupos_GrupoId__Participantes { grupoId = navBarModel.grupoId }
            , icon = Just "user-edit"
            , text = "Participantes"
            , attrs = []
            }
            []
        , navBarItem
            { currentPath = path
            , path = Grupos_GrupoId__Settings { grupoId = navBarModel.grupoId }
            , icon = Just "action-settings"
            , text = "Ajustes"
            , attrs = []
            }
            []
        , case Store.getGrupo navBarModel.grupoId store |> RemoteData.toMaybe of
            Just grupo ->
                viewGlobalUserSelector navBarModel.userId grupo

            Nothing ->
                text ""
        ]


navBarItem :
    { path : Path
    , currentPath : Path
    , attrs : List (Attribute Layout.Msg)
    , text : String
    , icon : Maybe String
    }
    -> List (Html Layout.Msg)
    -> Html Layout.Msg
navBarItem props children =
    Ui5.sideNavigationItem
        ([ Events.stopPropagationOn "click" (Decode.succeed ( Layout.ForwardSharedMessage HideNavbarAfterEvent <| Shared.NavigateTo props.path, True ))
         , Attr.attribute "text" props.text
         , props.icon |> Maybe.map (Attr.attribute "icon") |> Maybe.withDefault (class "")
         , Attr.property "selected" (Encode.bool <| props.currentPath == props.path)
         ]
            |> List.append props.attrs
        )
        children


pagoNavSection : NavBarModel -> Store -> Path -> Html Layout.Msg
pagoNavSection navBarModel store path =
    let
        nuevoPagoItem =
            navBarItem
                { currentPath = path
                , path = Grupos_GrupoId__Pagos_New { grupoId = navBarModel.grupoId }
                , icon = Just "add"
                , text = "Nuevo Pago"
                , attrs = [ Attr.attribute "design" "Action" ]
                }
                []

        pagoItem pagoPath pagoNombre repartijaChildren =
            navBarItem
                { path = pagoPath
                , currentPath = path
                , attrs = [ Attr.property "expanded" (Encode.bool True) ]
                , text = pagoNombre
                , icon = Just "receipt"
                }
                repartijaChildren

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
                                    , attrs = [ Attr.property "unselectable" (Encode.bool True) ]
                                    , text = "Repartija"
                                    , icon = Just "add-activity-2"
                                    }
                                ]
                            ]

                        Nothing ->
                            []

                _ ->
                    []
    in
    Ui5.sideNavigationGroup
        [ Attr.attribute "text" "Pagos"
        , Attr.attribute "icon" "money-bills"
        , Attr.property "expanded" (Encode.bool True)
        ]
        (currentPagoItems ++ [ nuevoPagoItem ])


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
                            , icon = Just "add-activity-2"
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
    Ui5.sideNavigationSubItem
        ([ Events.stopPropagationOn "click" (Decode.succeed ( Layout.ForwardSharedMessage HideNavbarAfterEvent <| Shared.NavigateTo props.path, True ))
         , Attr.attribute "text" props.text
         , props.icon |> Maybe.map (Attr.attribute "icon") |> Maybe.withDefault (class "")
         , Attr.property "selected" (Encode.bool <| props.currentPath == props.path)
         ]
            |> List.append props.attrs
        )
        []
