module Components.NavBar exposing (NavBarModel, modelFromShared, navBar)

import Components.Ui5 as Ui5
import Generated.Api exposing (ULID)
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
        , navBarItem
            { currentPath = path
            , path = Grupos_GrupoId__Pagos { grupoId = navBarModel.grupoId }
            , icon = Just "money-bills"
            , text = "Pagos"
            , attrs = []
            }
            [ navBarSubItem
                { currentPath = path
                , path = Grupos_GrupoId__Pagos_New { grupoId = navBarModel.grupoId }
                , icon = Just "add"
                , text = "Nuevo Pago"
                , attrs = [ Attr.attribute "design" "Action" ]
                }
            ]
        , navBarItem
            { currentPath = path
            , path = Grupos_GrupoId__Participantes { grupoId = navBarModel.grupoId }
            , icon = Just "user-edit"
            , text = "Participantes"
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
