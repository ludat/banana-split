module Components.NavBar exposing (NavBarModel, modelFromShared, navBar)

import Components.Ui5 as Ui5
import Generated.Api exposing (ULID)
import Html exposing (Attribute, Html, text)
import Html.Attributes as Attr exposing (class)
import Html.Events exposing (onClick)
import Json.Encode as Encode
import Layouts.Default as Layout exposing (ShouldHideNavbar(..), viewGlobalUserSelector)
import Models.Store as Store
import Models.Store.Types exposing (Store)
import RemoteData exposing (RemoteData(..))
import Route.Path as Route
import Shared.Model as Shared
import Shared.Msg as Shared


type alias NavBarModel =
    { grupoId : ULID
    , userId : Maybe ULID
    }


modelFromShared : Shared.Model -> ULID -> NavBarModel
modelFromShared shared grupoId =
    { grupoId = grupoId, userId = shared.userId }


navBar : NavBarModel -> Store -> Route.Path -> Bool -> Html Layout.Msg
navBar navBarModel store path _ =
    Ui5.sideNavigation
        [ Ui5.slot "sideContent"
        ]
        [ navBarItem { currentPath = path, path = Route.Grupos_Id_ { id = navBarModel.grupoId }, icon = Just "home", attrs = [] } <|
            case store |> Store.getGrupo navBarModel.grupoId of
                NotAsked ->
                    ""

                Loading ->
                    "Cargando..."

                Failure _ ->
                    ""

                Success grupo ->
                    grupo.nombre
        , navBarItem
            { currentPath = path
            , path = Route.Grupos_GrupoId__Pagos { grupoId = navBarModel.grupoId }
            , icon = Just "money-bills"
            , attrs = []
            }
            "Pagos"
        , navBarItem
            { currentPath = path
            , path = Route.Grupos_GrupoId__Participantes { grupoId = navBarModel.grupoId }
            , icon = Just "user-edit"
            , attrs = []
            }
            "Participantes"
        , case Store.getGrupo navBarModel.grupoId store |> RemoteData.toMaybe of
            Just grupo ->
                viewGlobalUserSelector navBarModel.userId grupo

            Nothing ->
                text ""
        ]


navBarItem :
    { path : Route.Path
    , currentPath : Route.Path
    , attrs : List (Attribute Layout.Msg)
    , icon : Maybe String
    }
    -> String
    -> Html Layout.Msg
navBarItem props title =
    Ui5.sideNavigationItem
        [ onClick <| Layout.ForwardSharedMessage HideNavbarAfterEvent <| Shared.NavigateTo props.path
        , Attr.attribute "text" title
        , props.icon |> Maybe.map (Attr.attribute "icon") |> Maybe.withDefault (class "")
        , Attr.property "selected" (Encode.bool <| props.currentPath == props.path)
        ]
        []
