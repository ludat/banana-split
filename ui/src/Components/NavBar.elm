module Components.NavBar exposing (NavBarModel, modelFromShared, navBar, viewGlobalUserSelector)

import Components.Ui5 as Ui5
import Generated.Api exposing (ULID)
import Html exposing (Attribute, Html, text)
import Html.Attributes as Attr exposing (class, selected, value)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Models.Grupo exposing (GrupoLike)
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


navBar : NavBarModel -> Store -> Route.Path -> Bool -> Html Shared.Msg
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


viewGlobalUserSelector : Maybe ULID -> GrupoLike r -> Html Shared.Msg
viewGlobalUserSelector activeUser grupo =
    Ui5.select
        [ on "change"
            (Decode.at [ "detail", "selectedOption", "value" ] Decode.string
                |> Decode.map (\userId -> Shared.SetCurrentUser { grupoId = grupo.id, userId = userId })
            )
        , Ui5.slot "fixedItems"
        ]
        (Ui5.option [ selected (activeUser == Nothing), value "" ] [ text "" ]
            :: (grupo.participantes
                    |> List.map
                        (\participante ->
                            Ui5.option
                                [ selected (activeUser == Just participante.participanteId)
                                , value participante.participanteId
                                ]
                                [ text participante.participanteNombre ]
                        )
               )
        )


navBarItem :
    { path : Route.Path
    , currentPath : Route.Path
    , attrs : List (Attribute Shared.Msg)
    , icon : Maybe String
    }
    -> String
    -> Html Shared.Msg
navBarItem props title =
    Ui5.sideNavigationItem
        [ onClick <| Shared.NavigateTo props.path
        , Attr.attribute "text" title
        , props.icon |> Maybe.map (Attr.attribute "icon") |> Maybe.withDefault (class "")
        , Attr.property "selected" (Encode.bool <| props.currentPath == props.path)
        ]
        []



-- a
--     ([ Route.href props.path
--      , style "text-decoration" "none"
--      , style "color" "var(--sapLinkColor)"
--      , style "padding" "0 0.75rem"
--      , style "display" "flex"
--      , style "align-items" "center"
--      , if props.path == props.currentPath then
--         style "font-weight" "bold"
--        else
--         class ""
--      ]
--         ++ props.attrs
--     )
