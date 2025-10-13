module Components.NavBar exposing (..)

import Generated.Api exposing (Grupo, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Models.Grupo exposing (GrupoLike)
import Models.Store as Store
import Models.Store.Types exposing (Store)
import RemoteData exposing (RemoteData(..), WebData)
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
navBar navBarModel store path navBarOpen =
    div
        [ classList [ ( "is-active", navBarOpen ) ]
        , class "navbar-menu"
        ]
        [ div [ class "navbar-start" ]
            [ navBarItem { currentPath = path, path = Route.Grupos_Id_ { id = navBarModel.grupoId }, attrs = [] }
                [ case store |> Store.getGrupo navBarModel.grupoId of
                    NotAsked ->
                        text ""

                    Loading ->
                        text "Cargando..."

                    Failure _ ->
                        text ""

                    Success grupo ->
                        text <| grupo.nombre
                ]
            , navBarItem { currentPath = path, path = Route.Grupos_GrupoId__Pagos { grupoId = navBarModel.grupoId }, attrs = [] }
                [ text "Pagos" ]
            , navBarItem { currentPath = path, path = Route.Grupos_GrupoId__Participantes { grupoId = navBarModel.grupoId }, attrs = [] }
                [ text "Participantes" ]
            ]
        , div [ class "navbar-end" ]
            [ div [ class "navbar-item" ]
                [ strong []
                    [ case Store.getGrupo navBarModel.grupoId store |> RemoteData.toMaybe of
                        Just grupo ->
                            viewGlobalUserSelector navBarModel.userId grupo

                        Nothing ->
                            text ""
                    ]
                ]
            ]
        ]


viewGlobalUserSelector : Maybe ULID -> GrupoLike r -> Html Shared.Msg
viewGlobalUserSelector activeUser grupo =
    div [ class "select" ]
        [ select
            [ onInput (\userId -> Shared.SetCurrentUser { grupoId = grupo.id, userId = userId }) ]
            ([ option [ selected (activeUser == Nothing) ] [ text "" ] ]
                ++ (grupo.participantes
                        |> List.map
                            (\participante ->
                                option [ selected (activeUser == Just participante.participanteId), value participante.participanteId ]
                                    [ text participante.participanteNombre ]
                            )
                   )
            )
        ]


navBarItem :
    { path : Route.Path
    , currentPath : Route.Path
    , attrs : List (Attribute msg)
    }
    -> List (Html msg)
    -> Html msg
navBarItem props =
    a
        ([ class "navbar-item is-tab"
         , if props.path == props.currentPath then
            class "is-active"

           else
            class ""
         , Route.href props.path
         ]
            ++ props.attrs
        )
