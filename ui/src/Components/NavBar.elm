module Components.NavBar exposing (..)

import Generated.Api exposing (Grupo, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
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
                        text <| grupo.grupoNombre
                ]
            , navBarItem { currentPath = path, path = Route.Grupos_GrupoId__Pagos { grupoId = navBarModel.grupoId }, attrs = [] }
                [ text "Pagos" ]
            , navBarItem { currentPath = path, path = Route.Grupos_GrupoId__Participantes { grupoId = navBarModel.grupoId }, attrs = [] }
                [ text "Participantes" ]
            , navBarItem { currentPath = path, path = Route.Grupos_GrupoId__Repartijas { grupoId = navBarModel.grupoId }, attrs = [] }
                [ text "Repartijas" ]
            ]
        , div [ class "navbar-end" ]
            [ div [ class "navbar-item" ]
                [ strong []
                    [ case ( Store.getGrupo navBarModel.grupoId store |> RemoteData.toMaybe, navBarModel.userId ) of
                        ( Just grupo, Just activeUser ) ->
                            div [ class "select" ]
                                [ select [ value activeUser, onInput Shared.SetCurrentUser ]
                                    (grupo.participantes
                                        |> List.map
                                            (\participante ->
                                                option
                                                    [ value participante.participanteId
                                                    ]
                                                    [ text participante.participanteNombre ]
                                            )
                                    )
                                ]

                        ( Just grupo, Nothing ) ->
                            div [ class "select" ]
                                [ select [ onInput Shared.SetCurrentUser ]
                                    ([ option [] [ text "" ] ]
                                        ++ (grupo.participantes
                                                |> List.map
                                                    (\participante ->
                                                        option
                                                            [ value participante.participanteId
                                                            ]
                                                            [ text participante.participanteNombre ]
                                                    )
                                           )
                                    )
                                ]

                        ( Nothing, Just userId ) ->
                            text ""

                        ( Nothing, Nothing ) ->
                            text ""
                    ]
                ]
            ]
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
