module Components.NavBar exposing (..)

import Generated.Api exposing (Grupo, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Models.Store as Store
import Models.Store.Types exposing (Store)
import RemoteData exposing (RemoteData(..), WebData)
import Route.Path as Route


navBar : ULID -> Store -> Route.Path -> Bool -> Html msg
navBar grupoId store path navBarOpen =
    div
        [ classList [ ( "is-active", navBarOpen ) ]
        , class "navbar-menu"
        ]
        [ div [ class "navbar-start" ]
            [ navBarItem { currentPath = path, path = Route.Grupos_Id_ { id = grupoId }, attrs = [] }
                [ case store |> Store.getGrupo grupoId of
                    NotAsked ->
                        text ""

                    Loading ->
                        text "Cargando..."

                    Failure _ ->
                        text ""

                    Success grupo ->
                        text <| grupo.grupoNombre
                ]
            , navBarItem { currentPath = path, path = Route.Grupos_Id__Pagos { id = grupoId }, attrs = [] }
                [ text "Pagos" ]
            , navBarItem { currentPath = path, path = Route.Grupos_Id__Participantes { id = grupoId }, attrs = [] }
                [ text "Participantes" ]
            ]

        --, div [ class "navbar-end" ]
        --    [ div [ class "navbar-item" ]
        --        [ div [ class "buttons" ]
        --            [ a [ class "button is-primary" ] [ strong [] [ text "Sign up" ] ]
        --            , a [ class "button is-light" ] [ text "Log in" ]
        --            ]
        --        ]
        --    ]
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
