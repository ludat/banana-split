module Components.NavBar exposing (..)

import Generated.Api exposing (Grupo, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import RemoteData exposing (RemoteData(..), WebData)
import Route.Path as Route


navBar : ULID -> WebData Grupo -> Bool -> Html msg
navBar grupoId remoteGrupo navBarOpen =
    div
        [ classList [ ( "is-active", navBarOpen ) ]
        , class "navbar-menu"
        ]
        [ div [ class "navbar-start" ]
            [ a
                [ class "navbar-item"
                , Route.href <| Route.Grupos_Id_ { id = grupoId }
                ]
                [ case remoteGrupo of
                    NotAsked ->
                        text ""

                    Loading ->
                        text "Cargando..."

                    Failure _ ->
                        text ""

                    Success grupo ->
                        text <| grupo.grupoNombre
                ]
            , a
                [ class "navbar-item"
                , Route.href <| Route.Grupos_Id__Pagos { id = grupoId }
                ]
                [ text "Pagos" ]
            , a
                [ class "navbar-item"
                , Route.href <| Route.Grupos_Id__Participantes { id = grupoId }
                ]
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
