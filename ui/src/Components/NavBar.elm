module Components.NavBar exposing (..)

import Generated.Api exposing (Grupo, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onInput)
import Json.Decode
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
        [ style "display" "flex"
        , style "align-items" "center"
        , style "gap" "0.5rem"
        , style "width" "100%"
        ]
        [ navBarItem { currentPath = path, path = Route.Home_, attrs = [ attribute "slot" "startContent" ] }
            [ text "🍌 Banana Split" ]
        , navBarItem { currentPath = path, path = Route.Grupos_Id_ { id = navBarModel.grupoId }, attrs = [] }
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
        , div [ style "margin-left" "auto" ]
            [ case Store.getGrupo navBarModel.grupoId store |> RemoteData.toMaybe of
                Just grupo ->
                    viewGlobalUserSelector navBarModel.userId grupo

                Nothing ->
                    text ""
            ]
        ]


viewGlobalUserSelector : Maybe ULID -> GrupoLike r -> Html Shared.Msg
viewGlobalUserSelector activeUser grupo =
    Html.node "ui5-select"
        [ on "change"
            (Json.Decode.at [ "detail", "selectedOption", "value" ] Json.Decode.string
                |> Json.Decode.map (\userId -> Shared.SetCurrentUser { grupoId = grupo.id, userId = userId })
            )
        ]
        (Html.node "ui5-option" [ selected (activeUser == Nothing), value "" ] [ text "" ]
            :: (grupo.participantes
                    |> List.map
                        (\participante ->
                            Html.node "ui5-option"
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
    , attrs : List (Attribute msg)
    }
    -> List (Html msg)
    -> Html msg
navBarItem props =
    a
        ([ Route.href props.path
         , style "text-decoration" "none"
         , style "color" "var(--sapLinkColor)"
         , style "padding" "0 0.75rem"
         , style "display" "flex"
         , style "align-items" "center"
         , if props.path == props.currentPath then
            style "font-weight" "bold"

           else
            class ""
         ]
            ++ props.attrs
        )
