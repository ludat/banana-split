module Pages.Grupos.Id_ exposing (Model, Msg, page)

import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.NavBar as NavBar
import Css
import Effect exposing (Effect)
import Generated.Api as Api exposing (Grupo, Netos, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Layouts
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import View exposing (View)


page : Shared.Model -> Route { id : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.id
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout
            (\m ->
                Layouts.Default { navBarContent = Just <| NavBar.navBar route.params.id m.grupo }
            )


type alias Model =
    { grupo : WebData Grupo
    , netos : WebData Netos
    }


init : ULID -> ( Model, Effect Msg )
init grupoId =
    ( { grupo = Loading
      , netos = Loading
      }
    , Effect.batch
        [ Effect.sendCmd <| Api.getGrupoById grupoId (RemoteData.fromResult >> GrupoResponse)
        , Effect.sendCmd <| Api.getGrupoByIdNetos grupoId (RemoteData.fromResult >> NetosResponse)
        ]
    )


type Msg
    = NoOp
    | GrupoResponse (WebData Grupo)
    | NetosResponse (WebData Netos)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )

        GrupoResponse webData ->
            ( { model | grupo = webData }
            , Effect.none
            )

        NetosResponse webData ->
            ( { model | netos = webData }
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> View Msg
view model =
    case model.grupo of
        NotAsked ->
            { title = "Impossible"
            , body = []
            }

        Loading ->
            { title = "Cargando"
            , body =
                [ div [ class "container" ]
                    [ section [ class "section" ]
                        [ text "Cargando..."
                        ]
                    ]
                ]
            }

        Failure e ->
            { title = "Fallo"
            , body = []
            }

        Success grupo ->
            { title = grupo.grupoNombre
            , body =
                [ div [ class "columns" ]
                    [ div [ class "column" ]
                        (if grupo.participantes == [] then
                            [ p [] [ text "Tu grupo todavía no tiene participantes!" ]
                            , p []
                                [ text "Agregalos "
                                , a
                                    [ Path.href <| Path.Grupos_Id__Participantes { id = grupo.grupoId }
                                    ]
                                    [ text "acá" ]
                                ]
                            ]

                         else
                            case model.netos of
                                Success netos ->
                                    [ viewNetosBarras grupo netos
                                    ]

                                NotAsked ->
                                    [ text "jajan't" ]

                                Loading ->
                                    [ text "jajan't" ]

                                Failure e ->
                                    [ text "jajan't:" ]
                        )
                    , div [ class "column" ] []
                    ]
                ]
            }
