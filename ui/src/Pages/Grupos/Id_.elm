module Pages.Grupos.Id_ exposing (Model, Msg, page)

import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.NavBar as NavBar
import Effect exposing (Effect)
import Generated.Api as Api exposing (Grupo, Netos, Transaccion(..), ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Layouts
import Models.Grupo exposing (lookupNombreParticipante)
import Models.Monto exposing (montoToDecimal)
import Models.Store as Store
import Models.Store.Types as Store exposing (Store)
import Numeric.Decimal as Decimal
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import View exposing (View)


page : Shared.Model -> Route { id : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.id shared.store
        , update = update
        , subscriptions = subscriptions
        , view = view shared.store
        }
        |> Page.withLayout
            (\m ->
                Layouts.Default { navBarContent = Just <| NavBar.navBar route.params.id shared.store route.path }
            )


type alias Model =
    { grupoId : String
    }


init : ULID -> Store -> ( Model, Effect Msg )
init grupoId store =
    ( { grupoId = grupoId }
    , Effect.batch
        [ Store.ensureNetos grupoId store
        , Store.ensureGrupo grupoId store
        ]
    )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Store -> Model -> View Msg
view store model =
    case store |> Store.getGrupo model.grupoId of
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
                                    [ Path.href <| Path.Grupos_GrupoId__Participantes { grupoId = grupo.grupoId }
                                    ]
                                    [ text "acá" ]
                                ]
                            ]

                         else
                            case store |> Store.getNetos model.grupoId of
                                Success netos ->
                                    [ viewNetosBarras grupo netos
                                    , viewTransferencias grupo netos
                                    ]

                                NotAsked ->
                                    [ text "Carganding" ]

                                Loading ->
                                    [ text "Carganding" ]

                                Failure e ->
                                    [ text "Error cargando los netos" ]
                        )
                    , div [ class "column" ] []
                    ]
                ]
            }


viewTransferencias : Grupo -> Netos -> Html Msg
viewTransferencias grupo netos =
    div []
        (netos.transaccionesParaSaldar
            |> List.map
                (\t ->
                    case t of
                        Transaccion from to monto ->
                            div []
                                [ text <| lookupNombreParticipante grupo from
                                , text " -> "
                                , text <| lookupNombreParticipante grupo to
                                , text " "
                                , text <| Decimal.toString <| montoToDecimal monto
                                ]
                )
        )
