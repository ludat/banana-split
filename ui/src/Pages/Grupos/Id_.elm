module Pages.Grupos.Id_ exposing (Model, Msg, page)

import Components.NavBar as NavBar
import Css
import Dict
import Effect exposing (Effect)
import Form exposing (Form)
import Form.Error as FormError
import Form.Field as FormField
import Form.Input as FormInput
import Form.Validate as V exposing (Validation, andMap, andThen, field, int, nonEmpty, string, succeed)
import Generated.Api as Api exposing (Grupo, Monto, Netos, Pago, Parte(..), Participante, ParticipanteAddParams, ParticipanteId, ULID)
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (onClick, onSubmit)
import Layouts
import Numeric.ArithmeticError as DecimalError
import Numeric.Decimal as Decimal
import Numeric.Decimal.Rounding as Decimal
import Numeric.Nat as Nat
import Numeric.Rational as Rational
import Page exposing (Page)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Utils.Form exposing (CustomFormError(..))
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
                                    [ div [ class "fixed-grid" ]
                                        [ div [ class "grid", Css.barras_precio ]
                                            (let
                                                maximo =
                                                    netos.netos
                                                        |> List.map
                                                            (\( _, ( _, numerador, denominador ) ) ->
                                                                Decimal.fromRational Decimal.RoundTowardsZero Nat.nat2 (Rational.ratio numerador denominador)
                                                                    |> Result.withDefault (Decimal.fromInt Decimal.RoundTowardsZero Nat.nat2 0)
                                                                    |> Decimal.abs
                                                            )
                                                        |> List.map Decimal.toFloat
                                                        |> List.maximum
                                                        |> Maybe.withDefault 0
                                             in
                                             netos.netos
                                                |> List.sortBy
                                                    (\( _, ( _, numerador, denominador ) ) ->
                                                        Decimal.fromRational Decimal.RoundTowardsZero Nat.nat2 (Rational.ratio numerador denominador)
                                                            |> Result.withDefault (Decimal.fromInt Decimal.RoundTowardsZero Nat.nat2 0)
                                                            |> Decimal.toFloat
                                                    )
                                                |> List.reverse
                                                |> List.concatMap
                                                    (\( participanteId, ( _, numerador, denominador ) ) ->
                                                        let
                                                            monto =
                                                                Decimal.fromRational Decimal.RoundTowardsZero Nat.nat2 (Rational.ratio numerador denominador)
                                                                    |> Result.withDefault (Decimal.fromInt Decimal.RoundTowardsZero Nat.nat2 0)

                                                            participante =
                                                                lookupParticipante grupo participanteId

                                                            nombreDerecha =
                                                                div [ class "cell nombre derecha" ] [ text participante.participanteNombre ]

                                                            nombreIzquierda =
                                                                div [ class "cell nombre izquierda" ] [ text participante.participanteNombre ]

                                                            barraIzquierda =
                                                                div [ class "cell monto izquierda" ]
                                                                    [ p
                                                                        []
                                                                        [ text <| Decimal.toString monto ]
                                                                    , div
                                                                        [ style "width" <| String.fromFloat (abs (Decimal.toFloat monto) * 100 / maximo) ++ "%"
                                                                        , class "barra has-background-danger"
                                                                        ]
                                                                        []
                                                                    ]

                                                            barraDerecha =
                                                                div [ class "cell monto derecha" ]
                                                                    [ p
                                                                        []
                                                                        [ text <| Decimal.toString monto ]
                                                                    , div
                                                                        [ style "width" <| String.fromFloat (Decimal.toFloat monto * 100 / maximo) ++ "%"
                                                                        , class "barra has-background-success"
                                                                        ]
                                                                        []
                                                                    ]
                                                        in
                                                        case compare numerador 0 of
                                                            LT ->
                                                                [ barraIzquierda
                                                                , nombreDerecha
                                                                ]

                                                            EQ ->
                                                                [ nombreIzquierda
                                                                , barraDerecha
                                                                ]

                                                            GT ->
                                                                [ nombreIzquierda
                                                                , barraDerecha
                                                                ]
                                                    )
                                            )
                                        ]
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


lookupParticipante : Grupo -> ParticipanteId -> Participante
lookupParticipante grupo participanteId =
    grupo.participantes
        |> List.filter (\p -> p.participanteId == participanteId)
        |> List.head
        |> Maybe.withDefault { participanteId = participanteId, participanteNombre = "Desconocido" }
