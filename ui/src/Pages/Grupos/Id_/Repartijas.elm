module Pages.Grupos.Id_.Repartijas exposing (Model, Msg, page)

import Components.NavBar as NavBar
import Effect exposing (Effect)
import Form exposing (Form)
import Form.Validate exposing (Validation, andMap, andThen, field, nonEmpty, string, succeed)
import Generated.Api exposing (ULID)
import Html exposing (text)
import Layouts
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route { id : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init shared.store route.params.id
        , update = update shared.store
        , subscriptions = subscriptions
        , view = view shared.store
        }
        |> Page.withLayout
            (\m ->
                Layouts.Default
                    { navBarContent = Just <| NavBar.navBar route.params.id shared.store route.path
                    }
            )



-- INIT


type RepartijaCreateParams
    = RepartijaCreateParams String


type alias Model =
    { grupoId : ULID, repartijaForm : Form () RepartijaCreateParams }


init : Store -> ULID -> ( Model, Effect Msg )
init store grupoId =
    ( { grupoId = grupoId
      , repartijaForm = Form.initial [] validateRepartija
      }
    , Store.ensureGrupo grupoId store
    )


validateRepartija : Validation () RepartijaCreateParams
validateRepartija =
    succeed RepartijaCreateParams
        |> andMap (field "nombre" (string |> andThen nonEmpty))



-- UPDATE


type Msg
    = NoOp


update : Store -> Msg -> Model -> ( Model, Effect Msg )
update store msg model =
    case msg of
        NoOp ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Store -> Model -> View Msg
view store model =
    case Nothing of
        Just _ ->
            { title = "Pages.Grupos.Id_.Repartijas"
            , body = [ text "/grupos/:id/repartijas" ]
            }

        Nothing ->
            { title = "Repartijas"
            , body = [ text "/grupos/:id/repartijas" ]
            }
