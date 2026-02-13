module Pages.Grupos.GrupoId_.Pagos.PagoId_ exposing (Model, Msg, page)

import Components.NavBar as NavBar
import Effect exposing (Effect)
import Form
import Generated.Api exposing (Parte(..), ULID)
import Layouts
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import Pages.Grupos.GrupoId_.Pagos.New as P exposing (Model, Msg(..), Section(..), andThenSendWarningOnExit, subscriptions, update, validatePago, validatePagoInSection, view, waitAndCheckNecessaryData)
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared


type alias Model =
    P.Model


type alias Msg =
    P.Msg


page : Shared.Model -> Route { grupoId : String, pagoId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = \() -> init route.params.grupoId route.params.pagoId shared.store
        , update = update shared.store shared.userId
        , subscriptions = subscriptions
        , view =
            \m ->
                let
                    result =
                        view shared.store m
                in
                { result
                    | title =
                        case
                            ( Store.getGrupo m.grupoId shared.store |> RemoteData.toMaybe
                            , m.currentPagoId |> Maybe.andThen (\pagoId -> Store.getPago pagoId shared.store |> RemoteData.toMaybe)
                            )
                        of
                            ( Just grupo, Just pago ) ->
                                grupo.nombre ++ ": " ++ pago.nombre

                            ( _, _ ) ->
                                "Cargando"
                }
        }
        |> Page.withLayout
            (\m ->
                Layouts.Default
                    { navBarContent = Just <| NavBar.navBar (NavBar.modelFromShared shared route.params.grupoId) shared.store route.path
                    , grupo = Store.getGrupo m.grupoId shared.store
                    }
            )



-- INIT


init : ULID -> ULID -> Store -> ( Model, Effect Msg )
init grupoId pagoId store =
    ( { grupoId = grupoId
      , currentPagoId = Just pagoId
      , currentSection = PagoConfirmation
      , pagoBasicoForm = Form.initial [] (validatePagoInSection BasicPagoData [])
      , deudoresForm = Form.initial [] (validatePagoInSection DeudoresSection [])
      , resumenDeudores = NotAsked
      , pagadoresForm = Form.initial [] (validatePagoInSection PagadoresSection [])
      , resumenPagadores = NotAsked
      , pagoForm = Form.initial [] (validatePago [])
      , resumenPago = NotAsked
      , receiptParseState = Nothing
      , storedClaims = Nothing
      , hasUnsavedChanges = False
      }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Store.ensurePago pagoId store
        , Effect.getCurrentUser grupoId
        , waitAndCheckNecessaryData
        ]
    )
        |> andThenSendWarningOnExit
