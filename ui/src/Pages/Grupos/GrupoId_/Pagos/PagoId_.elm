module Pages.Grupos.GrupoId_.Pagos.PagoId_ exposing (Model, Msg, page)

import Components.BarrasDeNetos exposing (viewNetosBarras)
import Components.NavBar as NavBar
import Effect exposing (Effect)
import FeatherIcons as Icons
import Form exposing (Form)
import Form.Error as FormError
import Form.Field as FormField
import Form.Init as Form
import Form.Input as FormInput
import Form.Validate as V exposing (Validation, nonEmpty)
import Generated.Api as Api exposing (Distribucion, Grupo, Monto, Netos, Pago, Parte(..), Participante, ParticipanteId, Repartija, RepartijaItem, ShallowGrupo, ULID)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onSubmit)
import Http
import Json.Encode
import Layouts
import List.Extra as List
import Models.Grupo exposing (lookupNombreParticipante, lookupParticipantes)
import Models.Monto as Monto
import Models.Pago as Pago
import Models.Store as Store
import Models.Store.Types exposing (Store)
import Page exposing (Page)
import Pages.Grupos.GrupoId_.Pagos.New as P exposing (Model, Msg(..), Section(..), subscriptions, update, validatePago, validatePagoInSection, view, waitAndCheckNecessaryData)
import RemoteData exposing (RemoteData(..), WebData)
import Route exposing (Route)
import Route.Path as Path
import Shared
import Utils.Form exposing (..)
import Utils.Toasts as Toasts exposing (pushToast)
import Utils.Toasts.Types as Toasts exposing (ToastLevel(..))
import Utils.Ulid exposing (emptyUlid)
import View exposing (View)


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
      }
    , Effect.batch
        [ Store.ensureGrupo grupoId store
        , Store.ensurePago pagoId store
        , Effect.getCurrentUser grupoId
        , waitAndCheckNecessaryData
        ]
    )
