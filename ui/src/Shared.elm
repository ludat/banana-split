module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Effect exposing (Effect)
import Json.Decode
import Models.Store as Store
import Route exposing (Route)
import Shared.Model
import Shared.Msg exposing (Msg(..))
import Toasty



-- FLAGS


type alias Flags =
    {}


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.succeed {}



-- INIT


type alias Model =
    Shared.Model.Model


updateWithCmd : (( m, Cmd msg ) -> ( m, Cmd msg )) -> ( m, Effect msg ) -> ( m, Effect msg )
updateWithCmd f ( oldModel, oldEffects ) =
    let
        ( newModel, newCmd ) =
            f ( oldModel, Cmd.none )
    in
    ( newModel
    , Effect.batch
        [ oldEffects
        , Effect.sendCmd newCmd
        ]
    )


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    ( { toasties = Toasty.initialState
      , store = Store.empty
      }
    , Effect.none
    )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        Shared.Msg.NoOp ->
            ( model
            , Effect.none
            )

        ToastyMsg toastyMsg ->
            let
                ( newModel, cmd ) =
                    Toasty.update Toasty.config ToastyMsg toastyMsg model
            in
            ( newModel, Effect.sendCmd cmd )

        AddToast toast ->
            let
                ( newModel, cmd ) =
                    Toasty.addToast Toasty.config ToastyMsg toast ( model, Cmd.none )
            in
            ( newModel, Effect.sendCmd cmd )

        StoreMsg storeMsg ->
            let
                ( store, cmd ) =
                    Store.update storeMsg model.store
            in
            ( { model | store = store }, cmd )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
