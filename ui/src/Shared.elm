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

import Effect exposing (Effect, incoming)
import Json.Decode
import Json.Encode
import Models.Store as Store
import Route exposing (Route)
import Shared.Model
import Shared.Msg exposing (Msg(..))
import Utils.Toasts as Toast



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
    ( { toasties = Toast.initialState
      , store = Store.empty
      , userId = Nothing
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

        ToastMsg toastMsg ->
            let
                ( newModel, cmd ) =
                    Toast.update Toast.config ToastMsg toastMsg model
            in
            ( newModel, Effect.sendCmd cmd )

        AddToast toast ->
            let
                ( newModel, cmd ) =
                    Toast.addToast Toast.config ToastMsg toast ( model, Cmd.none )
            in
            ( newModel, Effect.sendCmd cmd )

        StoreMsg storeMsg ->
            let
                ( store, cmd ) =
                    Store.update storeMsg model.store
            in
            ( { model | store = store }, cmd )

        SetCurrentUser { grupoId, userId } ->
            let
                newUserId =
                    if userId == "" then
                        Nothing
                    else
                        Just userId
            in
            ( { model | userId = newUserId }
            , case newUserId of
                Just uid ->
                    Effect.saveCurrentUser grupoId uid

                Nothing ->
                    Effect.clearCurrentUser grupoId
            )

        CurrentUserLoaded { grupoId, userId } ->
            ( { model
                | userId = userId
              }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    incoming (decodeIncomingPortMessage >> Maybe.withDefault NoOp)


decodeIncomingPortMessage : { tag : String, data : Json.Encode.Value } -> Maybe Msg
decodeIncomingPortMessage { tag, data } =
    case tag of
        "CURRENT_USER_LOADED" ->
            Json.Decode.decodeValue
                (Json.Decode.map2
                    (\grupoId userId ->
                        CurrentUserLoaded
                            { grupoId = grupoId
                            , userId = userId
                            }
                    )
                    (Json.Decode.field "grupoId" Json.Decode.string)
                    (Json.Decode.field "userId" (Json.Decode.nullable Json.Decode.string))
                )
                data
                |> Result.toMaybe

        _ ->
            Nothing
