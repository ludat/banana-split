module Utils.Toasts exposing (..)

import Effect exposing (Effect)
import Shared.Model
import Shared.Msg exposing (Msg(..))
import Toasty


config =
    Toasty.config


addToast :
    String
    -> ( { m | toasties : Toasty.Stack String }, Effect Shared.Msg.Msg )
    -> ( { m | toasties : Toasty.Stack String }, Effect Shared.Msg.Msg )
addToast toast ( oldModel, oldEffect ) =
    let
        ( newModel, cmd ) =
            Toasty.addToast config ToastyMsg toast ( oldModel, Cmd.none )
    in
    ( newModel, Effect.sendCmd cmd )
