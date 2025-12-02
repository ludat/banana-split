module Utils.DatePicker exposing
    ( DatePicker
    , Msg
    , init
    , update
    , view
    , getSelectedDate
    , setSelectedDate
    )

{-| A helper module for integrating mercurymedia/elm-datetime-picker with forms.

This module provides a simple wrapper around the SingleDatePicker that makes it
easy to use in forms throughout the application.

# Types
@docs DatePicker, Msg

# Initialization
@docs init

# Update
@docs update

# View
@docs view

# Helpers
@docs getSelectedDate, setSelectedDate
-}

import DatePicker.SingleDatePicker as SingleDatePicker
import Html exposing (Html)
import Time exposing (Posix)


{-| The DatePicker model wraps SingleDatePicker's model and stores the selected date
-}
type alias DatePicker =
    { picker : SingleDatePicker.DatePicker
    , selectedDate : Maybe Posix
    }


{-| Messages for the date picker
-}
type Msg
    = DatePickerMsg SingleDatePicker.Msg


{-| Initialize a new date picker with an optional initial date
-}
init : Maybe Posix -> DatePicker
init initialDate =
    { picker = SingleDatePicker.init
    , selectedDate = initialDate
    }


{-| Update the date picker
-}
update : Msg -> DatePicker -> ( DatePicker, Cmd Msg )
update msg model =
    case msg of
        DatePickerMsg subMsg ->
            let
                ( newPicker, pickerCmd, maybeNewDate ) =
                    SingleDatePicker.update subMsg model.picker
            in
            ( { model
                | picker = newPicker
                , selectedDate =
                    case maybeNewDate of
                        Just date ->
                            Just date

                        Nothing ->
                            model.selectedDate
              }
            , Cmd.map DatePickerMsg pickerCmd
            )


{-| View the date picker with custom settings
-}
view : DatePicker -> SingleDatePicker.Settings -> Html Msg
view model settings =
    SingleDatePicker.view
        model.selectedDate
        settings
        model.picker
        |> Html.map DatePickerMsg


{-| Get the currently selected date
-}
getSelectedDate : DatePicker -> Maybe Posix
getSelectedDate model =
    model.selectedDate


{-| Set the selected date programmatically
-}
setSelectedDate : Maybe Posix -> DatePicker -> DatePicker
setSelectedDate date model =
    { model | selectedDate = date }
