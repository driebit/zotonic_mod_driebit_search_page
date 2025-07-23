module DateDisplayMode exposing (..)

import DateDisplayMode.Calendar as Calendar
import DateDisplayMode.FixedRanges as FixedRanges
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Resource exposing (Resource)


type DateDisplayMode
    = FixedRanges FixedRanges.Model
    | Calendar Calendar.Model


type Msg
    = FixedRangesMsg FixedRanges.Msg
    | CalendarMsg Calendar.Msg


update : Msg -> DateDisplayMode -> DateDisplayMode
update msg displayMode =
    case ( msg, displayMode ) of
        ( FixedRangesMsg fixedRangesMsg, FixedRanges model ) ->
            FixedRanges (FixedRanges.update fixedRangesMsg model)

        ( CalendarMsg calendarMsg, Calendar model ) ->
            Calendar (Calendar.update calendarMsg model)

        _ ->
            displayMode


fromJson : Decoder DateDisplayMode
fromJson =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "buttons" ->
                        Decode.succeed (FixedRanges (FixedRanges.init Nothing))

                    "calendar" ->
                        Decode.succeed (Calendar Calendar.init)

                    _ ->
                        Decode.fail ("Unknown date display mode: " ++ str)
            )


view : DateDisplayMode -> Html Msg
view displayMode =
    case displayMode of
        FixedRanges model ->
            Html.map FixedRangesMsg (FixedRanges.view model)

        Calendar model ->
            Html.map CalendarMsg (Calendar.view model)
