module Filter.DateComponent exposing (..)

import Filter.DateComponent.Calendar as Calendar
import Filter.DateComponent.FixedRanges as FixedRanges
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Translations exposing (Language)


type DateComponent
    = FixedRanges FixedRanges.Model
    | Calendar Calendar.Model


type Msg
    = FixedRangesMsg FixedRanges.Msg
    | CalendarMsg Calendar.Msg


update : Msg -> DateComponent -> DateComponent
update msg displayMode =
    case ( msg, displayMode ) of
        ( FixedRangesMsg fixedRangesMsg, FixedRanges model ) ->
            FixedRanges (FixedRanges.update fixedRangesMsg model)

        ( CalendarMsg calendarMsg, Calendar model ) ->
            Calendar (Calendar.update calendarMsg model)

        _ ->
            displayMode


fromJson : String -> Decoder DateComponent
fromJson dateProp =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "fixed_ranges" ->
                        Decode.succeed (FixedRanges (FixedRanges.init Nothing dateProp))

                    "calendar" ->
                        Decode.succeed (Calendar Calendar.init)

                    _ ->
                        Decode.fail ("Unknown date display mode: " ++ str)
            )


view : Language -> DateComponent -> Html Msg
view language component =
    case component of
        FixedRanges model ->
            Html.map FixedRangesMsg (FixedRanges.view language model)

        Calendar model ->
            Html.map CalendarMsg (Calendar.view language model)


encodedValue : DateComponent -> List ( String, Decode.Value )
encodedValue component =
    case component of
        FixedRanges model ->
            FixedRanges.encodedValue model

        Calendar model ->
            Calendar.encodedValue model


summaryView : Language -> DateComponent -> Maybe (Html Msg)
summaryView language component =
    case component of
        FixedRanges model ->
            FixedRanges.summaryView language model
                |> Maybe.map (Html.map FixedRangesMsg)

        Calendar _ ->
            Nothing


toUrlValue : DateComponent -> Maybe String
toUrlValue component =
    case component of
        FixedRanges model ->
            FixedRanges.toUrlValue model

        Calendar _ ->
            Nothing


applyUrlValue : List ( String, Decode.Value ) -> DateComponent -> DateComponent
applyUrlValue params component =
    case component of
        FixedRanges model ->
            FixedRanges (FixedRanges.applyUrlValue params model)

        Calendar model ->
            Calendar model


applyUrlString : String -> DateComponent -> DateComponent
applyUrlString encoded component =
    case component of
        FixedRanges model ->
            FixedRanges (FixedRanges.applyUrlString encoded model)

        Calendar model ->
            Calendar model


isSet : DateComponent -> Bool
isSet component =
    case component of
        FixedRanges model ->
            FixedRanges.isSet model

        Calendar model ->
            Calendar.isSet model
