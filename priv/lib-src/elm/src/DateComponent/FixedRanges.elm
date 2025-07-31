module DateComponent.FixedRanges exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (custom, onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List exposing (range)
import Resource exposing (Resource)


type Range
    = Last7Days
    | LastMonth
    | LastYear
    | PreviousYear
    | NextWeek
    | NextMonth
    | Upcoming
    | Today
    | Custom


type DateProp
    = PublicationDate
    | ModificationDate
    | EventDate


datePropFromString : String -> DateProp
datePropFromString prop =
    case prop of
        "publication_date" ->
            PublicationDate

        "modification_date" ->
            ModificationDate

        "event_date" ->
            EventDate

        _ ->
            PublicationDate


allRanges : DateProp -> List Range
allRanges dateProp =
    case dateProp of
        PublicationDate ->
            [ Today, Last7Days, LastMonth, LastYear, PreviousYear, Custom ]

        ModificationDate ->
            [ Today, Last7Days, LastMonth, LastYear, PreviousYear, Custom ]

        EventDate ->
            [ Upcoming, Today, NextWeek, NextMonth, LastYear, Custom ]


type alias Model =
    { ranges : List Range
    , selectedRange : Maybe Range
    , customStart : String
    , customEnd : String
    , dateProp : DateProp
    }


init : Maybe (List Range) -> String -> Model
init ranges datePropString =
    let
        dateProp =
            datePropFromString datePropString

        ranges_ =
            Maybe.withDefault (allRanges dateProp) ranges
    in
    { ranges = ranges_
    , selectedRange = Nothing
    , customStart = ""
    , customEnd = ""
    , dateProp = dateProp
    }


type Msg
    = SelectRange Range
    | EnterStartDate String
    | EnterEndDate String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectRange range ->
            if model.selectedRange == Just range then
                { model | selectedRange = Nothing }

            else
                { model | selectedRange = Just range }

        EnterStartDate start ->
            let
                cleanedStart =
                    removeInvalidDateChars start

                newSelectedRange =
                    if isValidDate cleanedStart then
                        Just Custom

                    else
                        Nothing
            in
            { model | customStart = cleanedStart, selectedRange = newSelectedRange }

        EnterEndDate end ->
            let
                cleanedEnd =
                    removeInvalidDateChars end

                newSelectedRange =
                    if isValidDate cleanedEnd then
                        Just Custom

                    else
                        Nothing
            in
            { model | customEnd = cleanedEnd, selectedRange = newSelectedRange }


removeInvalidDateChars : String -> String
removeInvalidDateChars date =
    String.filter (\c -> Char.isDigit c || c == '-' || c == '/') date


isValidDate : String -> Bool
isValidDate date =
    case String.split "-" date of
        [ day, month, year ] ->
            case ( String.toInt day, String.toInt month, String.toInt year ) of
                ( Just d, Just m, Just y ) ->
                    d > 0 && d < 32 && m > 0 && m <= 12 && y > 1000 && y < 3000

                _ ->
                    False

        _ ->
            False


view : Model -> Html Msg
view model =
    div [ class "fixed-ranges" ]
        (List.map
            (viewOption
                model.selectedRange
                model.customStart
                model.customEnd
            )
            model.ranges
        )


viewOption : Maybe Range -> String -> String -> Range -> Html Msg
viewOption selectedRange start end currentRange =
    let
        isSelected =
            Just currentRange == selectedRange
    in
    li
        [ class
            (if isSelected then
                "selected"

             else
                ""
            )
        , if isSelected then
            style "background-color" "lightblue"

          else
            style "background-color" "transparent"
        ]
        [ case currentRange of
            Custom ->
                div []
                    [ input [ type_ "text", placeholder "Start date", value start, onInput EnterStartDate, value start ] []
                    , input [ type_ "text", placeholder "End date", value end, onInput EnterEndDate, value end ] []
                    , text "Custom Range"
                    ]

            _ ->
                button [ onClick (SelectRange currentRange) ] [ text (toString currentRange) ]
        ]


toString : Range -> String
toString range =
    case range of
        Last7Days ->
            "Last 7 Days"

        LastMonth ->
            "Last Month"

        LastYear ->
            "Last Year"

        PreviousYear ->
            "Previous Year"

        NextWeek ->
            "Next Week"

        NextMonth ->
            "Next Month"

        Upcoming ->
            "Upcoming"

        Today ->
            "Today"

        Custom ->
            "Custom Range"


encodedValue : Model -> List ( String, Encode.Value )
encodedValue model =
    case model.dateProp of
        PublicationDate ->
            rangeToEncodedValue "publication_before" "publication_after" model.customStart model.customEnd model.selectedRange

        ModificationDate ->
            rangeToEncodedValue "modified_before" "modified_after" model.customStart model.customEnd model.selectedRange

        EventDate ->
            rangeToEncodedValue "date_start_before" "date_end_after" model.customStart model.customEnd model.selectedRange


rangeToEncodedValue : String -> String -> String -> String -> Maybe Range -> List ( String, Encode.Value )
rangeToEncodedValue beforeTerm afterTerm customStart customEnd range =
    case range of
        Just Custom ->
            [ ( afterTerm, Encode.string customStart ), ( beforeTerm, Encode.string customEnd ) ]

        Just Last7Days ->
            [ ( afterTerm, Encode.string "-1 week" ), ( beforeTerm, Encode.string "now" ) ]

        Just LastMonth ->
            [ ( afterTerm, Encode.string "-1 month" ), ( beforeTerm, Encode.string "now" ) ]

        Just LastYear ->
            [ ( afterTerm, Encode.string "-1 year" ), ( beforeTerm, Encode.string "now" ) ]

        Just PreviousYear ->
            [ ( afterTerm, Encode.string "-2 years" ), ( beforeTerm, Encode.string "-1 year" ) ]

        Just NextWeek ->
            [ ( afterTerm, Encode.string "now" ), ( beforeTerm, Encode.string "+1 week" ) ]

        Just NextMonth ->
            [ ( afterTerm, Encode.string "now" ), ( beforeTerm, Encode.string "+1 month" ) ]

        Just Upcoming ->
            [ ( afterTerm, Encode.string "now" ) ]

        Just Today ->
            [ ( afterTerm, Encode.string "now" ), ( beforeTerm, Encode.string "+1 day" ) ]

        Nothing ->
            []
