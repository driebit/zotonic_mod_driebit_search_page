module Filter.DateComponent.FixedRanges exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (custom, onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List exposing (range)
import Resource exposing (Resource)
import Translations exposing (Language, translate, translations)


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
    | CustomDateProp String


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
            CustomDateProp prop


allRanges : DateProp -> List Range
allRanges dateProp =
    case dateProp of
        PublicationDate ->
            [ Today, Last7Days, LastMonth, LastYear, PreviousYear, Custom ]

        ModificationDate ->
            [ Today, Last7Days, LastMonth, LastYear, PreviousYear, Custom ]

        EventDate ->
            [ Upcoming, Today, NextWeek, NextMonth, LastYear, Custom ]

        CustomDateProp _ ->
            [ Today
            , Last7Days
            , LastMonth
            , LastYear
            , PreviousYear
            , Upcoming
            , NextWeek
            , NextMonth
            , Custom
            ]


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
            { model | customStart = start, selectedRange = Just Custom }

        EnterEndDate end ->
            { model | customEnd = end, selectedRange = Just Custom }


view : Language -> Model -> Html Msg
view language model =
    ul [ class "c-fixed-ranges" ]
        (List.map
            (viewOption
                language
                model.selectedRange
                model.customStart
                model.customEnd
            )
            model.ranges
        )


viewOption : Language -> Maybe Range -> String -> String -> Range -> Html Msg
viewOption language selectedRange start end currentRange =
    let
        isSelected =
            Just currentRange == selectedRange
    in
    li
        [ class
            (if isSelected then
                "c-fixed-ranges__option c-fixed-ranges__option--selected"

             else
                "c-fixed-ranges__option"
            )
        ]
        [ case currentRange of
            Custom ->
                div [ class "c-fixed-ranges__custom-inputs" ]
                    [ h3 [ class "c-fixed-ranges__custom-title" ] [ text (translate language translations.fixedRangesCustom) ]
                    , div [ class "c-fixed-ranges__custom" ]
                        [ label [ class "c-fixed-ranges__label" ] [ text (translate language translations.fixedRangesFrom) ]
                        , input
                            [ classList [ ( "c-fixed-ranges__input", True ), ( "c-fixed-ranges__input--selected", isSelected ) ]
                            , type_ "date"
                            , placeholder (translate language translations.fixedRangesPlaceholder)
                            , value start
                            , onInput EnterStartDate
                            ]
                            []
                        ]
                    , div [ class "c-fixed-ranges__custom" ]
                        [ label [ class "c-fixed-ranges__label" ] [ text (translate language translations.fixedRangesTo) ]
                        , input
                            [ classList [ ( "c-fixed-ranges__input", True ), ( "c-fixed-ranges__input--selected", isSelected ) ]
                            , type_ "date"
                            , placeholder (translate language translations.fixedRangesPlaceholder)
                            , value end
                            , onInput EnterEndDate
                            ]
                            []
                        ]
                    ]

            _ ->
                button
                    [ classList
                        [ ( "c-fixed-ranges__button", True )
                        , ( "c-fixed-ranges__button--selected", isSelected )
                        ]
                    , onClick (SelectRange currentRange)
                    ]
                    [ text (toStringTranslated language currentRange) ]
        ]


toStringTranslated : Language -> Range -> String
toStringTranslated language range =
    case range of
        Last7Days ->
            translate language translations.fixedRangesLast7Days

        LastMonth ->
            translate language translations.fixedRangesLastMonth

        LastYear ->
            translate language translations.fixedRangesLastYear

        PreviousYear ->
            translate language translations.fixedRangesPreviousYear

        NextWeek ->
            translate language translations.fixedRangesNextWeek

        NextMonth ->
            translate language translations.fixedRangesNextMonth

        Upcoming ->
            translate language translations.fixedRangesUpcoming

        Today ->
            translate language translations.fixedRangesToday

        Custom ->
            translate language translations.fixedRangesCustomRange


encodedValue : Model -> List ( String, Encode.Value )
encodedValue model =
    case model.dateProp of
        PublicationDate ->
            rangeToEncodedValue "publication_before" "publication_after" model.customStart model.customEnd model.selectedRange

        ModificationDate ->
            rangeToEncodedValue "modified_before" "modified_after" model.customStart model.customEnd model.selectedRange

        EventDate ->
            rangeToEncodedValue "date_start_before" "date_end_after" model.customStart model.customEnd model.selectedRange

        CustomDateProp prop ->
            rangeToEncodedValue (prop ++ "_before") (prop ++ "_after") model.customStart model.customEnd model.selectedRange


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
            [ ( afterTerm, Encode.string "-1 day" ), ( beforeTerm, Encode.string "+1 day" ) ]

        Nothing ->
            []
