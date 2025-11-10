module Filter.DateComponent.FixedRanges exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (custom, onClick, onInput)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List exposing (range)
import Resource exposing (Resource)
import String
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


summaryView : Language -> Model -> Maybe (Html Msg)
summaryView language model =
    case model.selectedRange of
        Nothing ->
            Nothing

        Just Custom ->
            let
                start =
                    String.trim model.customStart

                end =
                    String.trim model.customEnd
            in
            case ( String.isEmpty start, String.isEmpty end ) of
                ( True, True ) ->
                    pill (translate language translations.fixedRangesCustomRange) (SelectRange Custom)

                ( False, True ) ->
                    pill start (SelectRange Custom)

                ( True, False ) ->
                    pill end (SelectRange Custom)

                ( False, False ) ->
                    pill (start ++ " - " ++ end) (SelectRange Custom)

        Just range ->
            pill (toStringTranslated language range) (SelectRange range)


pill : String -> Msg -> Maybe (Html Msg)
pill label msg =
    let
        stopClick =
            custom "click" (Decode.succeed { message = msg, stopPropagation = True, preventDefault = True })
    in
    Just <|
        div [ class "c-multiselect__selected" ]
            [ span [ class "c-multiselect__pill", stopClick ]
                [ text label
                , span [ class "c-multiselect__remove" ] [ text "×" ]
                ]
            ]


toUrlValue : Model -> Maybe String
toUrlValue model =
    case model.selectedRange of
        Nothing ->
            Nothing

        Just Custom ->
            Just ("custom:" ++ model.customStart ++ "|" ++ model.customEnd)

        Just range ->
            Just (rangeToSlug range)


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


applyUrlValue : List ( String, Decode.Value ) -> Model -> Model
applyUrlValue params model =
    let
        ( beforeTerm, afterTerm ) =
            case model.dateProp of
                PublicationDate ->
                    ( "publication_before", "publication_after" )

                ModificationDate ->
                    ( "modified_before", "modified_after" )

                EventDate ->
                    ( "date_start_before", "date_end_after" )

                CustomDateProp prop ->
                    ( prop ++ "_before", prop ++ "_after" )

        maybeDecodeString value =
            case Decode.decodeValue Decode.string value of
                Ok str ->
                    Just str

                Err _ ->
                    Nothing

        findValue term =
            params
                |> List.filterMap
                    (\( key, value ) ->
                        if key == term then
                            maybeDecodeString value

                        else
                            Nothing
                    )
                |> List.head

        afterValue =
            findValue afterTerm

        beforeValue =
            findValue beforeTerm

        resetCustom model_ =
            { model_ | customStart = "", customEnd = "" }
    in
    case ( afterValue, beforeValue ) of
        ( Just "-1 week", Just "now" ) ->
            { model | selectedRange = Just Last7Days }
                |> resetCustom

        ( Just "-1 month", Just "now" ) ->
            { model | selectedRange = Just LastMonth }
                |> resetCustom

        ( Just "-1 year", Just "now" ) ->
            { model | selectedRange = Just LastYear }
                |> resetCustom

        ( Just "-2 years", Just "-1 year" ) ->
            { model | selectedRange = Just PreviousYear }
                |> resetCustom

        ( Just "now", Just "+1 week" ) ->
            { model | selectedRange = Just NextWeek }
                |> resetCustom

        ( Just "now", Just "+1 month" ) ->
            { model | selectedRange = Just NextMonth }
                |> resetCustom

        ( Just "now", Just "+1 day" ) ->
            { model | selectedRange = Just Today }
                |> resetCustom

        ( Just "now", Nothing ) ->
            { model | selectedRange = Just Upcoming }
                |> resetCustom

        ( Nothing, Nothing ) ->
            model

        ( after, before ) ->
            { model
                | selectedRange = Just Custom
                , customStart = Maybe.withDefault "" after
                , customEnd = Maybe.withDefault "" before
            }


applyUrlString : String -> Model -> Model
applyUrlString encoded model =
    let
        trimmed =
            String.trim encoded

        resetCustom model_ =
            { model_ | customStart = "", customEnd = "" }
    in
    if String.isEmpty trimmed then
        { model | selectedRange = Nothing }
            |> resetCustom

    else if String.length trimmed >= 7 && String.toLower (String.left 7 trimmed) == "custom:" then
        let
            remainder =
                String.dropLeft 7 trimmed

            ( start, end_ ) =
                parseCustom remainder
        in
        { model
            | selectedRange = Just Custom
            , customStart = start
            , customEnd = end_
        }

    else
        case slugToRange (String.toLower trimmed) of
            Just range ->
                { model | selectedRange = Just range }
                    |> resetCustom

            Nothing ->
                model


rangeToSlug : Range -> String
rangeToSlug range =
    case range of
        Last7Days ->
            "last7days"

        LastMonth ->
            "lastmonth"

        LastYear ->
            "lastyear"

        PreviousYear ->
            "previousyear"

        NextWeek ->
            "nextweek"

        NextMonth ->
            "nextmonth"

        Upcoming ->
            "upcoming"

        Today ->
            "today"

        Custom ->
            "custom"


slugToRange : String -> Maybe Range
slugToRange slug =
    case slug of
        "last7days" ->
            Just Last7Days

        "lastmonth" ->
            Just LastMonth

        "lastyear" ->
            Just LastYear

        "previousyear" ->
            Just PreviousYear

        "nextweek" ->
            Just NextWeek

        "nextmonth" ->
            Just NextMonth

        "upcoming" ->
            Just Upcoming

        "today" ->
            Just Today

        _ ->
            Nothing


parseCustom : String -> ( String, String )
parseCustom value =
    case String.split "|" value of
        start :: end_ :: _ ->
            ( start, end_ )

        [ start ] ->
            ( start, "" )

        _ ->
            ( "", "" )


isSet : Model -> Bool
isSet model =
    model.selectedRange /= Nothing
