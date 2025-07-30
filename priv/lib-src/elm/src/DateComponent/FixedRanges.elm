module DateComponent.FixedRanges exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Resource exposing (Resource)


type Range
    = Last7Days
    | LastMonth
    | LastYear
    | Upcoming
    | Today
    | All
    | Custom String String


allRanges : List Range
allRanges =
    [ Last7Days
    , LastMonth
    , LastYear
    , Upcoming
    , Today
    , Custom "" ""
    ]


type alias Model =
    { ranges : List Range
    , selectedRange : Maybe Range
    }


init : Maybe (List Range) -> Model
init ranges =
    { ranges = Maybe.withDefault allRanges ranges
    , selectedRange = Nothing
    }


type Msg
    = SelectRange Range


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectRange range ->
            if model.selectedRange == Just range then
                { model | selectedRange = Nothing }

            else
                { model | selectedRange = Just range }


view : Model -> Html Msg
view model =
    div [ class "fixed-ranges" ]
        (List.map
            (\range ->
                let
                    isSelected =
                        model.selectedRange == Just range
                in
                button
                    [ class
                        (if isSelected then
                            "selected"

                         else
                            ""
                        )
                    , onClick (SelectRange range)
                    ]
                    [ text (toString range) ]
            )
            model.ranges
        )


toString : Range -> String
toString range =
    case range of
        Last7Days ->
            "Last 7 Days"

        LastMonth ->
            "Last Month"

        LastYear ->
            "Last Year"

        Upcoming ->
            "Upcoming"

        Today ->
            "Today"

        All ->
            "All"

        Custom start end ->
            "Custom: " ++ start ++ " to " ++ end


encodedValue : Model -> Maybe Decode.Value
encodedValue model =
    Nothing
