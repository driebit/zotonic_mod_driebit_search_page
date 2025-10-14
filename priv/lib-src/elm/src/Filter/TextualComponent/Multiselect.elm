module Filter.TextualComponent.Multiselect exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Resource exposing (Resource)
import Time exposing (Month(..))
import Translations exposing (Language, translate, translations)


type alias Model =
    { selected : List Int
    , options : List Resource
    , filteredOptions : List Resource
    , searchQuery : String
    , numberOfResultsVisible : Int
    }


init : List Resource -> Model
init options =
    let
        initialModel =
            { selected = []
            , options = options
            , searchQuery = ""
            , filteredOptions = options
            , numberOfResultsVisible = 10
            }
    in
    initialModel


type Msg
    = NoOp
    | Select Int
    | SearchInput String
    | ShowMoreResults


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model

        Select option ->
            if List.member option model.selected then
                { model | selected = List.filter ((/=) option) model.selected }

            else
                { model | selected = option :: model.selected }

        SearchInput query ->
            let
                filteredOptions =
                    if String.isEmpty query then
                        model.options

                    else
                        List.filter (\option -> String.contains (String.toLower query) (String.toLower option.title)) model.options
            in
            { model | searchQuery = query, filteredOptions = filteredOptions }

        ShowMoreResults ->
            { model | numberOfResultsVisible = model.numberOfResultsVisible + 5 }



-- VIEW


view : Language -> Model -> Html Msg
view language model =
    div [ class "c-multiselect" ]
        [ input
            [ name "multiselect"
            , class "c-multiselect__input"
            , type_ "text"
            , placeholder (translate language translations.multiselectSearchPlaceholder)
            , onInput SearchInput
            ]
            []

        -- Display pills of the selected options
        , div [ class "c-multiselect__selected" ]
            (List.map
                (\id ->
                    let
                        option =
                            List.head (List.filter (\o -> o.id == id) model.options)
                    in
                    case option of
                        Just res ->
                            span [ class "c-multiselect__pill", onClick (Select res.id) ]
                                [ text res.title
                                , span [ class "c-multiselect__remove" ] [ text "×" ]
                                ]

                        Nothing ->
                            text ""
                )
                model.selected
            )
        , div [ class "c-multiselect__options" ]
            [ ul [ class "c-multiselect__list" ]
                (List.take model.numberOfResultsVisible model.filteredOptions
                    |> List.map (viewOption model.selected)
                )
            , if List.length model.filteredOptions > model.numberOfResultsVisible then
                button [ class "c-multiselect__show-more", onClick ShowMoreResults ]
                    [ text (translate language translations.multiselectShowMore) ]

              else
                text ""
            ]
        ]


viewOption : List Int -> Resource -> Html Msg
viewOption selectedOptions option =
    let
        isSelected =
            List.member option.id selectedOptions
    in
    li
        [ class "c-multiselect__option"
        ]
        [ input [ onCheck (always (Select option.id)), class "c-multiselect__checkbox", type_ "checkbox", checked isSelected, id (String.fromInt option.id) ] []
        , label [ class "c-multiselect__label", for (String.fromInt option.id) ] [ text option.title ]
        ]


encodedValue : String -> Maybe String -> Model -> List ( String, Encode.Value )
encodedValue filterProp maybePredicate model =
    let
        encodeList predicate =
            model.selected
                |> List.map (\id -> [ String.fromInt id, predicate ])
                |> Encode.list (Encode.list Encode.string)
    in
    if List.isEmpty model.selected then
        []

    else
        [ ( filterProp
          , case maybePredicate of
                Just predicate ->
                    encodeList predicate

                Nothing ->
                    model.selected
                        |> List.map String.fromInt
                        |> Encode.list Encode.string
          )
        ]


applyUrlValue : String -> Maybe String -> List ( String, Decode.Value ) -> Model -> Model
applyUrlValue filterProp maybePredicate params model =
    let
        matchingValues =
            params
                |> List.filterMap
                    (\( key, value ) ->
                        if key == filterProp then
                            Just value

                        else
                            Nothing
                    )

        maybeDecode decoder value =
            case Decode.decodeValue decoder value of
                Ok decoded ->
                    Just decoded

                Err _ ->
                    Nothing

        decodedIds =
            case maybePredicate of
                Just predicate ->
                    matchingValues
                        |> List.filterMap (maybeDecode (Decode.list (Decode.list Decode.string)))
                        |> List.concatMap
                            (\pairs ->
                                pairs
                                    |> List.filterMap
                                        (\pair ->
                                            case pair of
                                                [ idStr, predicateStr ] ->
                                                    if predicateStr == predicate then
                                                        String.toInt idStr

                                                    else
                                                        Nothing

                                                _ ->
                                                    Nothing
                                        )
                            )

                Nothing ->
                    matchingValues
                        |> List.filterMap (maybeDecode (Decode.list Decode.string))
                        |> List.concatMap identity
                        |> List.filterMap String.toInt
    in
    { model | selected = List.sort decodedIds }


selectedIds : Model -> List Int
selectedIds model =
    List.sort model.selected


setSelectedIds : List Int -> Model -> Model
setSelectedIds ids model =
    { model | selected = List.sort ids }
