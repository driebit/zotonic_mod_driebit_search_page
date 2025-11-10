module Filter.TextualComponent.Multiselect exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Resource exposing (Resource)
import String
import Translations exposing (Language, translate, translations)


type alias Model =
    { selected : List Int
    , options : List Resource
    , searchQuery : String
    , currentQuery : String
    , numberOfResultsVisible : Int
    , isLoading : Bool
    , hasFetchedOnce : Bool
    , page : Int
    , hasMore : Bool
    , pendingAppend : Bool
    , initialOptions : List Resource
    , initialHasMore : Bool
    }


init : List Resource -> Bool -> Model
init options hasMore =
    { selected = []
    , options = options
    , searchQuery = ""
    , currentQuery = ""
    , numberOfResultsVisible = 10
    , isLoading = False
    , hasFetchedOnce = not (List.isEmpty options)
    , page = if List.isEmpty options then 0 else 1
    , hasMore = hasMore
    , pendingAppend = False
    , initialOptions = options
    , initialHasMore = hasMore
    }


type Msg
    = NoOp
    | Select Int
    | SearchInput String
    | ShowMoreResults
    | OptionsFetched String (List Resource) Int Bool


type alias FetchRequest =
    { query : String
    , page : Int
    , append : Bool
    }


type Effect
    = FetchOptions FetchRequest


update : Msg -> Model -> ( Model, List Effect )
update msg model =
    case msg of
        NoOp ->
            ( model, [] )

        Select option ->
            if List.member option model.selected then
                ( { model | selected = List.filter ((/=) option) model.selected }, [] )

            else
                ( { model | selected = option :: model.selected }, [] )

        SearchInput query ->
            let
                normalizedQuery =
                    String.trim query

                previousQuery =
                    model.currentQuery

                baseModel =
                    { model
                        | searchQuery = query
                        , currentQuery = normalizedQuery
                        , numberOfResultsVisible = 10
                        , page = 0
                        , hasMore = False
                        , pendingAppend = False
                    }
            in
            if String.isEmpty normalizedQuery then
                let
                    resetOptions =
                        baseModel.initialOptions

                    resetHasMore =
                        baseModel.initialHasMore

                    resetPage =
                        if List.isEmpty resetOptions then
                            0

                        else
                            1

                    resetVisible =
                        Basics.min (List.length resetOptions) 10

                    resetModel =
                        { baseModel
                            | options = resetOptions
                            , isLoading = True
                            , hasFetchedOnce = False
                            , page = resetPage
                            , hasMore = resetHasMore
                            , numberOfResultsVisible = resetVisible
                            , pendingAppend = False
                        }

                    shouldFetch =
                        model.currentQuery /= "" || not model.hasFetchedOnce || List.isEmpty resetOptions
                in
                if shouldFetch then
                    ( resetModel
                    , [ FetchOptions { query = "", page = resetModel.page, append = False } ]
                    )

                else
                    ( { resetModel | isLoading = False, hasFetchedOnce = True }, [] )

            else if normalizedQuery == previousQuery && model.hasFetchedOnce then
                ( baseModel, [] )

            else
                ( { baseModel
                    | options = []
                    , isLoading = True
                    , hasFetchedOnce = False
                    , page = 0
                    , hasMore = False
                    , pendingAppend = False
                  }
                , [ FetchOptions { query = normalizedQuery, page = 1, append = False } ]
                )

        ShowMoreResults ->
            let
                newVisible =
                    model.numberOfResultsVisible + 5
            in
            if newVisible <= List.length model.options || not model.hasMore || model.isLoading then
                ( { model | numberOfResultsVisible = newVisible }, [] )

            else
                ( { model
                    | numberOfResultsVisible = newVisible
                    , isLoading = True
                    , pendingAppend = True
                  }
                , [ FetchOptions { query = model.currentQuery, page = model.page + 1, append = True } ]
                )

        OptionsFetched query options page hasMore ->
            if query /= model.currentQuery then
                ( model, [] )

            else
                let
                    updatedOptions =
                        if model.pendingAppend then
                            let
                                existingIds =
                                    List.map .id model.options

                                newOptions =
                                    List.filter (
                                        \option ->
                                            not (List.member option.id existingIds)
                                    ) options
                            in
                            model.options ++ newOptions

                        else
                            options

                    newVisibleCount =
                        if model.pendingAppend then
                            model.numberOfResultsVisible

                        else
                            Basics.min (List.length updatedOptions) 10

                    updatedInitialOptions =
                        if model.currentQuery == "" then
                            updatedOptions

                        else
                            model.initialOptions

                    updatedInitialHasMore =
                        if model.currentQuery == "" then
                            hasMore

                        else
                            model.initialHasMore
                in
                ( { model
                    | options = updatedOptions
                    , isLoading = False
                    , numberOfResultsVisible = newVisibleCount
                    , hasFetchedOnce = True
                    , page = page
                    , hasMore = hasMore
                    , pendingAppend = False
                    , initialOptions = updatedInitialOptions
                    , initialHasMore = updatedInitialHasMore
                  }
                , []
                )


initialize : Model -> ( Model, List Effect )
initialize model =
    ( model, [] )


view : Language -> Model -> Html Msg
view language model =
    div [ class "c-multiselect" ]
        [ input
            [ name "multiselect"
            , class "c-multiselect__input"
            , type_ "text"
            , placeholder (translate language translations.multiselectSearchPlaceholder)
            , onInput SearchInput
            , value model.searchQuery
            ]
            []
        , div [ class "c-multiselect__selected" ]
            (List.map (viewSelectedPill model.options) model.selected)
        , div [ class "c-multiselect__options" ]
            [ if model.isLoading then
                div [ class "c-multiselect__loading" ] [ text (translate language translations.loading) ]

              else
                text ""
            , ul [ class "c-multiselect__list" ]
                (model.options
                    |> List.take model.numberOfResultsVisible
                    |> List.map (viewOption model.selected)
                )
            , if List.length model.options > model.numberOfResultsVisible then
                button [ class "c-multiselect__show-more", onClick ShowMoreResults ]
                    [ text (translate language translations.multiselectShowMore) ]

              else
                text ""
            ]
        ]


viewSelectedPill : List Resource -> Int -> Html Msg
viewSelectedPill options id =
    case List.filter (\o -> o.id == id) options |> List.head of
        Just option ->
            span [ class "c-multiselect__pill", onClick (Select option.id) ]
                [ text option.title
                , span [ class "c-multiselect__remove" ] [ text "×" ]
                ]

        Nothing ->
            text ""


viewOption : List Int -> Resource -> Html Msg
viewOption selectedOptions option =
    let
        isSelected =
            List.member option.id selectedOptions
    in
    li [ class "c-multiselect__option" ]
        [ input
            [ onCheck (always (Select option.id))
            , class "c-multiselect__checkbox"
            , type_ "checkbox"
            , checked isSelected
            , id (String.fromInt option.id)
            ]
            []
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


isSet : Model -> Bool
isSet model =
    not (List.isEmpty model.selected)
