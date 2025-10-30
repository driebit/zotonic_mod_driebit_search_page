port module SearchPage exposing (..)

import Browser
import Collapse exposing (Collapse)
import Cotonic exposing (CotonicCall, searchPageTopic, templateTopic)
import Dict exposing (Dict)
import Filter exposing (Filter)
import Flags exposing (Flags)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Parser
import Html.Parser.Util
import Json.Decode as Decode
import Json.Encode as Encode
import List exposing (sort)
import Pagination
import Task exposing (Task)
import Time exposing (Month(..))
import Translations exposing (Language, translate, translations)


port searchPageCall : Encode.Value -> Cmd msg


port searchPageReply : (Encode.Value -> msg) -> Sub msg


port connected : (Bool -> msg) -> Sub msg


port screenResized : (Int -> msg) -> Sub msg


port updateUrl : Encode.Value -> Cmd msg


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { filters : List Filter
    , fullTextSearchQuery : String
    , results : SearchResult
    , templateCache : Dict Int (List (Html Msg))
    , sortBy : Maybe String
    , language : Translations.Language
    , showFilters : Collapse
    , excludedCategories : List String
    , pagination : Pagination.Model
    , pageLength : Int
    }


type SearchResult
    = NotAsked
    | WaitingForConnection
    | Loading
    | Loaded (List Int) Pagination.Model
    | Error String


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        decodedFlags =
            Decode.decodeValue Flags.fromJson flags
                |> Result.withDefault Flags.defaultFlags

        initialModel =
            { filters = decodedFlags.filters
            , results = WaitingForConnection
            , fullTextSearchQuery = decodedFlags.queryString |> Maybe.withDefault ""
            , templateCache = Dict.empty
            , sortBy = Nothing
            , language = decodedFlags.language
            , showFilters = Collapse.fromPageWidth decodedFlags.screenWidth
            , excludedCategories = decodedFlags.excludeCategories
            , pagination = Pagination.init
            , pageLength = decodedFlags.pageLength
            }

        hydratedModel =
            applyQueryParams decodedFlags.queryParams initialModel
    in
    ( hydratedModel, syncUrl hydratedModel )


type Msg
    = FilterMsg String Filter.Msg
    | SearchPageReply Decode.Value
    | FullTextSearchInput String
    | CotonicReady Bool
    | ChangeSort String
    | ChangePage Int
    | ScreenResized Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FilterMsg id filterMsg ->
            let
                updatedFilters =
                    model.filters
                        |> List.map
                            (\filter ->
                                if filter.id == id then
                                    Filter.update filterMsg filter

                                else
                                    filter
                            )

                pagination =
                    model.pagination

                updatedPagination =
                    { pagination | currentPage = 1 }

                updatedModel =
                    { model | filters = updatedFilters, pagination = updatedPagination }
            in
            ( updatedModel
            , Cmd.batch
                [ searchPageCall (encodedSearchParams updatedModel)
                , syncUrl updatedModel
                ]
            )

        SearchPageReply reply ->
            case Decode.decodeValue (Decode.field "topic" Decode.string) reply |> Result.map (String.split "/") of
                Ok [ "SearchReply" ] ->
                    let
                        decoder =
                            Decode.map2 Loaded
                                (Decode.at [ "reply", "payload", "result", "result" ] (Decode.list Decode.int))
                                (Decode.at [ "reply", "payload", "result" ] Pagination.fromJson)
                    in
                    case Decode.decodeValue decoder reply of
                        Ok (Loaded results paginationInfo) ->
                            let
                                templateCalls =
                                    results
                                        |> List.map (\id -> templateTopic id)
                                        |> List.map Cotonic.toJson
                                        |> List.map searchPageCall
                            in
                            ( { model | results = Loaded results paginationInfo, pagination = paginationInfo }, Cmd.batch templateCalls )

                        Ok _ ->
                            ( { model | results = Error "Search results returned an unexpected format" }, Cmd.none )

                        Err err ->
                            ( { model | results = Error (Decode.errorToString err) }, Cmd.none )

                Ok [ "TemplateReply", idString ] ->
                    case Decode.decodeValue (Decode.at [ "reply", "payload", "result" ] Decode.string) reply of
                        Ok template ->
                            let
                                parsedTemplateResult =
                                    Html.Parser.run template

                                newTemplateCache =
                                    case ( String.toInt idString, parsedTemplateResult ) of
                                        ( Just id, Ok parsedTemplate ) ->
                                            Dict.insert id (Html.Parser.Util.toVirtualDom parsedTemplate) model.templateCache

                                        _ ->
                                            model.templateCache
                            in
                            ( { model | templateCache = newTemplateCache }, Cmd.none )

                        Err err ->
                            ( { model | results = Error (Decode.errorToString err) }, Cmd.none )

                Ok _ ->
                    ( model, Cmd.none )

                Err err ->
                    ( { model | results = Error (Decode.errorToString err) }, Cmd.none )

        FullTextSearchInput query ->
            let
                pagination =
                    model.pagination

                updatedModel =
                    { model | fullTextSearchQuery = query, pagination = { pagination | currentPage = 1 } }
            in
            ( updatedModel
            , Cmd.batch
                [ searchPageCall (encodedSearchParams updatedModel)
                , syncUrl updatedModel
                ]
            )

        CotonicReady _ ->
            let
                updatedModel =
                    { model | results = Loading }
            in
            ( updatedModel
            , Cmd.batch
                [ searchPageCall (encodedSearchParams updatedModel)
                , syncUrl updatedModel
                ]
            )

        ChangePage pageNumber ->
            let
                pagination =
                    model.pagination

                updatedModel =
                    { model | pagination = { pagination | currentPage = pageNumber } }
            in
            ( updatedModel
            , Cmd.batch
                [ searchPageCall (encodedSearchParamsWithPage updatedModel)
                , syncUrl updatedModel
                ]
            )

        ChangeSort newSort ->
            let
                maybeNewSort =
                    if newSort == "relevance" then
                        Nothing

                    else
                        Just newSort

                pagination =
                    model.pagination

                updatedModel =
                    { model | sortBy = maybeNewSort, pagination = { pagination | currentPage = 1 } }
            in
            ( updatedModel
            , Cmd.batch
                [ searchPageCall (encodedSearchParams updatedModel)
                , syncUrl updatedModel
                ]
            )

        ScreenResized width ->
            let
                newCollapseState =
                    Collapse.fromPageWidth width

                updatedModel =
                    { model | showFilters = newCollapseState }
            in
            ( updatedModel, Cmd.none )


encodedSearchParams : Model -> Decode.Value
encodedSearchParams model =
    model
        |> searchParamsList
        |> Cotonic.searchPageTopic
        |> Cotonic.toJson


encodedSearchParamsWithPage : Model -> Decode.Value
encodedSearchParamsWithPage model =
    model
        |> searchParamsList
        |> List.append [ ( "page", Encode.int model.pagination.currentPage ) ]
        |> Cotonic.searchPageTopic
        |> Cotonic.toJson


syncUrl : Model -> Cmd Msg
syncUrl model =
    model
        |> queryParams
        |> encodeQueryParams
        |> updateUrl


encodeQueryParams : List ( String, String ) -> Encode.Value
encodeQueryParams params =
    Encode.list
        (\( key, value ) ->
            Encode.object
                [ ( "key", Encode.string key )
                , ( "value", Encode.string value )
                ]
        )
        params


queryParams : Model -> List ( String, String )
queryParams model =
    let
        textParam =
            if String.isEmpty model.fullTextSearchQuery then
                []

            else
                [ ( "qs", model.fullTextSearchQuery ) ]

        sortParam =
            case model.sortBy of
                Just sort ->
                    [ ( "asort", sort ) ]

                Nothing ->
                    []

        pageParam =
            if model.pagination.currentPage <= 1 then
                []

            else
                [ ( "page", String.fromInt model.pagination.currentPage ) ]

        filterParams =
            model.filters
                |> List.filterMap
                    (\filter ->
                        Filter.toUrlQueryValue filter
                            |> Maybe.map (\value -> ( filter.id, value ))
                    )
    in
    textParam
        ++ sortParam
        ++ pageParam
        ++ filterParams


applyQueryParams : Dict String String -> Model -> Model
applyQueryParams urlParams model =
    let
        fullText =
            Dict.get "qs" urlParams
                |> Maybe.withDefault model.fullTextSearchQuery

        sortByValue =
            case Dict.get "asort" urlParams of
                Just sort ->
                    if sort == "relevance" || String.isEmpty sort then
                        Nothing

                    else
                        Just sort

                Nothing ->
                    model.sortBy

        pageNumber =
            Dict.get "page" urlParams
                |> Maybe.andThen String.toInt
                |> Maybe.withDefault model.pagination.currentPage

        filters =
            model.filters
                |> List.map (Filter.applyUrlEncodedValue urlParams)

        paginationWithPage =
            let
                paginationModel =
                    model.pagination
            in
            { paginationModel | currentPage = pageNumber }
    in
    { model
        | fullTextSearchQuery = fullText
        , sortBy = sortByValue
        , filters = filters
        , pagination = paginationWithPage
    }


decodeStringList : String -> Maybe (List String)
decodeStringList raw =
    case Decode.decodeString (Decode.list Decode.string) raw of
        Ok values ->
            Just values

        Err _ ->
            Nothing


searchParamsList : Model -> List ( String, Encode.Value )
searchParamsList model =
    let
        baseFilters =
            model.filters
                |> List.concatMap Filter.toSearchParams

        filters =
            ( "pagelen", Encode.int model.pageLength )
                :: ( "text_contains", Encode.string model.fullTextSearchQuery )
                :: ( "cat_exclude", Encode.list Encode.string model.excludedCategories )
                :: ( "page", Encode.int model.pagination.currentPage )
                :: baseFilters
    in
    case model.sortBy of
        Just sort ->
            ( "asort", Encode.string sort )
                :: filters

        Nothing ->
            filters



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "c-search" ]
        [ div [ class "c-full-text-search" ]
            [ div [ class "c-full-text-search__wrapper" ]
                [ input
                    [ class "c-full-text-search__searchbar"
                    , type_ "text"
                    , placeholder (translate model.language translations.searchPlaceholder)
                    , value model.fullTextSearchQuery
                    , onInput FullTextSearchInput
                    , id "search-bar"
                    ]
                    []
                ]
            ]
        , div [ class "c-search-filters" ]
            [ Collapse.view
                model.showFilters
                (h2 [ class "c-search-filters__title" ] [ text (translate model.language translations.searchFilters) ])
                (div
                    [ class "c-search-filters__content" ]
                    (List.map
                        (\filter ->
                            Html.map (FilterMsg filter.id) (Filter.view model.language filter)
                        )
                        model.filters
                    )
                )
            ]
        , div [ class "c-search-results" ]
            [ viewResults model.language model.results model.templateCache model.sortBy
            , div [ class "c-pagination" ]
                [ Pagination.view model.language model.pagination ChangePage ]
            ]
        ]


viewResults : Language -> SearchResult -> Dict Int (List (Html Msg)) -> Maybe String -> Html Msg
viewResults language results templateCache activeSort =
    case results of
        NotAsked ->
            div [ class "c-search-results__notice" ]
                [ text (translate language translations.noSearchYet) ]

        Loading ->
            div [ class "c-search-results__notice" ]
                [ text (translate language translations.loading) ]

        WaitingForConnection ->
            div [ class "c-search-results__notice" ]
                [ text (translate language translations.waitingForConnection) ]

        Loaded resultIds paginationInfo ->
            let
                resultTemplates =
                    resultIds
                        |> List.map (\id -> Dict.get id templateCache |> Maybe.withDefault [ text "" ])

                resultEstimateText =
                    if paginationInfo.isTotalEstimated then
                        translate language translations.aboutResults ++ " " ++ String.fromInt paginationInfo.totalResults ++ " " ++ translate language translations.results

                    else
                        String.fromInt paginationInfo.totalResults ++ " " ++ translate language translations.results
            in
            if List.isEmpty resultIds then
                text (translate language translations.noResultsFound)

            else
                div [ class "c-search-results__wrapper" ]
                    [ div [ class "c-search-results__header" ]
                        [ h3 [ class "c-search-results__title" ] [ text resultEstimateText ]
                        , viewSort language activeSort
                        ]
                    , ul [ class "c-search-results__list" ] (List.map (div [ class "c-search-results__item" ]) resultTemplates)
                    ]

        Error errorMsg ->
            div [ class "c-search-results__error" ] [ text (translate language translations.errorPrefix ++ errorMsg) ]


viewSort : Language -> Maybe String -> Html Msg
viewSort language activeSort =
    let
        sortOptions =
            [ Nothing, Just "pivot.title", Just "-rsc.modified", Just "-rsc.created" ]

        sortTranslation option_ =
            case option_ of
                Nothing ->
                    translate language translations.sortRelevance

                Just "pivot.title" ->
                    translate language translations.sortTitle

                Just "-rsc.modified" ->
                    translate language translations.sortModified

                Just "-rsc.created" ->
                    translate language translations.sortCreated

                _ ->
                    ""
    in
    div [ class "c-sort" ]
        [ select [ class "c-sort__select", onChange ChangeSort ]
            (List.map
                (\option_ ->
                    option
                        [ value (option_ |> Maybe.withDefault "relevance")
                        , selected (option_ == activeSort)
                        ]
                        [ text (sortTranslation option_) ]
                )
                sortOptions
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ searchPageReply SearchPageReply
        , connected CotonicReady
        , screenResized ScreenResized
        ]


onChange : (String -> msg) -> Attribute msg
onChange toMsg =
    on "change" (Decode.at [ "target", "value" ] Decode.string |> Decode.map toMsg)
