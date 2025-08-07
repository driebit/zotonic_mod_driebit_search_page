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


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { filters : Dict String Filter
    , fullTextSearchQuery : String
    , results : SearchResult
    , templateCache : Dict Int (List (Html Msg))
    , page : Int
    , sortBy : String
    , language : Translations.Language
    , showFilters : Collapse
    , excludedCategories : List String
    , pagination : Pagination.Model
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
        { filters, language, screenWidth, excludeCategories } =
            Decode.decodeValue Flags.fromJson flags
                |> Result.withDefault Flags.defaultFlags

        filterDict =
            List.map (\filter -> ( filter.id, filter )) filters
                |> Dict.fromList
    in
    ( { filters = filterDict
      , results = WaitingForConnection
      , fullTextSearchQuery = ""
      , templateCache = Dict.empty
      , page = 1
      , sortBy = "-rsc.modified"
      , language = language
      , showFilters = Collapse.fromPageWidth screenWidth
      , excludedCategories = excludeCategories
      , pagination = Pagination.init
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | FilterMsg String Filter.Msg
    | SearchPageReply Decode.Value
    | FullTextSearchInput String
    | CotonicReady Bool
    | ChangeSort String
    | ChangePage Int
    | ScreenResized Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FilterMsg id filterMsg ->
            let
                updatedFilters =
                    Dict.update id
                        (Maybe.map (Filter.update filterMsg))
                        model.filters

                updatedModel =
                    { model | filters = updatedFilters, page = 1 }
            in
            ( updatedModel, searchPageCall (encodedSearchParams updatedModel) )

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
                updatedModel =
                    { model | fullTextSearchQuery = query, page = 1 }
            in
            ( updatedModel
            , searchPageCall (encodedSearchParams updatedModel)
            )

        CotonicReady _ ->
            ( { model | results = Loading }, searchPageCall (encodedSearchParams model) )

        ChangePage pageNumber ->
            let
                updatedModel =
                    { model | page = pageNumber }
            in
            ( updatedModel
            , searchPageCall (encodedSearchParamsWithPage updatedModel)
            )

        ChangeSort newSort ->
            let
                updatedModel =
                    { model | sortBy = newSort, page = 1 }
            in
            ( updatedModel
            , searchPageCall (encodedSearchParams updatedModel)
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
    model.filters
        |> Dict.toList
        |> List.concatMap (\( _, filter ) -> Filter.toSearchParams filter)
        |> List.append [ ( "text", Encode.string model.fullTextSearchQuery ) ]
        |> List.append [ ( "sort", Encode.string model.sortBy ) ]
        |> List.append [ ( "cat_exclude", Encode.list Encode.string model.excludedCategories ) ]
        |> Cotonic.searchPageTopic
        |> Cotonic.toJson


encodedSearchParamsWithPage : Model -> Decode.Value
encodedSearchParamsWithPage model =
    model.filters
        |> Dict.toList
        |> List.concatMap (\( _, filter ) -> Filter.toSearchParams filter)
        |> List.append [ ( "text", Encode.string model.fullTextSearchQuery ) ]
        |> List.append [ ( "sort", Encode.string model.sortBy ) ]
        |> List.append [ ( "cat_exclude", Encode.list Encode.string model.excludedCategories ) ]
        |> List.append [ ( "page", Encode.int model.page ) ]
        |> Cotonic.searchPageTopic
        |> Cotonic.toJson



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
                        (\( id, filter ) ->
                            Html.map (FilterMsg id) (Filter.view model.language filter)
                        )
                        (Dict.toList model.filters)
                    )
                )
            ]
        , div [ class "c-search-results" ]
            [ viewResults model.language model.results model.templateCache model.sortBy
            , div [ class "c-pagination" ]
                [ Pagination.view model.language model.pagination ChangePage ]
            ]
        ]


viewResults : Language -> SearchResult -> Dict Int (List (Html Msg)) -> String -> Html Msg
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
            in
            if List.isEmpty resultIds then
                text (translate language translations.noResultsFound)

            else
                div [ class "c-search-results__wrapper" ]
                    [ div [ class "c-search-results__header" ]
                        [ h3 [ class "c-search-results__title" ] [ text (String.fromInt paginationInfo.totalResults ++ " " ++ translate language translations.results) ]
                        , viewSort language activeSort
                        ]
                    , ul [ class "c-search-results__list" ] (List.map (div [ class "c-search-results__item" ]) resultTemplates)
                    ]

        Error errorMsg ->
            div [ class "c-search-results__error" ] [ text (translate language translations.errorPrefix ++ errorMsg) ]


viewSort : Language -> String -> Html Msg
viewSort language activeSort =
    let
        sortOptions =
            [ "pivot.title", "-rsc.modified", "-rsc.created" ]

        sortTranslation option_ =
            case option_ of
                "pivot.title" ->
                    translate language translations.sortTitle

                "-rsc.modified" ->
                    translate language translations.sortModified

                "-rsc.created" ->
                    translate language translations.sortCreated

                _ ->
                    ""
    in
    div [ class "c-sort" ]
        [ select [ class "c-sort__select", onChange ChangeSort ]
            (List.map
                (\option_ ->
                    option
                        [ value option_
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
