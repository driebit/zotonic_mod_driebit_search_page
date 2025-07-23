port module SearchPage exposing (..)

import Browser
import Cotonic exposing (CotonicCall, searchPageTopic, templateTopic)
import Dict exposing (Dict)
import DisplayMode exposing (DisplayMode)
import Filter exposing (Filter)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Parser
import Html.Parser.Util
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)
import Time exposing (Month(..))


port searchPageCall : Encode.Value -> Cmd msg


port searchPageReply : (Encode.Value -> msg) -> Sub msg


port connected : (Bool -> msg) -> Sub msg


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
    }


type SearchResult
    = NotAsked
    | WaitingForConnection
    | Loading
    | Loaded (List Int)
    | Error String


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        filters =
            Decode.decodeValue (Decode.list Filter.fromJson) flags
                |> Result.mapError (\err -> Debug.log "Error decoding filters" err)
                |> Result.withDefault []

        filterDict =
            List.map (\filter -> ( idFromFilter filter, filter )) filters
                |> Dict.fromList
    in
    ( { filters = filterDict
      , results = WaitingForConnection
      , fullTextSearchQuery = ""
      , templateCache = Dict.empty
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | FilterMsg String Filter.Msg
    | SearchPageReply Decode.Value
    | FullTextSearchInput String
    | CotonicReady Bool


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

                encodedSearchParams =
                    updatedFilters
                        |> Dict.toList
                        |> List.concatMap (\( _, filter ) -> Filter.toSearchParams filter)
                        |> Cotonic.searchPageTopic
                        |> Cotonic.toJson
            in
            ( { model | filters = updatedFilters }, searchPageCall encodedSearchParams )

        SearchPageReply reply ->
            case Decode.decodeValue (Decode.field "topic" Decode.string) reply |> Result.map (String.split "/") of
                Ok [ "SearchReply" ] ->
                    case Decode.decodeValue (Decode.at [ "reply", "payload", "result", "result" ] (Decode.list Decode.int)) reply of
                        Ok results ->
                            let
                                templateCalls =
                                    results
                                        |> List.map (\id -> templateTopic id)
                                        |> List.map Cotonic.toJson
                                        |> List.map searchPageCall
                            in
                            ( { model | results = Loaded results }, Cmd.batch templateCalls )

                        Err err ->
                            ( { model | results = Error (Decode.errorToString err) }, Cmd.none )

                Ok [ "TemplateReply", idString ] ->
                    case Decode.decodeValue (Decode.at [ "reply", "payload", "result" ] Decode.string) reply of
                        Ok template ->
                            let
                                markdownOptions =
                                    { githubFlavored = Just { tables = False, breaks = False }
                                    , defaultHighlighting = Nothing
                                    , sanitize = False
                                    , smartypants = False
                                    }

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
                    { model | fullTextSearchQuery = query }

                encodedSearchFilters =
                    model.filters
                        |> Dict.toList
                        |> List.concatMap (\( _, filter ) -> Filter.toSearchParams filter)
                        |> List.append [ ( "full_text_search", Encode.string query ) ]
                        |> Cotonic.searchPageTopic
                        |> Cotonic.toJson
            in
            ( updatedModel
            , searchPageCall encodedSearchFilters
            )

        CotonicReady _ ->
            ( { model | results = Loading }, Cotonic.searchPageTopic [] |> Cotonic.toJson |> searchPageCall )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "c-search" ]
        [ div [ class "c-full-text-search" ]
            [ input
                [ class "c-full-text-search__searchbar"
                , type_ "text"
                , placeholder "zoeken"
                , value model.fullTextSearchQuery
                , onInput FullTextSearchInput
                , id "search-bar"
                ]
                []
            ]
        , div [ class "c-search-filters" ]
            (Dict.toList model.filters
                |> List.map (\( id, filter ) -> Html.map (FilterMsg id) (Filter.view filter))
            )
        , div [ class "c-search-results" ]
            [ viewResults model.results model.templateCache
            ]
        ]


viewResults : SearchResult -> Dict Int (List (Html Msg)) -> Html Msg
viewResults results templateCache =
    case results of
        NotAsked ->
            text "No search has been performed yet."

        Loading ->
            text "Loading results..."

        WaitingForConnection ->
            text "Waiting for connection..."

        Loaded resultIds ->
            let
                resultTemplates =
                    resultIds
                        |> List.map (\id -> Dict.get id templateCache |> Maybe.withDefault [ text "Template not found" ])
            in
            if List.isEmpty resultIds then
                text "No results found."

            else
                ul [ class "list" ] (List.map (div []) resultTemplates)

        Error errorMsg ->
            div [ class "error" ] [ text ("Error: " ++ errorMsg) ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ searchPageReply SearchPageReply
        , connected CotonicReady
        ]


idFromFilter : Filter -> String
idFromFilter filter =
    case filter of
        Filter.Category categoryFilter ->
            categoryFilter.id

        Filter.Object objectFilter ->
            objectFilter.id

        Filter.Date dateFilter ->
            dateFilter.id

        Filter.UnknownFilter ->
            "unknown_filter"
