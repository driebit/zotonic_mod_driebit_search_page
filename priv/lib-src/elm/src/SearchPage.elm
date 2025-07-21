port module SearchPage exposing (..)

import Browser
import Dict exposing (Dict)
import DisplayMode exposing (DisplayMode)
import Filter exposing (Filter)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Time exposing (Month(..))


port searchPageCall : Encode.Value -> Cmd msg


port searchPageReply : (Encode.Value -> msg) -> Sub msg


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
    , results : List Int
    }


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
    ( { filters = filterDict, results = [] }, Cmd.none )


type Msg
    = NoOp
    | FilterMsg String Filter.Msg
    | SearchPageReply Decode.Value


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
                        |> Encode.object
            in
            ( { model | filters = updatedFilters }, searchPageCall encodedSearchParams )

        SearchPageReply reply ->
            case Decode.decodeValue (Decode.at [ "reply", "payload", "result", "result" ] (Decode.list Decode.int)) reply of
                Ok results ->
                    ( { model | results = results }, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log "Error decoding search results" (Decode.errorToString err)
                    in
                    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "c-search" ]
        [ div [ class "c-full-text-search" ]
            [ input
                [ class "c-full-text-search__searchbar"
                , type_ "text"
                , placeholder "zoeken"
                ]
                []
            ]
        , div [ class "c-search-filters" ]
            (Dict.toList model.filters
                |> List.map (\( id, filter ) -> Html.map (FilterMsg id) (Filter.view filter))
            )
        , div [ class "c-search-results" ]
            [ if List.isEmpty model.results then
                text "No results found."

              else
                ul []
                    (List.map (\resultId -> li [] [ text ("Result ID: " ++ String.fromInt resultId) ]) model.results)
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    searchPageReply SearchPageReply


idFromFilter : Filter -> String
idFromFilter filter =
    case filter of
        Filter.Category categoryFilter ->
            categoryFilter.id

        Filter.Object objectFilter ->
            objectFilter.id
