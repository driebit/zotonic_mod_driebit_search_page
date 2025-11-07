module Filter.TextualComponent.Checkboxes exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)
import Json.Decode as Decode exposing (maybe)
import Json.Encode as Encode exposing (Value)
import Resource exposing (Resource)
import Set exposing (Set)
import Translations exposing (Language, translate, translations)


type alias Model =
    { selectedResources : Set Int
    , options : List Resource
    }


init : List Resource -> Model
init options =
    { selectedResources = Set.empty
    , options = options
    }


type Msg
    = ResourceSelected Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        ResourceSelected selectedResourceId ->
            let
                newSelectedResources =
                    if Set.member selectedResourceId model.selectedResources then
                        Set.remove selectedResourceId model.selectedResources

                    else
                        Set.insert selectedResourceId model.selectedResources
            in
            { model | selectedResources = newSelectedResources }


view : Language -> Model -> Html Msg
view language { selectedResources, options } =
    fieldset [ class "c-checkboxes" ]
        (List.map
            (\resource ->
                let
                    isSelected =
                        Set.member resource.id selectedResources
                in
                div [ class "c-checkboxes__item" ]
                    [ input [ class "c-checkboxes__checkbox", type_ "checkbox", checked isSelected, onCheck (\_ -> ResourceSelected resource.id), id (String.fromInt resource.id) ] []
                    , label [ class "c-checkboxes__label" ] [ span [] [ text resource.title ] ]
                    ]
            )
            options
        )


encodedValue : String -> Maybe String -> Model -> List ( String, Value )
encodedValue filterProp maybePredicate model =
    if Set.isEmpty model.selectedResources then
        []

    else
        [ ( filterProp
          , case maybePredicate of
                Just predicate ->
                    model.selectedResources
                        |> Set.toList
                        |> List.map (\id -> [ String.fromInt id, predicate ])
                        |> Encode.list (Encode.list Encode.string)

                Nothing ->
                    model.selectedResources
                        |> Set.toList
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
    { model | selectedResources = Set.fromList decodedIds }


selectedIds : Model -> List Int
selectedIds model =
    model.selectedResources
        |> Set.toList
        |> List.sort


setSelectedIds : List Int -> Model -> Model
setSelectedIds ids model =
    { model | selectedResources = Set.fromList ids }


isSet : Model -> Bool
isSet model =
    not <| Set.isEmpty model.selectedResources
