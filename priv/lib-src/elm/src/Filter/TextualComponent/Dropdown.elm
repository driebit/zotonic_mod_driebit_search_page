module Filter.TextualComponent.Dropdown exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import Resource exposing (Resource)
import Translations exposing (Language, translate, translations)


type alias Model =
    { selectedResource : Maybe Resource
    , options : List Resource
    , id : String
    }


init : String -> List Resource -> Model
init id options =
    { selectedResource = Nothing
    , options = options
    , id = id
    }


type Msg
    = ResourceSelected String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ResourceSelected selectedResourceId ->
            let
                selectedResource =
                    model.options
                        |> List.filter (\resource -> String.fromInt resource.id == selectedResourceId)
                        |> List.head

                newSelectedResource =
                    case model.selectedResource of
                        Just resource ->
                            if String.fromInt resource.id == selectedResourceId then
                                Nothing

                            else
                                selectedResource

                        Nothing ->
                            selectedResource

                newSelectedResource_ =
                    if selectedResourceId == "all" then
                        Nothing

                    else
                        newSelectedResource
            in
            { model | selectedResource = newSelectedResource_ }


view : Language -> Model -> Html Msg
view language { selectedResource, options } =
    div [ class "c-dropdown" ]
        [ select [ class "c-dropdown__select", onChange ResourceSelected ]
            (option [ value "all" ] [ text (translate language translations.dropdownAll) ]
                :: List.map
                    (\resource ->
                        option
                            [ value (String.fromInt resource.id)
                            , selected (Maybe.map (\r -> r.id == resource.id) selectedResource |> Maybe.withDefault False)
                            ]
                            [ text resource.title ]
                    )
                    options
            )
        ]


onChange : (String -> msg) -> Attribute msg
onChange toMsg =
    on "change" (Decode.at [ "target", "value" ] Decode.string |> Decode.map toMsg)


encodedValue : String -> Maybe String -> Model -> List ( String, Encode.Value )
encodedValue filterProp maybePredicate model =
    case maybePredicate of
        Just predicate ->
            model.selectedResource
                |> Maybe.map (\resource -> ( filterProp, Encode.list Encode.string [ predicate, String.fromInt resource.id ] ))
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        Nothing ->
                    model.selectedResource
                        |> Maybe.map (\resource -> ( filterProp, Encode.string (String.fromInt resource.id) ))
                        |> Maybe.map List.singleton
                        |> Maybe.withDefault []


selectedIds : Model -> List Int
selectedIds model =
    case model.selectedResource of
        Just resource ->
            [ resource.id ]

        Nothing ->
            []


setSelectedIds : List Int -> Model -> Model
setSelectedIds ids model =
    case ids of
        id :: _ ->
            { model | selectedResource = findResource id model.options }

        [] ->
            { model | selectedResource = Nothing }


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

        findResourceFromString idStr =
            String.toInt idStr
                |> Maybe.andThen
                    (\id ->
                        findResource id model.options
                    )
    in
    case maybePredicate of
        Just predicate ->
            let
                maybeResource =
                    matchingValues
                        |> List.filterMap (maybeDecode (Decode.list Decode.string))
                        |> List.filterMap
                            (\parts ->
                                case parts of
                                    [ predicateValue, idStr ] ->
                                        if predicateValue == predicate then
                                            findResourceFromString idStr

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )
                        |> List.head
            in
            case maybeResource of
                Just resource ->
                    { model | selectedResource = Just resource }

                Nothing ->
                    model

        Nothing ->
            let
                maybeSelection =
                    matchingValues
                        |> List.filterMap (maybeDecode Decode.string)
                        |> List.filterMap
                            (\idStr ->
                                if idStr == "all" || String.isEmpty idStr then
                                    Just Nothing

                                else
                                    findResourceFromString idStr |> Maybe.map Just
                            )
                            |> List.head
            in
            case maybeSelection of
                Just selection ->
                    { model | selectedResource = selection }

                Nothing ->
                    model


findResource : Int -> List Resource -> Maybe Resource
findResource id options =
    options
        |> List.filter (\resource -> resource.id == id)
        |> List.head
