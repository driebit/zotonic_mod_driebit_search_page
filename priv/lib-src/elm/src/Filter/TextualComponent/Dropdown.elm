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
