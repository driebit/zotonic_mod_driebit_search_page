module TextualComponent.Multiselect exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Resource exposing (Resource)
import Time exposing (Month(..))


type alias Model =
    { selected : List Int
    , options : List Resource
    , filteredOptions : List Resource
    , searchQuery : String
    , isOpen : Bool
    , numberOfResultsVisible : Int
    }


init : List Resource -> Model
init options =
    let
        initialModel =
            { selected = []
            , options = options
            , searchQuery = ""
            , isOpen = False
            , filteredOptions = options
            , numberOfResultsVisible = 5
            }
    in
    initialModel


type Msg
    = NoOp
    | Select Int
    | ToggleDropdown
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

        ToggleDropdown ->
            { model | isOpen = not model.isOpen }

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


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Search...", onInput SearchInput ] []
        , button [ onClick ToggleDropdown ] [ text "open" ]
        , if model.isOpen then
            div []
                [ ul []
                    (List.take model.numberOfResultsVisible model.filteredOptions
                        |> List.map (viewOption model.selected)
                    )
                , if List.length model.filteredOptions > model.numberOfResultsVisible then
                    button [ onClick ShowMoreResults ] [ text "Show More" ]

                  else
                    text ""
                ]

          else
            text ""
        ]


viewOption : List Int -> Resource -> Html Msg
viewOption selectedOptions option =
    let
        isSelected =
            List.member option.id selectedOptions
    in
    li
        [ onClick (Select option.id)
        , style "background-color"
            (if isSelected then
                "lightblue"

             else
                "white"
            )
        ]
        [ input [ type_ "checkbox", checked isSelected, id (String.fromInt option.id) ] []
        , label [ for (String.fromInt option.id) ] [ text option.title ]
        ]


encodedValue : Maybe String -> Model -> Maybe Encode.Value
encodedValue maybePredicate model =
    let
        encodeList predicate =
            model.selected
                |> List.map (\id -> [ predicate, String.fromInt id ])
                |> Encode.list (Encode.list Encode.string)
    in
    if List.isEmpty model.selected then
        Nothing

    else
        case maybePredicate of
            Just predicate ->
                Just (encodeList predicate)

            Nothing ->
                model.selected
                    |> List.map String.fromInt
                    |> Encode.list Encode.string
                    |> Just
