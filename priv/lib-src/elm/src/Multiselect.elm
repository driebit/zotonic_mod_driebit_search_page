module Multiselect exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Time exposing (Month(..))


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { selected : List String
    , options : List String
    , filteredOptions : List String
    , searchQuery : String
    , isOpen : Bool
    }


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        dummyOptions =
            [ "Option 1"
            , "Option 2"
            , "Option 3"
            , "Option 4"
            ]

        initialModel =
            { selected = dummyOptions
            , options = []
            , searchQuery = ""
            , isOpen = False
            , filteredOptions = dummyOptions
            }
    in
    ( initialModel, Cmd.none )


type Msg
    = NoOp
    | Select String
    | ToggleDropdown
    | SearchInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Select option ->
            if List.member option model.selected then
                ( { model | selected = List.filter ((/=) option) model.selected }, Cmd.none )

            else
                ( { model | selected = option :: model.selected }, Cmd.none )

        ToggleDropdown ->
            ( { model | isOpen = not model.isOpen }, Cmd.none )

        SearchInput query ->
            let
                filteredOptions =
                    if String.isEmpty query then
                        model.options

                    else
                        List.filter (\option -> String.contains query option) model.options
            in
            ( { model | searchQuery = query, filteredOptions = filteredOptions }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Search...", onInput SearchInput ] []
        , button [ onClick ToggleDropdown ] [ text "Toggle Dropdown" ]
        , if model.isOpen then
            ul []
                (List.map
                    viewOption
                    model.filteredOptions
                )

          else
            text ""
        ]


viewOption : String -> Html Msg
viewOption option =
    li [ onClick (Select option) ]
        [ text option ]
