module DisplayMode.Checkboxes exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)
import Resource exposing (Resource)
import Set exposing (Set)


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


view : Model -> Html Msg
view { selectedResources, options } =
    fieldset []
        [ div [ class "" ]
            (List.map
                (\resource ->
                    let
                        isSelected =
                            Set.member resource.id selectedResources
                    in
                    div []
                        [ label [ class "keyword-label" ]
                            [ input [ type_ "checkbox", checked isSelected, onCheck (\_ -> ResourceSelected resource.id), id (String.fromInt resource.id) ] []
                            , span [] [ text resource.title ]
                            ]
                        ]
                )
                options
            )
        ]
