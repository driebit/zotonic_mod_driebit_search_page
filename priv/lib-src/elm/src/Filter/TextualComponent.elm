module Filter.TextualComponent exposing (..)

import Filter.TextualComponent.Checkboxes as Checkboxes
import Filter.TextualComponent.Dropdown as Dropdown
import Filter.TextualComponent.Multiselect as Multiselect
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (custom)
import Json.Decode as Decode exposing (Decoder)
import List
import Resource exposing (Resource)
import Set
import Translations exposing (Language)


type TextualComponent
    = Dropdown Dropdown.Model
    | Checkboxes Checkboxes.Model
    | MultiSelect Multiselect.Model


type Msg
    = DropdownMsg Dropdown.Msg
    | CheckboxesMsg Checkboxes.Msg
    | MultiselectMsg Multiselect.Msg


update : Msg -> TextualComponent -> TextualComponent
update msg displayMode =
    case ( msg, displayMode ) of
        ( DropdownMsg dropdownMsg, Dropdown model ) ->
            Dropdown (Dropdown.update dropdownMsg model)

        ( CheckboxesMsg checkboxesMsg, Checkboxes model ) ->
            Checkboxes (Checkboxes.update checkboxesMsg model)

        ( MultiselectMsg multiselectMsg, MultiSelect model ) ->
            MultiSelect (Multiselect.update multiselectMsg model)

        _ ->
            displayMode


fromJson : Decoder (String -> List Resource -> TextualComponent)
fromJson =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "dropdown" ->
                        Decode.succeed (\id resources -> Dropdown (Dropdown.init id resources))

                    "checkboxes" ->
                        Decode.succeed (\_ resources -> Checkboxes (Checkboxes.init resources))

                    "multiselect" ->
                        Decode.succeed (\_ resources -> MultiSelect (Multiselect.init resources))

                    _ ->
                        Decode.fail ("Unknown display mode: " ++ str)
            )


view : Language -> TextualComponent -> Html Msg
view language displayMode =
    case displayMode of
        Dropdown dropdownModel ->
            Html.map DropdownMsg (Dropdown.view language dropdownModel)

        Checkboxes model ->
            Html.map CheckboxesMsg (Checkboxes.view language model)

        MultiSelect model ->
            Html.map MultiselectMsg (Multiselect.view language model)


encodedValue : String -> Maybe String -> TextualComponent -> List ( String, Decode.Value )
encodedValue filterProp maybePredicate component =
    case component of
        Dropdown model ->
            Dropdown.encodedValue filterProp maybePredicate model

        Checkboxes model ->
            Checkboxes.encodedValue filterProp maybePredicate model

        MultiSelect model ->
            Multiselect.encodedValue filterProp maybePredicate model


summaryView : TextualComponent -> Maybe (Html Msg)
summaryView component =
    case component of
        MultiSelect model ->
            let
                selections =
                    model.options
                        |> List.filter (\resource -> List.member resource.id model.selected)
            in
            if List.isEmpty selections then
                Nothing

            else
                Just <|
                    div [ class "c-multiselect__selected" ]
                        (List.map
                            (\resource ->
                                pill resource.title (MultiselectMsg (Multiselect.Select resource.id))
                            )
                            selections
                        )

        _ ->
            Nothing


pill : String -> Msg -> Html Msg
pill title toMsg =
    let
        stopClick =
            custom "click" (Decode.succeed { message = toMsg, stopPropagation = True, preventDefault = True })
    in
    span [ class "c-multiselect__pill", stopClick ]
        [ text title
        , span [ class "c-multiselect__remove" ] [ text "×" ]
        ]
