module TextualComponent exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Resource exposing (Resource)
import TextualComponent.Checkboxes as Checkboxes
import TextualComponent.Dropdown as Dropdown
import TextualComponent.Multiselect as Multiselect


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


view : TextualComponent -> Html Msg
view displayMode =
    case displayMode of
        Dropdown dropdownModel ->
            Html.map DropdownMsg (Dropdown.view dropdownModel)

        Checkboxes model ->
            Html.map CheckboxesMsg (Checkboxes.view model)

        MultiSelect model ->
            Html.map MultiselectMsg (Multiselect.view model)


encodedValue : Maybe String -> TextualComponent -> Maybe Decode.Value
encodedValue maybePredicate component =
    case component of
        Dropdown model ->
            Dropdown.encodedValue maybePredicate model

        Checkboxes model ->
            Checkboxes.encodedValue maybePredicate model

        MultiSelect model ->
            Multiselect.encodedValue maybePredicate model
