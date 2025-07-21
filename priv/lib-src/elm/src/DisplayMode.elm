module DisplayMode exposing (..)

import DisplayMode.Checkboxes as Checkboxes
import DisplayMode.Dropdown as Dropdown
import DisplayMode.Multiselect as Multiselect
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Resource exposing (Resource)


type DisplayMode
    = Dropdown Dropdown.Model
    | Checkboxes Checkboxes.Model
    | MultiSelect Multiselect.Model


type Msg
    = DropdownMsg Dropdown.Msg
    | CheckboxMsg Checkboxes.Msg
    | MultiSelectMsg Multiselect.Msg


update : Msg -> DisplayMode -> DisplayMode
update msg displayMode =
    case msg of
        DropdownMsg dropdownMsg ->
            case displayMode of
                Dropdown model ->
                    Dropdown (Dropdown.update dropdownMsg model)

                _ ->
                    displayMode

        CheckboxMsg checkboxesMsg ->
            case displayMode of
                Checkboxes model ->
                    Checkboxes (Checkboxes.update checkboxesMsg model)

                _ ->
                    displayMode

        MultiSelectMsg multiselectMsg ->
            case displayMode of
                MultiSelect model ->
                    MultiSelect (Multiselect.update multiselectMsg model)

                _ ->
                    displayMode


fromJson : String -> List Resource -> Decoder DisplayMode
fromJson id options =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "dropdown" ->
                        Decode.succeed (Dropdown (Dropdown.init id options))

                    "checkboxes" ->
                        Decode.succeed (Checkboxes (Checkboxes.init options))

                    "multiselect" ->
                        Decode.succeed (MultiSelect (Multiselect.init options))

                    "default" ->
                        Decode.succeed (Dropdown (Dropdown.init id options))

                    _ ->
                        Decode.fail ("Unknown display mode: " ++ str)
            )


view : DisplayMode -> Html Msg
view displayMode =
    case displayMode of
        Dropdown dropdownModel ->
            Html.map DropdownMsg (Dropdown.view dropdownModel)

        Checkboxes model ->
            Html.map CheckboxMsg (Checkboxes.view model)

        MultiSelect model ->
            Html.map MultiSelectMsg (Multiselect.view model)
