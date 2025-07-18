module DisplayMode exposing (..)

import DisplayMode.Dropdown as Dropdown
import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Resource exposing (Resource)


type DisplayMode
    = Dropdown Dropdown.Model
    | Checkboxes
    | MultiSelect


type Msg
    = DropdownMsg Dropdown.Msg
    | CheckboxMsg
    | MultiSelectMsg


update : Msg -> DisplayMode -> DisplayMode
update msg displayMode =
    case msg of
        DropdownMsg dropdownMsg ->
            case displayMode of
                Dropdown model ->
                    Dropdown (Dropdown.update dropdownMsg model)

                _ ->
                    displayMode

        CheckboxMsg ->
            Checkboxes

        MultiSelectMsg ->
            MultiSelect


fromJson : String -> List Resource -> Decoder DisplayMode
fromJson id options =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "dropdown" ->
                        Decode.succeed (Dropdown (Dropdown.init id options))

                    "checkboxes" ->
                        Decode.succeed Checkboxes

                    "multiselect" ->
                        Decode.succeed MultiSelect

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

        Checkboxes ->
            div [] [ text "Checkboxes not implemented yet" ]

        MultiSelect ->
            div [] [ text "MultiSelect not implemented yet" ]
