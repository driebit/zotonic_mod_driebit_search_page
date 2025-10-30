module Filter.TextualComponent exposing (..)

import Filter.TextualComponent.Checkboxes as Checkboxes
import Filter.TextualComponent.Dropdown as Dropdown
import Filter.TextualComponent.Multiselect as Multiselect
import Html exposing (..)
import Html.Attributes exposing (..)
import List
import Json.Decode as Decode exposing (Decoder)
import Resource exposing (Resource)
import Translations exposing (Language)


type TextualComponent
    = Dropdown Dropdown.Model
    | Checkboxes Checkboxes.Model
    | MultiSelect Multiselect.Model


type Msg
    = DropdownMsg Dropdown.Msg
    | CheckboxesMsg Checkboxes.Msg
    | MultiselectMsg Multiselect.Msg


type Effect
    = MultiselectEffect Multiselect.Effect


update : Msg -> TextualComponent -> ( TextualComponent, List Effect )
update msg displayMode =
    case ( msg, displayMode ) of
        ( DropdownMsg dropdownMsg, Dropdown model ) ->
            ( Dropdown (Dropdown.update dropdownMsg model), [] )

        ( CheckboxesMsg checkboxesMsg, Checkboxes model ) ->
            ( Checkboxes (Checkboxes.update checkboxesMsg model), [] )

        ( MultiselectMsg multiselectMsg, MultiSelect model ) ->
            let
                ( updatedModel, effects ) =
                    Multiselect.update multiselectMsg model
            in
            ( MultiSelect updatedModel, List.map MultiselectEffect effects )

        _ ->
            ( displayMode, [] )


initialize : TextualComponent -> ( TextualComponent, List Effect )
initialize displayMode =
    case displayMode of
        MultiSelect model ->
            let
                ( updatedModel, effects ) =
                    Multiselect.initialize model
            in
            ( MultiSelect updatedModel, List.map MultiselectEffect effects )

        _ ->
            ( displayMode, [] )


fromJson : Decoder (String -> List Resource -> Bool -> TextualComponent)
fromJson =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "dropdown" ->
                        Decode.succeed (\id resources _ -> Dropdown (Dropdown.init id resources))

                    "checkboxes" ->
                        Decode.succeed (\_ resources _ -> Checkboxes (Checkboxes.init resources))

                    "multiselect" ->
                        Decode.succeed (\_ resources hasMore -> MultiSelect (Multiselect.init resources hasMore))

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
