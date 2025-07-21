module Filter exposing (..)

import Collapse exposing (Collapse)
import DisplayMode exposing (DisplayMode)
import DisplayMode.Dropdown
import Html exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Resource exposing (Resource)
import Set


type Filter
    = Category CategoryFilter
    | Object ObjectFilter


type alias CategoryFilter =
    { name : String
    , displayMode : DisplayMode
    , collapse : Collapse
    , options : List Resource
    , id : String
    }


type alias ObjectFilter =
    { name : String
    , displayMode : DisplayMode
    , collapse : Collapse
    , options : List Resource
    , id : String
    }


type Msg
    = DisplayModeMsg DisplayMode.Msg
    | CollapseMsg Collapse.Msg


update : Msg -> Filter -> Filter
update msg filter =
    case filter of
        Category categoryFilter ->
            case msg of
                DisplayModeMsg displayModeMsg ->
                    Category { categoryFilter | displayMode = DisplayMode.update displayModeMsg categoryFilter.displayMode }

                CollapseMsg collapseMsg ->
                    Category { categoryFilter | collapse = Collapse.update collapseMsg categoryFilter.collapse }

        Object objectFilter ->
            case msg of
                DisplayModeMsg displayModeMsg ->
                    Object { objectFilter | displayMode = DisplayMode.update displayModeMsg objectFilter.displayMode }

                CollapseMsg collapseMsg ->
                    Object { objectFilter | collapse = Collapse.update collapseMsg objectFilter.collapse }


fromJson : Decoder Filter
fromJson =
    Decode.map6 toFilter
        (Decode.field "type" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "display_mode" Decode.value)
        (Decode.field "collapse" Collapse.fromJson)
        (Decode.field "options" (Decode.list Resource.fromJson))
        (Decode.field "name" Decode.string)


toFilter : String -> String -> Decode.Value -> Collapse -> List Resource -> String -> Filter
toFilter type_ name displayModeEncoded collapse options id =
    let
        displayMode =
            case Decode.decodeValue (DisplayMode.fromJson id options) displayModeEncoded of
                Ok dm ->
                    dm

                Err _ ->
                    DisplayMode.Dropdown (DisplayMode.Dropdown.init id options)
    in
    case type_ of
        "category_filter" ->
            Category { name = name, displayMode = displayMode, collapse = collapse, options = options, id = id }

        "object_filter" ->
            Object { name = name, displayMode = displayMode, collapse = collapse, options = options, id = id }

        _ ->
            Category { name = name, displayMode = displayMode, collapse = collapse, options = options, id = id }


view : Filter -> Html Msg
view filter =
    case filter of
        Category categoryFilter ->
            Collapse.view categoryFilter.collapse
                categoryFilter.name
                (Html.map DisplayModeMsg (DisplayMode.view categoryFilter.displayMode))

        Object objectFilter ->
            Collapse.view objectFilter.collapse
                objectFilter.name
                (Html.map DisplayModeMsg (DisplayMode.view objectFilter.displayMode))


toSearchParams : Filter -> List ( String, Encode.Value )
toSearchParams filter =
    case filter of
        Category categoryFilter ->
            case categoryFilter.displayMode of
                DisplayMode.Dropdown dropdownModel ->
                    case dropdownModel.selectedResource of
                        Just resource ->
                            [ ( "cat", Encode.int resource.id ) ]

                        Nothing ->
                            []

                DisplayMode.Checkboxes checkboxesModel ->
                    let
                        selectedIds =
                            Set.toList checkboxesModel.selectedResources
                                |> List.map String.fromInt
                    in
                    if List.isEmpty selectedIds then
                        []

                    else
                        [ ( "cat", Encode.list Encode.string selectedIds ) ]

                DisplayMode.MultiSelect multiselectModel ->
                    if List.isEmpty multiselectModel.selected then
                        []

                    else
                        [ ( "cat", Encode.list Encode.int multiselectModel.selected ) ]

        Object objectFilter ->
            case objectFilter.displayMode of
                DisplayMode.Dropdown dropdownModel ->
                    case dropdownModel.selectedResource of
                        Just resource ->
                            [ ( "hasobject", Encode.int resource.id ) ]

                        Nothing ->
                            []

                DisplayMode.Checkboxes checkboxesModel ->
                    let
                        selectedIds =
                            Set.toList checkboxesModel.selectedResources
                                |> List.map String.fromInt
                    in
                    if List.isEmpty selectedIds then
                        []

                    else
                        [ ( "hasanyobject", Encode.list Encode.string selectedIds ) ]

                DisplayMode.MultiSelect multiselectModel ->
                    if List.isEmpty multiselectModel.selected then
                        []

                    else
                        [ ( "hasanyobject", Encode.list Encode.int multiselectModel.selected ) ]
