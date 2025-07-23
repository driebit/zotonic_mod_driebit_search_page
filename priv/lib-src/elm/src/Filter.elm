module Filter exposing (..)

import Collapse exposing (Collapse)
import DateDisplayMode exposing (DateDisplayMode)
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
    | Date DateFilter
    | UnknownFilter


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


type alias DateFilter =
    { name : String
    , displayMode : DateDisplayMode
    , id : String
    , property : String
    , collapse : Collapse
    }


type Msg
    = DisplayModeMsg DisplayMode.Msg
    | DateDisplayModeMsg DateDisplayMode.Msg
    | CollapseMsg Collapse.Msg


update : Msg -> Filter -> Filter
update msg filter =
    case filter of
        Category categoryFilter ->
            case msg of
                DisplayModeMsg displayModeMsg ->
                    Category { categoryFilter | displayMode = DisplayMode.update displayModeMsg categoryFilter.displayMode }

                _ ->
                    filter

        Object objectFilter ->
            case msg of
                DisplayModeMsg displayModeMsg ->
                    Object { objectFilter | displayMode = DisplayMode.update displayModeMsg objectFilter.displayMode }

                _ ->
                    filter

        Date dateFilter ->
            case msg of
                DateDisplayModeMsg dateDisplayModeMsg ->
                    Date { dateFilter | displayMode = DateDisplayMode.update dateDisplayModeMsg dateFilter.displayMode }

                _ ->
                    filter

        UnknownFilter ->
            filter


fromJson : Decoder Filter
fromJson =
    Decode.oneOf
        [ decodeTextualFilter
        , decodeDateFilter
        ]


decodeTextualFilter : Decoder Filter
decodeTextualFilter =
    Decode.map6 toFilter
        (Decode.field "type" Decode.string)
        (Decode.field "title" Decode.string)
        (Decode.field "display_mode" Decode.value)
        (Decode.field "collapse" Collapse.fromJson)
        (Decode.field "options" (Decode.list Resource.fromJson))
        (Decode.field "name" Decode.string)


decodeDateFilter : Decoder Filter
decodeDateFilter =
    Decode.map6 toDateFilter
        (Decode.field "type" Decode.string)
        (Decode.field "filter_title" Decode.string)
        (Decode.field "displaymode" DateDisplayMode.fromJson)
        (Decode.field "collapse" Collapse.fromJson)
        (Decode.field "name" Decode.string)
        (Decode.field "date_prop" Decode.string)


toFilter : String -> String -> Decode.Value -> Collapse -> List Resource -> String -> Filter
toFilter type_ name displayModeEncoded collapse options id =
    let
        maybeDisplayMode =
            Decode.decodeValue (DisplayMode.fromJson id options) displayModeEncoded |> Result.toMaybe
    in
    case maybeDisplayMode of
        Just displayMode ->
            case type_ of
                "category_filter" ->
                    Category { name = name, displayMode = displayMode, collapse = collapse, options = options, id = id }

                "object_filter" ->
                    Object { name = name, displayMode = displayMode, collapse = collapse, options = options, id = id }

                _ ->
                    UnknownFilter

        Nothing ->
            UnknownFilter


toDateFilter : String -> String -> DateDisplayMode -> Collapse -> String -> String -> Filter
toDateFilter type_ title displayMode collapse name property =
    case type_ of
        "date_filter" ->
            Date { id = name, displayMode = displayMode, name = title, property = property, collapse = collapse }

        _ ->
            UnknownFilter


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

        Date dateFilter ->
            Collapse.view dateFilter.collapse
                dateFilter.name
                (Html.map DateDisplayModeMsg (DateDisplayMode.view dateFilter.displayMode))

        UnknownFilter ->
            text ""


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

        Date dateFilter ->
            []

        UnknownFilter ->
            []
