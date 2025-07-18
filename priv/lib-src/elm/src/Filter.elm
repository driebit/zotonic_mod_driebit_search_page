module Filter exposing (..)

import Collapse exposing (Collapse)
import DisplayMode exposing (DisplayMode)
import DisplayMode.Dropdown
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List exposing (drop)
import Resource exposing (Resource)


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


update : Msg -> Filter -> Filter
update msg filter =
    case filter of
        Category categoryFilter ->
            case msg of
                DisplayModeMsg displayModeMsg ->
                    Category { categoryFilter | displayMode = DisplayMode.update displayModeMsg categoryFilter.displayMode }

        Object objectFilter ->
            case msg of
                DisplayModeMsg displayModeMsg ->
                    Object { objectFilter | displayMode = DisplayMode.update displayModeMsg objectFilter.displayMode }


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


view filter =
    Html.map DisplayModeMsg <|
        case filter of
            Category categoryFilter ->
                DisplayMode.view categoryFilter.displayMode

            Object objectFilter ->
                DisplayMode.view objectFilter.displayMode


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

                _ ->
                    []

        Object _ ->
            []
