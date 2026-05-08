module Flags exposing (..)

import Dict exposing (Dict)
import Filter exposing (Filter)
import Json.Decode as Decode exposing (Decoder)
import Translations exposing (Language)


type alias Flags =
    { filters : List Filter
    , excludeCategories : List String
    , language : Language
    , screenWidth : Int
    , queryString : Maybe String
    , pageLength : Int
    , defaultSort : Maybe String
    , queryParams : Dict String String
    }


fromJson : Decoder Flags
fromJson =
    Decode.map8 Flags
        (Decode.at [ "blocks", "filters" ] (Decode.list Filter.fromJson))
        (Decode.at [ "blocks", "exclude_categories" ] (Decode.list Decode.string))
        (Decode.field "language" Translations.languageFromJson)
        (Decode.field "screenWidth" Decode.int)
        (Decode.oneOf
            [ Decode.map Just (Decode.field "qs" Decode.string)
            , Decode.succeed Nothing
            ]
        )
        (Decode.oneOf
            [ Decode.at [ "blocks", "pagelen" ] Decode.int
            , Decode.succeed 20
            ]
        )
        (Decode.oneOf
            [ Decode.at [ "blocks", "default_sort" ] Decode.string
                |> Decode.map defaultSortFromString
            , Decode.succeed Nothing
            ]
        )
        (Decode.oneOf
            [ Decode.field "queryParams" (Decode.dict Decode.string)
            , Decode.succeed Dict.empty
            ]
        )


defaultSortFromString : String -> Maybe String
defaultSortFromString sort =
    case sort of
        "pivot.title" ->
            Just sort

        "-rsc.modified" ->
            Just sort

        "-rsc.created" ->
            Just sort

        _ ->
            Nothing


defaultFlags : Flags
defaultFlags =
    { filters = []
    , excludeCategories = []
    , language = Translations.NL
    , screenWidth = 800
    , queryString = Nothing
    , pageLength = 20
    , defaultSort = Nothing
    , queryParams = Dict.empty
    }
