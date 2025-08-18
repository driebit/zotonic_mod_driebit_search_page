module Flags exposing (..)

import Filter exposing (Filter)
import Json.Decode as Decode exposing (Decoder)
import Translations exposing (Language)


type alias Flags =
    { filters : List Filter
    , excludeCategories : List String
    , language : Language
    , screenWidth : Int
    , queryString : Maybe String
    }


fromJson : Decoder Flags
fromJson =
    Decode.map5 Flags
        (Decode.at [ "blocks", "filters" ] (Decode.list Filter.fromJson))
        (Decode.at [ "blocks", "exclude_categories" ] (Decode.list Decode.string))
        (Decode.field "language" Translations.languageFromJson)
        (Decode.field "screenWidth" Decode.int)
        (Decode.oneOf
            [ Decode.map Just (Decode.field "qs" Decode.string)
            , Decode.succeed Nothing
            ]
        )


defaultFlags : Flags
defaultFlags =
    { filters = []
    , excludeCategories = []
    , language = Translations.NL
    , screenWidth = 800
    , queryString = Nothing
    }
