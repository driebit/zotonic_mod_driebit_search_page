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
    , pageLength : Int
    }


fromJson : Decoder Flags
fromJson =
    Decode.map6 Flags
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


defaultFlags : Flags
defaultFlags =
    { filters = []
    , excludeCategories = []
    , language = Translations.NL
    , screenWidth = 800
    , queryString = Nothing
    , pageLength = 20
    }
