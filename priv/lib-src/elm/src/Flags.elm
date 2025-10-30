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
    , queryParams : List ( String, String )
    }


fromJson : Decoder Flags
fromJson =
    Decode.map7 Flags
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
            [ Decode.field "queryParams" (Decode.list queryParamDecoder)
            , Decode.succeed []
            ]
        )


queryParamDecoder : Decoder ( String, String )
queryParamDecoder =
    Decode.map2 Tuple.pair
        (Decode.index 0 Decode.string)
        (Decode.index 1 Decode.string)


defaultFlags : Flags
defaultFlags =
    { filters = []
    , excludeCategories = []
    , language = Translations.NL
    , screenWidth = 800
    , queryString = Nothing
    , pageLength = 20
    , queryParams = []
    }
