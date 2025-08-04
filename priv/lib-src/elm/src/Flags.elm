module Flags exposing (..)

import Filter exposing (Filter)
import Json.Decode as Decode exposing (Decoder)
import Translations exposing (Language)


type alias Flags =
    { filters : List Filter
    , language : Language
    , screenWidth : Int
    }


fromJson : Decoder Flags
fromJson =
    Decode.map3 Flags
        (Decode.field "blocks" (Decode.list Filter.fromJson))
        (Decode.field "language" Translations.languageFromJson)
        (Decode.field "screenWidth" Decode.int)


defaultFlags : Flags
defaultFlags =
    { filters = []
    , language = Translations.NL
    , screenWidth = 800
    }
