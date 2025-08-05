module Resource exposing (..)

import Json.Decode as Decode exposing (Decoder)


type alias Resource =
    { title : String
    , id : Int
    }


fromJson : Decoder Resource
fromJson =
    Decode.map2 Resource
        (Decode.field "title" Decode.string)
        (Decode.field "id" Decode.int)
