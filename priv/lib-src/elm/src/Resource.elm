module Resource exposing (..)

import Json.Decode as Decode exposing (Decoder)
import String


type alias Resource =
    { title : String
    , id : Int
    }


fromJson : Decoder Resource
fromJson =
    Decode.map2 Resource
        (Decode.field "title" Decode.string)
        (Decode.field "id" decodeId)


decodeId : Decoder Int
decodeId =
    Decode.oneOf
        [ Decode.int
        , Decode.string
            |> Decode.andThen
                (\str ->
                    case String.toInt str of
                        Just intId ->
                            Decode.succeed intId

                        Nothing ->
                            Decode.fail ("Expected numeric id, got " ++ str)
                )
        ]
