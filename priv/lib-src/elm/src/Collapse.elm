module Collapse exposing (..)

import Json.Decode as Decode exposing (Decoder)


type Collapse
    = Collapsabable CollapseState
    | NotCollapsable


type CollapseState
    = Open
    | Closed


fromJson : Decoder Collapse
fromJson =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "collapsed" ->
                        Decode.succeed (Collapsabable Closed)

                    "uncollapsed" ->
                        Decode.succeed (Collapsabable Open)

                    "not_collapsable" ->
                        Decode.succeed NotCollapsable

                    _ ->
                        Decode.fail ("Unknown collapse state: " ++ str)
            )
