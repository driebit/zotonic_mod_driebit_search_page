module Collapse exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)


type Collapse
    = Collapsabable CollapseState
    | NotCollapsable


type CollapseState
    = Open
    | Closed


type Msg
    = ToggleCollapse


update : Msg -> Collapse -> Collapse
update msg collapse =
    case msg of
        ToggleCollapse ->
            case collapse of
                Collapsabable Open ->
                    Collapsabable Closed

                Collapsabable Closed ->
                    Collapsabable Open

                NotCollapsable ->
                    NotCollapsable


view : Collapse -> String -> Html msg -> Html msg
view collapse title content =
    case collapse of
        Collapsabable state ->
            details
                [ if state == Open then
                    Html.Attributes.attribute "open" "true"

                  else
                    Html.Attributes.attribute "open" "false"
                ]
                [ summary [] [ text title ]
                , content
                ]

        NotCollapsable ->
            div []
                [ h3 [] [ text title ]
                , content
                ]


fromJson : Decoder Collapse
fromJson =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "collapsed" ->
                        Decode.succeed (Collapsabable Closed)

                    "not_collapsed" ->
                        Decode.succeed (Collapsabable Open)

                    "uncollapsable" ->
                        Decode.succeed NotCollapsable

                    _ ->
                        Decode.fail ("Unknown collapse state: " ++ str)
            )
