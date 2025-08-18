module Collapse exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)


type Collapse
    = Collapsabable CollapseState
    | NotCollapsable


fromPageWidth : Int -> Collapse
fromPageWidth width =
    if width < 800 then
        Collapsabable Closed

    else
        NotCollapsable


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


view : Collapse -> Html msg -> Html msg -> Html msg
view collapse summary_ content =
    case collapse of
        Collapsabable state ->
            details
                (class "c-collapse"
                    :: (if state == Open then
                            [ Html.Attributes.attribute "open" "true" ]

                        else
                            []
                       )
                )
                [ summary [ class "c-collapse__summary" ] [ summary_ ]
                , div [ class "c-collapse__content" ] [ content ]
                ]

        NotCollapsable ->
            div [ class "c-collapse--uncollapsable" ]
                [ h3 [ class "c-collapse__title" ] [ summary_ ]
                , div [ class "c-collapse__content" ] [ content ]
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
