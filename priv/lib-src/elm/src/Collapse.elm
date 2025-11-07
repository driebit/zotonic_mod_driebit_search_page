module Collapse exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)


type Collapse
    = Collapsabable CollapseState
    | NotCollapsible


fromPageWidth : Int -> Collapse
fromPageWidth width =
    if width < 800 then
        Collapsabable Closed

    else
        NotCollapsible


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

                NotCollapsible ->
                    NotCollapsible


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

        NotCollapsible ->
            div [ class "c-collapse--uncollapsible" ]
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

                    "uncollapsible" ->
                        Decode.succeed NotCollapsible

                    _ ->
                        Decode.fail ("Unknown collapse state: " ++ str)
            )


open : Collapse
open =
    Collapsabable Open


closed : Collapse
closed =
    Collapsabable Closed


openIf : Bool -> Collapse
openIf condition =
    if condition then
        open

    else
        closed
