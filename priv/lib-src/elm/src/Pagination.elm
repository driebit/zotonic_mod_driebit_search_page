module Pagination exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Set
import Translations exposing (Language, translate, translations)


type alias Model =
    { currentPage : Int
    , totalResults : Int
    , totalPages : Int
    , isTotalEstimated : Bool
    }


init : Model
init =
    { currentPage = 1
    , totalResults = 0
    , totalPages = 0
    , isTotalEstimated = False
    }


fromJson : Decoder Model
fromJson =
    Decode.map4 Model
        (Decode.field "page" Decode.int)
        (Decode.field "total" Decode.int)
        (Decode.field "pages" Decode.int)
        (Decode.field "is_total_estimated" Decode.bool)


view : Language -> Model -> (Int -> msg) -> Html msg
view language paginationInfo toPage =
    div [ class "c-pagination" ]
        [ button
            [ class "c-pagination__button c-pagination__button--arrow c-pagination__button--previous"
            , onClick (toPage (paginationInfo.currentPage - 1))
            , disabled (paginationInfo.currentPage <= 1)
            ]
            [ span [ class "u-visually-hidden" ] [ text (translate language translations.previous) ] ]
        , if paginationInfo.currentPage - 3 > 1 then
            text "..."

          else
            text ""
        , div [ class "c-pagination__buttons" ]
            (List.map (viewPaginationButton toPage paginationInfo.currentPage)
                (buttonsToShow paginationInfo)
            )
        , if paginationInfo.currentPage + 3 < paginationInfo.totalPages then
            text "..."

          else
            text ""
        , button
            [ class "c-pagination__button c-pagination__button--arrow c-pagination__button--next"
            , onClick (toPage (paginationInfo.currentPage + 1))
            , disabled (paginationInfo.currentPage >= paginationInfo.totalPages)
            ]
            [ span [ class "u-visually-hidden" ] [ text (translate language translations.next) ] ]
        ]


viewPaginationButton : (Int -> msg) -> Int -> Int -> Html msg
viewPaginationButton toPage currentPage page =
    button
        [ classList
            [ ( "c-pagination__button--active", currentPage == page )
            , ( "c-pagination__button", True )
            ]
        , onClick (toPage page)
        ]
        [ text (String.fromInt page) ]


buttonsToShow : Model -> List Int
buttonsToShow paginationInfo =
    let
        totalPages =
            paginationInfo.totalPages

        currentPage =
            paginationInfo.currentPage

        range =
            3
    in
    List.filterMap
        (\page ->
            if page > 0 && page <= totalPages then
                Just page

            else
                Nothing
        )
        (List.concatMap (\offset -> [ currentPage - offset, currentPage + offset ]) (List.range 0 range))
        |> Set.fromList
        |> Set.toList
        |> List.sort
