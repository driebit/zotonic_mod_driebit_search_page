module Cotonic exposing (..)

import Json.Encode as Encode


type alias CotonicCall =
    { topic : String
    , parameters : List ( String, Encode.Value )
    , replyTopic : String
    }


type alias ParametersAcc =
    { singles : List ( String, Encode.Value )
    , duplicates : List ( String, Encode.Value )
    }


searchPageTopic : List ( String, Encode.Value ) -> CotonicCall
searchPageTopic parameters =
    { topic = "bridge/origin/model/search/get"
    , parameters = parameters
    , replyTopic = "SearchReply"
    }


filterOptionsTopic : String -> String -> Int -> Maybe String -> Maybe String -> CotonicCall
filterOptionsTopic filterId query page maybeCategory maybePredicate =
    let
        baseParams =
            [ ( "query", Encode.string query )
            , ( "page", Encode.int page )
            , ( "pagelen", Encode.int 30 )
            ]

        categoryParams =
            case maybeCategory of
                Just category ->
                    [ ( "category", Encode.string category ) ]

                Nothing ->
                    []

        predicateParams =
            case maybePredicate of
                Just predicate ->
                    [ ( "predicate", Encode.string predicate ) ]

                Nothing ->
                    []
    in
    { topic = "bridge/origin/model/search_filters/get/options"
    , parameters = baseParams ++ categoryParams ++ predicateParams
    , replyTopic = "FilterOptionsReply/" ++ filterId
    }


templateTopic : Int -> CotonicCall
templateTopic id =
    { topic = "bridge/origin/model/template/get/render/search_result.tpl"
    , parameters = [ ( "id", Encode.int id ) ]
    , replyTopic = "TemplateReply/" ++ String.fromInt id
    }


toJson : CotonicCall -> Encode.Value
toJson call =
    Encode.object
        [ ( "topic", Encode.string call.topic )
        , ( "parameters", encodeParameters call.parameters )
        , ( "replyTopic", Encode.string call.replyTopic )
        ]


encodeParameters : List ( String, Encode.Value ) -> Encode.Value
encodeParameters parameters =
    parameters
        |> List.foldl splitSinglesAndDuplicates emptyAcc
        |> mergeSinglesAndduplicates
        |> Encode.object


type alias Acc =
    { singles : List ( String, Encode.Value )
    , duplicates : List ( String, Encode.Value )
    }


emptyAcc : Acc
emptyAcc =
    { singles = []
    , duplicates = []
    }


splitSinglesAndDuplicates : ( String, Encode.Value ) -> Acc -> Acc
splitSinglesAndDuplicates ( key, value ) ({ singles, duplicates } as acc) =
    if List.member key (List.map Tuple.first duplicates) then
        { acc | duplicates = ( key, value ) :: duplicates }

    else
        let
            ( matches, remainingSingles ) =
                List.partition (\( existingKey, _ ) -> existingKey == key) singles
        in
        case matches of
            [] ->
                { acc
                    | singles = ( key, value ) :: singles
                }

            _ ->
                { singles = remainingSingles
                , duplicates =
                    matches
                        |> List.foldr (::) (( key, value ) :: duplicates)
                }


mergeSinglesAndduplicates : Acc -> List ( String, Encode.Value )
mergeSinglesAndduplicates { singles, duplicates } =
    singles
        ++ (if List.isEmpty duplicates then
                []

            else
                Encode.list
                    (\( key, value ) ->
                        Encode.object
                            [ ( "term", Encode.string key )
                            , ( "value", value )
                            ]
                    )
                    duplicates
                    |> (\v -> [ ( "allof", v ) ])
           )
