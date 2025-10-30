module Cotonic exposing (..)

import Json.Encode as Encode


type alias CotonicCall =
    { topic : String
    , parameters : List ( String, Encode.Value )
    , replyTopic : String
    }


searchPageTopic : List ( String, Encode.Value ) -> CotonicCall
searchPageTopic parameters =
    { topic = "bridge/origin/model/search/get"
    , parameters = parameters
    , replyTopic = "SearchReply"
    }


templateTopic : Int -> CotonicCall
templateTopic id =
    { topic = "bridge/origin/model/template/get/render/search_result.tpl"
    , parameters = [ ( "id", Encode.int id ) ]
    , replyTopic = "TemplateReply/" ++ String.fromInt id
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


toJson : CotonicCall -> Encode.Value
toJson call =
    Encode.object
        [ ( "topic", Encode.string call.topic )
        , ( "parameters", Encode.object call.parameters )
        , ( "replyTopic", Encode.string call.replyTopic )
        ]
