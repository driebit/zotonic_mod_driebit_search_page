module Filter exposing (..)

import Collapse exposing (Collapse)
import Dict exposing (Dict)
import Filter.DateComponent as DateComponent exposing (DateComponent)
import Filter.TextualComponent as TextualComponent exposing (TextualComponent)
import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Resource
import Translations exposing (Language)


type alias Filter =
    { title : String
    , collapse : Collapse
    , id : String
    , component : Component
    , filterType : FilterType
    }


type Component
    = TextualComponent TextualComponent
    | DateComponent DateComponent


type FilterType
    = Category
    | Object (Maybe String)
    | Date


type Msg
    = CollapseMsg Collapse.Msg
    | TextualComponentMsg TextualComponent.Msg
    | DateComponentMsg DateComponent.Msg


update : Msg -> Filter -> Filter
update msg filter =
    case ( msg, filter.component ) of
        ( TextualComponentMsg textualMsg, TextualComponent textualComponent ) ->
            { filter | component = TextualComponent <| TextualComponent.update textualMsg textualComponent }

        ( DateComponentMsg dateMsg, DateComponent dateComponent ) ->
            { filter | component = DateComponent <| DateComponent.update dateMsg dateComponent }

        ( CollapseMsg collapseMsg, filter_ ) ->
            filter

        _ ->
            filter


fromJson : Decoder Filter
fromJson =
    decodeBaseFilterProps
        |> Decode.andThen
            (\baseFilterProps ->
                decodeSpecificFilterProps baseFilterProps.type_ baseFilterProps.name
                    |> Decode.map
                        (\( component, filterType ) ->
                            { title = baseFilterProps.name
                            , collapse = baseFilterProps.collapse
                            , id = baseFilterProps.id
                            , component = component
                            , filterType = filterType
                            }
                        )
            )


type alias BaseFilterProps =
    { name : String
    , collapse : Collapse
    , id : String
    , type_ : String
    }


decodeBaseFilterProps : Decoder BaseFilterProps
decodeBaseFilterProps =
    Decode.map4 BaseFilterProps
        (Decode.field "title" Decode.string)
        (Decode.field "collapse" Collapse.fromJson)
        (Decode.field "name" Decode.string)
        (Decode.field "type" Decode.string)


decodeSpecificFilterProps : String -> String -> Decoder ( Component, FilterType )
decodeSpecificFilterProps type_ title =
    case type_ of
        "category_filter" ->
            Decode.map2 (\toComponent options -> ( TextualComponent (toComponent title options), Category ))
                (Decode.field "component" TextualComponent.fromJson)
                (Decode.field "options" (Decode.list Resource.fromJson))

        "object_filter" ->
            Decode.map3
                (\toComponent options predicate ->
                    ( TextualComponent (toComponent title options)
                    , Object
                        (if predicate == Just "" then
                            Nothing

                         else
                            predicate
                        )
                    )
                )
                (Decode.field "component" TextualComponent.fromJson)
                (Decode.field "options" (Decode.list Resource.fromJson))
                (Decode.oneOf
                    [ Decode.field "selected_predicate" (Decode.maybe Decode.string)
                    , Decode.succeed Nothing
                    ]
                )

        "date_filter" ->
            Decode.field "date_prop" Decode.string
                |> Decode.andThen
                    (\dateProp ->
                        Decode.field "component" (DateComponent.fromJson dateProp)
                            |> Decode.map (\component -> ( DateComponent component, Date ))
                    )

        _ ->
            Decode.fail ("Unknown filter type: " ++ type_)


view : Language -> Filter -> Html Msg
view language filter =
    case filter.component of
        TextualComponent textualComponent ->
            Collapse.view filter.collapse
                (h3 [ class "c-collapse__title" ] [ text filter.title ])
                (Html.map TextualComponentMsg (TextualComponent.view language textualComponent))

        DateComponent dateComponent ->
            Collapse.view filter.collapse
                (h3 [ class "c-collapse__title" ] [ text filter.title ])
                (Html.map DateComponentMsg (DateComponent.view language dateComponent))


toSearchParams : Filter -> List ( String, Encode.Value )
toSearchParams filter =
    case filter.filterType of
        Category ->
            case filter.component of
                TextualComponent textualComponent ->
                    TextualComponent.encodedValue "cat" Nothing textualComponent

                DateComponent _ ->
                    []

        Object maybePredicate ->
            case filter.component of
                TextualComponent textualComponent ->
                    TextualComponent.encodedValue "hasanyobject" maybePredicate textualComponent

                DateComponent _ ->
                    []

        Date ->
            case filter.component of
                TextualComponent _ ->
                    []

                DateComponent dateComponent ->
                    DateComponent.encodedValue dateComponent


applyUrlEncodedValue : Dict String String -> Filter -> Filter
applyUrlEncodedValue queryParams filter =
    Dict.get filter.id queryParams
        |> applyUrlValue filter


applyUrlValue : Filter -> Maybe String -> Filter
applyUrlValue filter maybeEncoded =
    case maybeEncoded of
        Nothing ->
            filter

        Just encoded ->
            case Decode.decodeString urlValueDecoder encoded of
                Ok params ->
                    applyParams params filter

                Err _ ->
                    applySimpleValue encoded filter


applyParams : List ( String, Decode.Value ) -> Filter -> Filter
applyParams params filter =
    case ( filter.filterType, filter.component ) of
        ( Category, TextualComponent textualComponent ) ->
            { filter
                | component =
                    TextualComponent <|
                        TextualComponent.applyUrlValue "cat" Nothing params textualComponent
            }

        ( Object maybePredicate, TextualComponent textualComponent ) ->
            { filter
                | component =
                    TextualComponent <|
                        TextualComponent.applyUrlValue "hasanyobject" maybePredicate params textualComponent
            }

        ( Date, DateComponent dateComponent ) ->
            { filter
                | component =
                    DateComponent <|
                        DateComponent.applyUrlValue params dateComponent
            }

        _ ->
            filter


applySimpleValue : String -> Filter -> Filter
applySimpleValue encoded filter =
    case ( filter.filterType, filter.component ) of
        ( Category, TextualComponent textualComponent ) ->
            let
                ids =
                    parseIds encoded
            in
            { filter
                | component =
                    TextualComponent <|
                        TextualComponent.setSelectedIds ids textualComponent
            }

        ( Object _, TextualComponent textualComponent ) ->
            let
                ids =
                    parseIds encoded
            in
            { filter
                | component =
                    TextualComponent <|
                        TextualComponent.setSelectedIds ids textualComponent
            }

        ( Date, DateComponent dateComponent ) ->
            { filter
                | component =
                    DateComponent <|
                        DateComponent.applyUrlString encoded dateComponent
            }

        _ ->
            filter


parseIds : String -> List Int
parseIds encoded =
    case Decode.decodeString (Decode.list Decode.string) encoded of
        Ok stringIds ->
            stringIds |> List.filterMap String.toInt

        Err _ ->
            case Decode.decodeString (Decode.list (Decode.list Decode.string)) encoded of
                Ok listOfLists ->
                    listOfLists
                        |> List.concat
                        |> List.filterMap String.toInt

                Err _ ->
                    case Decode.decodeString Decode.string encoded of
                        Ok single ->
                            parseIdsFromText single

                        Err _ ->
                            parseIdsFromText encoded


parseIdsFromText : String -> List Int
parseIdsFromText text =
    text
        |> String.split ","
        |> List.map String.trim
        |> List.filter (\part -> not (String.isEmpty part))
        |> List.filterMap String.toInt


toUrlQueryValue : Filter -> Maybe String
toUrlQueryValue filter =
    case ( filter.filterType, filter.component ) of
        ( Category, TextualComponent textualComponent ) ->
            idsToUrl (TextualComponent.selectedIds textualComponent)

        ( Object _, TextualComponent textualComponent ) ->
            idsToUrl (TextualComponent.selectedIds textualComponent)

        ( Date, DateComponent dateComponent ) ->
            DateComponent.toUrlValue dateComponent

        _ ->
            Nothing


idsToUrl : List Int -> Maybe String
idsToUrl ids =
    case ids of
        [] ->
            Nothing

        _ ->
            ids
                |> List.map String.fromInt
                |> String.join ","
                |> Just


urlValueDecoder : Decode.Decoder (List ( String, Decode.Value ))
urlValueDecoder =
    Decode.list
        (Decode.map2 Tuple.pair
            (Decode.field "key" Decode.string)
            (Decode.field "value" Decode.value)
        )
