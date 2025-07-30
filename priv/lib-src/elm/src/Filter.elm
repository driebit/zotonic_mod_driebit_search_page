module Filter exposing (..)

import Collapse exposing (Collapse)
import DateComponent exposing (DateComponent)
import Html exposing (..)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Resource
import TextualComponent exposing (TextualComponent)


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
    | Date String


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
            Decode.map3 (\toComponent options predicate -> ( TextualComponent (toComponent title options), Object predicate ))
                (Decode.field "component" TextualComponent.fromJson)
                (Decode.field "options" (Decode.list Resource.fromJson))
                (Decode.oneOf
                    [ Decode.field "predicate" (Decode.maybe Decode.string)
                    , Decode.succeed Nothing
                    ]
                )

        "date_filter" ->
            Decode.map2 (\component dateProp -> ( DateComponent component, Date dateProp ))
                (Decode.field "component" DateComponent.fromJson)
                (Decode.field "date_prop" Decode.string)

        _ ->
            Decode.fail ("Unknown filter type: " ++ type_)


view : Filter -> Html Msg
view filter =
    case filter.component of
        TextualComponent textualComponent ->
            Collapse.view filter.collapse
                filter.title
                (Html.map TextualComponentMsg (TextualComponent.view textualComponent))

        DateComponent dateComponent ->
            Collapse.view filter.collapse
                filter.title
                (Html.map DateComponentMsg (DateComponent.view dateComponent))


componentToValue : Maybe String -> Component -> Maybe Encode.Value
componentToValue maybePredicate component =
    case component of
        TextualComponent textualComponent ->
            TextualComponent.encodedValue maybePredicate textualComponent

        DateComponent dateComponent ->
            DateComponent.encodedValue dateComponent


toSearchParams : Filter -> List ( String, Encode.Value )
toSearchParams filter =
    case filter.filterType of
        Category ->
            case componentToValue Nothing filter.component of
                Just value ->
                    [ ( "cat", value ) ]

                Nothing ->
                    []

        Object maybePredicate ->
            case componentToValue maybePredicate filter.component of
                Just value ->
                    [ ( "hasanyobject", value ) ]

                Nothing ->
                    []

        Date dateProp ->
            case componentToValue Nothing filter.component of
                Just value ->
                    [ ( "date", Encode.string dateProp ), ( "value", value ) ]

                Nothing ->
                    []
