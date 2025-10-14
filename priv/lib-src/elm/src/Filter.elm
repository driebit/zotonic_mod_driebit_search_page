module Filter exposing (..)

import Collapse exposing (Collapse)
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


toUrlQueryValue : Filter -> Maybe Encode.Value
toUrlQueryValue filter =
    filter
    |> toSearchParams 
    |> List.map Tuple.second
    |> List.head
