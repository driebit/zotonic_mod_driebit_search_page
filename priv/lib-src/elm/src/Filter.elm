module Filter exposing (..)

import Collapse exposing (Collapse)
import Filter.DateComponent as DateComponent exposing (DateComponent)
import Filter.TextualComponent as TextualComponent exposing (TextualComponent)
import Filter.TextualComponent.Multiselect as Multiselect
import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import List
import Resource
import String
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
    | Object ObjectMeta
    | Date


type alias ObjectMeta =
    { predicate : Maybe String
    , category : Maybe String
    }


type Msg
    = CollapseMsg Collapse.Msg
    | TextualComponentMsg TextualComponent.Msg
    | DateComponentMsg DateComponent.Msg


type FilterEffect
    = FetchMultiselectOptions
        { filterId : String
        , query : String
        , category : Maybe String
        , predicate : Maybe String
        , page : Int
        }


update : Msg -> Filter -> ( Filter, List FilterEffect )
update msg filter =
    case ( msg, filter.component ) of
        ( TextualComponentMsg textualMsg, TextualComponent textualComponent ) ->
            let
                ( updatedComponent, textualEffects ) =
                    TextualComponent.update textualMsg textualComponent

                updatedFilter =
                    { filter | component = TextualComponent updatedComponent }
            in
            ( updatedFilter, textualEffectsToFilterEffects updatedFilter textualEffects )

        ( DateComponentMsg dateMsg, DateComponent dateComponent ) ->
            ( { filter | component = DateComponent <| DateComponent.update dateMsg dateComponent }, [] )

        ( CollapseMsg _, _ ) ->
            ( filter, [] )

        _ ->
            ( filter, [] )


textualEffectsToFilterEffects : Filter -> List TextualComponent.Effect -> List FilterEffect
textualEffectsToFilterEffects filter effects =
    effects
        |> List.filterMap (textualEffectToFilterEffect filter)


textualEffectToFilterEffect : Filter -> TextualComponent.Effect -> Maybe FilterEffect
textualEffectToFilterEffect filter effect =
    case ( effect, filter.filterType ) of
        ( TextualComponent.MultiselectEffect (Multiselect.FetchOptions request), Object objectMeta ) ->
            case objectMeta.category of
                Just category ->
                    Just
                        (FetchMultiselectOptions
                            { filterId = filter.id
                            , query = request.query
                            , category = Just category
                            , predicate = objectMeta.predicate
                            , page = request.page
                            }
                        )

                _ ->
                    Nothing

        _ ->
            Nothing


initialize : Filter -> ( Filter, List FilterEffect )
initialize filter =
    case filter.component of
        TextualComponent textualComponent ->
            let
                ( updatedComponent, textualEffects ) =
                    TextualComponent.initialize textualComponent

                updatedFilter =
                    { filter | component = TextualComponent updatedComponent }
            in
            ( updatedFilter, textualEffectsToFilterEffects updatedFilter textualEffects )

        DateComponent _ ->
            ( filter, [] )


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


decodeMaybeStringField : String -> Decoder (Maybe String)
decodeMaybeStringField fieldName =
    Decode.oneOf
        [ Decode.field fieldName (Decode.string |> Decode.map Just)
        , Decode.field fieldName (Decode.int |> Decode.map (String.fromInt >> Just))
        , Decode.succeed Nothing
        ]


decodeSpecificFilterProps : String -> String -> Decoder ( Component, FilterType )
decodeSpecificFilterProps type_ title =
    case type_ of
        "category_filter" ->
            Decode.map2 (\toComponent options -> ( TextualComponent (toComponent title options False), Category ))
                (Decode.field "component" TextualComponent.fromJson)
                (Decode.field "options" (Decode.list Resource.fromJson))

        "object_filter" ->
            Decode.map5
                (\toComponent options hasMore predicate category ->
                    let
                        normalizedPredicate =
                            case predicate of
                                Just "" ->
                                    Nothing

                                _ ->
                                    predicate

                        component =
                            TextualComponent (toComponent title options hasMore)
                    in
                    ( component
                    , Object
                        { predicate = normalizedPredicate
                        , category = category
                        }
                    )
                )
                (Decode.field "component" TextualComponent.fromJson)
                (Decode.field "options" (Decode.list Resource.fromJson))
                (Decode.oneOf
                    [ Decode.field "options_has_more" Decode.bool
                    , Decode.succeed False
                    ]
                )
                (Decode.oneOf
                    [ Decode.field "selected_predicate" (Decode.maybe Decode.string)
                    , Decode.succeed Nothing
                    ]
                )
                (decodeMaybeStringField "selected_category")

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

        Object objectMeta ->
            case filter.component of
                TextualComponent textualComponent ->
                    TextualComponent.encodedValue "hasanyobject" objectMeta.predicate textualComponent

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
