-module(m_search_filters).
-author("Driebit").

-behavior(zotonic_model).

-export([
    m_get/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

m_get([<<"json">>, Id |Rest], _Msg, Context) ->
    Blocks = m_rsc:p(Id, <<"blocks">>, [], Context),
    Filters = lists:map(fun(Block) ->
        search_filter(Block, Context)
    end, Blocks),
    ExcludedCategories = m_rsc:p(Id, <<"exclude_categories">>, Context),
    FilteredExludedCategories = 
        lists:filter(fun(Cat) -> not z_utils:is_empty(Cat) end, ExcludedCategories),
    PageLength = max(0, z_convert:to_integer(m_rsc:p(Id, <<"page_len">>, 20, Context))),

    FiltersAndExcludedCategories = 
        maps:merge(
            #{<<"filters">> => Filters},
            #{<<"exclude_categories">> => FilteredExludedCategories}
        ),

    ConfigWithPagelen = maps:put(<<"pagelen">>, PageLength, FiltersAndExcludedCategories),

    {ok, {jsx:encode(ConfigWithPagelen), Rest}};

m_get([<<"options">>] = _Path, Msg, Context) ->
    Payload = maps:get(payload, Msg, #{}),
    Category = maps:get(<<"category">>, Payload, undefined),
    Predicate = maps:get(<<"predicate">>, Payload, undefined),
    Query = z_convert:to_binary(maps:get(<<"query">>, Payload, <<>>)),
    SelectedIds = selected_ids_from_payload(maps:get(<<"selected_ids">>, Payload, [])),
    Page = case z_convert:to_integer(maps:get(<<"page">>, Payload, 1)) of
        P when is_integer(P), P > 0 -> P;
        _ -> 1
    end,
    PageLen = case z_convert:to_integer(maps:get(<<"pagelen">>, Payload, 20)) of
        N when N > 0 -> N;
        _ -> 20
    end,
    {Options, HasMore, ReturnedPage} =
        case Category of
            undefined ->
                {[], false, Page};
            _ ->
                search_options(Category, Predicate, Query, PageLen, Page, Context)
        end,
    SelectedOptions = add_title(SelectedIds, Context),
    OptionsWithSelected = merge_selected_options(SelectedOptions, Options),
    {ok, {#{<<"options">> => OptionsWithSelected, <<"query">> => Query, <<"has_more">> => HasMore, <<"page">> => ReturnedPage}, []}};

m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.


search_options(Category, PredicateName, Query, PageLen, Page, Context) ->
    SearchProps0 = [{cat, Category}, {text, Query}, {page, Page}, {pagelen, PageLen}],
    SearchProps1 =
        case PredicateName of
            undefined -> SearchProps0;
            _ -> SearchProps0 ++ [{hasanysubject, ['*', PredicateName]}]
        end,
    case m_search:search({query, SearchProps1}, Context) of
        #search_result{result = Result, next = Next, pages = Pages, page = CurrentPage} ->
            Titles = add_title(Result, Context),
            HasMore =
                case Next of
                    undefined -> pages_has_more(Pages, CurrentPage);
                    false -> pages_has_more(Pages, CurrentPage);
                    _ -> true
                end,
            {Titles, HasMore, CurrentPage};
        _ ->
            {[], false, Page}
    end.

pages_has_more(undefined, _CurrentPage) -> false;
pages_has_more(Pages, CurrentPage) when is_integer(Pages) ->
    Pages > CurrentPage;
pages_has_more(_, _) ->
    false.


search_filter(#{<<"type">> := <<"object_filter">>} = Filter, Context) ->
    BaseProps = base_props(Filter, Context),
    SelectedCategory = maps:get(<<"selected_category">>, Filter, undefined),
    FilterName = maps:get(<<"name">>, Filter, undefined),
    
    Predicate = maps:get(<<"selected_predicate">>, Filter, undefined),
    PredicateName = 
        case Predicate of
            undefined -> undefined;
            Pred -> m_rsc:p(Pred, name, undefined, Context)
        end,
    SelectedIds = selected_ids_from_query(FilterName, Context),
    SelectedOptions = add_title(SelectedIds, Context),
    {Options, HasMore, _Page} = 
        case SelectedCategory of
            undefined -> {[], false, 1};
            <<>> -> {[], false, 1};
            [] -> {[], false, 1};
            Category -> 
                search_options(Category, PredicateName, <<>>, 30, 1, Context)
        end,
    OptionsWithSelected = merge_selected_options(SelectedOptions, Options),
    % only add predicate if it is not undefined
    Props = maps:merge(BaseProps, #{
        <<"selected_category">> => SelectedCategory,
        <<"options">> => OptionsWithSelected,
        <<"options_has_more">> => HasMore,
        <<"type">> => <<"object_filter">>
    }),

    case PredicateName of
        undefined -> 
            Props;
        _ -> 
            maps:merge(Props, #{
                <<"selected_predicate">> => PredicateName
            })
    end;
  

search_filter(#{<<"type">> := <<"category_filter">>} = Filter, Context) ->
    BaseProps = base_props(Filter, Context),
    SelectedCategories = maps:get(<<"show_categories">>, Filter, []),
    maps:merge(BaseProps, #{
        <<"options">> => add_title(lists:filter(fun(Value) -> not z_utils:is_empty(Value) end, SelectedCategories), Context),
        <<"type">> => <<"category_filter">>
    });

    
search_filter(#{<<"type">> := <<"date_filter">>} = Filter, Context) ->
    BaseProps = base_props(Filter, Context),
    DateProp = case maps:get(<<"date_prop">>, Filter, undefined) of 
        <<"custom">> -> maps:get(<<"custom_date_prop">>, Filter, undefined);
        Prop -> Prop
    end,
    maps:merge(BaseProps, #{
        <<"date_prop">> => DateProp,
        <<"type">> => <<"date_filter">>
    });

search_filter(Filter, _Context) ->
    %% For other filter types, return the filter as is
    Filter.

base_props(Filter, Context) ->
    Name = maps:get(<<"name">>, Filter),
    Collapse = maps:get(<<"collapse">>, Filter, collapsed),
    Component = maps:get(<<"component">>, Filter, <<"dropdown">>),
    Title = maps:get(<<"title">>, Filter, <<"Filter">>),
    #{
        <<"name">> => Name,
        <<"collapse">> => Collapse,
        <<"component">> => Component,
        <<"title">> => z_trans:lookup_fallback(Title, Context)
    }.

add_title(Ids, Context) ->
    lists:map(fun(R) -> #{id => m_rsc:p(R, id, Context), title => z_trans:lookup_fallback(m_rsc:p(R, title, Context), Context)} end, Ids).

selected_ids_from_query(undefined, _Context) ->
    [];
selected_ids_from_query(Name, Context) ->
    Values = z_context:get_q_all(Name, Context),
    Ids =
        lists:flatmap(
            fun(Value) ->
                parse_selected_value(z_convert:to_binary(Value))
            end,
            Values
        ),
    lists:usort([ Id || Id <- Ids, is_integer(Id), Id > 0 ]).

selected_ids_from_payload(undefined) ->
    [];
selected_ids_from_payload(Value) when is_list(Value) ->
    Ids =
        lists:flatmap(
            fun(Item) ->
                case catch z_convert:to_integer(Item) of
                    Int when is_integer(Int) -> [Int];
                    _ -> []
                end
            end,
            Value
        ),
    lists:usort([ Id || Id <- Ids, Id > 0 ]);
selected_ids_from_payload(_) ->
    [].

parse_selected_value(<<>>) ->
    [];
parse_selected_value(Value) ->
    Parts = binary:split(Value, <<",">>, [ global ]),
    lists:foldl(
        fun(Part, Acc) ->
            Trimmed = z_string:trim(Part),
            case Trimmed of
                <<>> ->
                    Acc;
                _ ->
                    case catch binary_to_integer(Trimmed) of
                        Int when is_integer(Int) ->
                            [ Int | Acc ];
                        _ ->
                            Acc
                    end
            end
        end,
        [],
        Parts
    ).

merge_selected_options([], Options) ->
    Options;
merge_selected_options(SelectedOptions, Options) ->
    SelectedIds = [ maps:get(id, Opt) || Opt <- SelectedOptions ],
    RemainingOptions = [ Opt || Opt <- Options, not lists:member(maps:get(id, Opt), SelectedIds) ],
    SelectedOptions ++ RemainingOptions.
