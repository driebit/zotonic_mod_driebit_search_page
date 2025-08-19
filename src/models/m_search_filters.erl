-module(m_search_filters).
-author("Driebit").

-behavior(zotonic_model).

-export([
    m_get/3
]).

-include_lib("zotonic_core/include/zotonic.hrl").

m_get([<<"json">>, Id |Rest], _Msg, Context) ->
    Blocks = m_rsc:p(Id, <<"blocks">>, Context),
    Filters = lists:map(fun(Block) ->
        search_filter(Block, Context)
    end, Blocks),
    ExcludedCategories = m_rsc:p(Id, <<"exclude_categories">>, Context),
    FilteredExludedCategories = 
        lists:filter(fun(Cat) -> not z_utils:is_empty(Cat) end, ExcludedCategories),

    FiltersAndExcludedCategories = 
        maps:merge(
            #{<<"filters">> => Filters},
            #{<<"exclude_categories">> => FilteredExludedCategories}
        ),

    {ok, {jsx:encode(FiltersAndExcludedCategories), Rest}};

    
m_get(_Path, _Msg, _Context) ->
    {error, unknown_path}.


search_filter(#{<<"type">> := <<"object_filter">>} = Filter, Context) ->
    BaseProps = base_props(Filter, Context),
    SelectedCategory = maps:get(<<"selected_category">>, Filter, undefined),
    Options = 
        case SelectedCategory of
            undefined -> [];
            Category -> 
                case m_search:search({query, [{cat, Category}, {pagelen, 1000}]}, Context) of 
                    #search_result{result = Result} ->
                        add_title(Result, Context);
                    _Res -> 
                        []
                end
        end,
    Predicate = maps:get(<<"selected_predicate">>, Filter, undefined),
    PredicateName = 
        case Predicate of
            undefined -> undefined;
            Pred -> m_rsc:p(Pred, name, undefined, Context)
        end,
    
    % only add predicate if it is not undefined
    Props = maps:merge(BaseProps, #{
        <<"selected_category">> => SelectedCategory,
        <<"options">> => Options,
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
    DateProp = maps:get(<<"date_prop">>, Filter, undefined),
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
