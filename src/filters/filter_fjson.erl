%% @author Driebit <tech@driebit.nl>
%% @copyright 2025 Driebit

%% Copyright 2025 Driebit
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(filter_fjson).

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    fjson/2
]).

fjson(#{<<"type">>:= <<"object_filter">>} = Filter, Context) ->
    Title = maps:get(<<"filter_title">>, Filter, <<"Filter">>),
    Collapse = maps:get(<<"collapse">>, Filter, uncollapsable),
    SelectedCategory = maps:get(<<"selected_category">>, Filter, undefined),
    Predicate = maps:get(<<"predicate">>, Filter, undefined),
    DisplayMode = maps:get(<<"displaymode">>, Filter, default),
    Name = maps:get(<<"name">>, Filter),
    Options = 
        case SelectedCategory of
            undefined -> [];
            Category -> 
                case m_search:search({query, [{cat, Category}, {pagelen, 1000}]}, Context) of 
                    #search_result{result = Result} ->
                        add_title(Result, Context);
                    Res -> 
                        []
                end
        end,
    Props = #{
        type => <<"object_filter">>,
        title => Title,
        collapse => Collapse,
        predicate => Predicate,
        options => Options,
        display_mode => DisplayMode,
        name => Name
        
    },

    %% Convert the filter to JSON
    jsx:encode(Props);

fjson(#{<<"type">>:= <<"category_filter">>} = Filter, Context) ->
    Title = maps:get(<<"filter_title">>, Filter, <<"Filter">>),
    Collapse = maps:get(<<"collapse">>, Filter, uncollapsable),
    SelectedCategories = maps:get(<<"show_categories">>, Filter, []),
    DisplayMode = maps:get(<<"displaymode">>, Filter, default),
    Name = maps:get(<<"name">>, Filter),
    Props = #{
        type => <<"category_filter">>,
        title => Title,
        collapse => Collapse,
        options => add_title(lists:filter(fun(Value) -> not z_utils:is_empty(Value) end, SelectedCategories), Context),
        display_mode => DisplayMode,
        name => Name
    },
    %% Convert the filter to JSON

    jsx:encode(Props);

fjson(Filter, _Context) ->
    %% For other filter types, we can return an empty JSON object or handle them as needed
    jsx:encode(#{}).

add_title(Ids, Context) ->
    lists:map(fun(R) -> #{id => m_rsc:p(R, id, Context), title => z_trans:lookup_fallback(m_rsc:p(R, title, Context), Context)} end, Ids).
