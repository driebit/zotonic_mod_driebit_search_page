%% @author Driebit <tech@driebit.nl>
%% @copyright 2025

-module(mod_driebit_search_page).
-author("Driebit <tech@driebit.nl>").

-mod_title("Driebit search page").
-mod_description("Configurable search page").

-include_lib("zotonic_core/include/zotonic.hrl").
-include_lib("zotonic_mod_admin/include/admin_menu.hrl").

-mod_prio(500).

-mod_schema(1).

-export([
    observe_admin_menu/3,
    manage_schema/2,
    observe_admin_edit_blocks/3,
    observe_search_query_term/2
]).


observe_admin_menu(#admin_menu{}, Acc, _Context) ->
    [#menu_item{
        id = admin_search_page,
        parent = admin_modules,
        label = "Search page",
        url = {admin_edit_rsc, [{id, search_page}]},
        visiblecheck = {acl, use, mod_admin}
       }
    | Acc ].

manage_schema(_Version, _Context) ->
    #datamodel{
        resources = [
            {search_page, collection, [
                {title, <<"Search page">>}
            ]}
        ]
    }.

observe_admin_edit_blocks(#admin_edit_blocks{id = Id}, Menu, Context) ->
    case m_rsc:p(Id, name, Context) of
        <<"search_page">> ->
            [
                {100, ?__("Filters", Context), [
                    {category_filter, ?__("Filter op categorie", Context)},
                    {object_filter, ?__("Filter op relatie", Context)},
                    {date_filter, ?__("Filter op datum", Context)}
                ]}
            ];
        _ ->
            Menu
        end.

observe_search_query_term(#search_query_term{ term = <<"text_contains">>, arg = Arg }, Context) ->
    driebit_search_text_contains:search_query_term(Arg, Context);
observe_search_query_term(#search_query_term{}, _Context) ->
    undefined.


