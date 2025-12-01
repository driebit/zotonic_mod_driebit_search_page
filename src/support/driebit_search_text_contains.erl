%% @author Driebit <tech@driebit.nl>
%% @copyright 2025

-module(driebit_search_text_contains).
-author("Driebit <tech@driebit.nl>").

-include_lib("zotonic_core/include/zotonic.hrl").

-export([
    search_query_text/2,

    search_query_title/2
]).

% This is a Zotonic search query term that approximates a more permissive
% "full-text search" that also matches documents with words that "contain" the
% ones given in the query text.
% For example, a document titled "doorontwikkeling" would match the search for
% "ontwikkeling", whereas the standard 'text' term would discard it.
% Note: this is (and can only be) an approximation, because actual full-text search
% by definition does not operate on words, but on tokens that are extracted from
% documents for faster and logically meaningful matching.
% Please note: this is an experimental approach and hasn't been widely tested yet.
search_query_text(InputText, Context) ->
    CleanedInput = mod_search:trim(z_convert:to_binary(InputText), Context),
    case mod_search:to_tsquery(CleanedInput, Context) of
        <<>> ->
            % We skip queries with an empty input text
            [];
        TsQuery ->
            % Otherwise, we transform the 'tsquery' from the input text into a
            % series of regular expressions, such that each token needs to be
            % a substring of any space-separated word in the document's tokens.
            % We do this complicated tranformation in place so that we still
            % get most of the benefits of full-text indexing (e.g. stemming,
            % filtering of stopwords and space characters, etc.) while also
            % matching words that 'contain' the ones in the query.
            RegExes = tsquery_to_regexes(TsQuery),

            RankBehaviour = mod_search:rank_behaviour(Context),
            #search_sql_term{
                where = [
                    % As a condition for selection, we extract a string from the
                    % pre-caculated 'tsvector' used for full-text searches, then
                    % match it against all the regexes created before:
                    <<"array_to_string(tsvector_to_array(rsc.pivot_tsv), ' ') ~* ALL(", RegExes/binary ,")">>
                ],
                sort = [
                    % We sort in the same way as 'text' does (see the 'search_query' module).
                    % Note: this means that even tho we will find more results
                    % than with a full-text search, all of these additional
                    % results will be scored 0 here and thus be at the bottom.
                    %
                    % No extra query args in the sort term, as that gives a problem when
                    % removing the sort term when counting the exact number of rows.
                    [
                      "ts_rank_cd(", mod_search:rank_weight(Context),
                      ", rsc.pivot_tsv, ", '$1', ", ", integer_to_binary(RankBehaviour), ") desc"
                    ]
                ],
                args = [
                    TsQuery
                ]
            }
    end.

% This is a simple Zotonic search query term that returns any resource whose
% title contain all of the words in the given text.
% This is meant to find options, generally to aid a selection or menu, thus it
% simply sorts the results by their title's length (from shortest to longest).
% Please note: this is an experimental approach and hasn't been widely tested yet.
search_query_title(InputText, Context) ->
    CleanedInput = mod_search:trim(z_convert:to_binary(InputText), Context),
    RegExes = words_to_regexes(CleanedInput),
    #search_sql_term{
        % Use a regex on the pivot title in the resource:
        where = [<<"rsc.pivot_title ~* ALL(", RegExes/binary ,")">>],
        % We simply sort by the length of the title, from shortest to longest:
        sort = ["length(rsc.pivot_title) ASC"]
    }.

words_to_regexes(Text) ->
    RegExes = lists:foldl(
        fun (Word, Acc) ->
            case z_string:trim(Word) of
                <<>> -> Acc;
                Token ->
                    RegEx = match_word_regex(Token),
                    case Acc of
                        <<>> -> RegEx;
                        _ -> <<Acc/binary, ", ", RegEx/binary>>
                    end
            end
        end,
        <<>>,
        binary:split(Text, <<" ">>, [global])
    ),
    <<"ARRAY[", RegExes/binary ,"]">>.

tsquery_to_regexes(TsQuery) ->
    RegExes = lists:foldl(
        fun (Word, Acc) ->
            case z_string:trim(Word) of
                <<>> -> Acc;
                <<"&">> -> Acc;
                <<":*">> -> Acc;
                % Because 'mod_search:to_tsquery' uses psql's 'plainto_tsquery'
                % and adds a ':*' at the end, the above are the only cases we
                % need to filter out
                Token ->
                    RegEx = match_word_regex(Token),
                    case Acc of
                        <<>> -> RegEx;
                        _ -> <<Acc/binary, ", ", RegEx/binary>>
                    end
            end
        end,
        <<>>,
        binary:split(TsQuery, <<"'">>, [global])
    ),
    <<"ARRAY[", RegExes/binary ,"]">>.

match_word_regex(Token) ->
    % matches: word start > any word chars > token > any word chars > word end
    <<"'\\m\\w*", Token/binary, "\\w*\\M'">>.
