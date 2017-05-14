%%===================================================================
%% @doc This module exposes functions providing JSON object generation
%% for implementation of the JSON API.
%% @end
%%===================================================================
-module(fizzbuzz_json).

-export([top_level_numbers_json/1,
         top_level_numbers_page_json/2,
         resource_number/1,
         error_json/3,
         numbers_list/2]).

%%===================================================================
%% Exported functions
%%===================================================================

%% @doc Given a positive interger value, it returns to the top level
%% JSON for the number (/number) resource.
%% @end
-spec top_level_numbers_json(Number :: integer()) -> jsx:json_text().
top_level_numbers_json(Number) ->
    Internal = [{<<"data">>, resource_number(Number)}],
    jsx:encode(Internal).

%% @doc Given a PageSize and PageNumber values, generate the top level JSON
%% element to be included as the body of the response to the query
%% @end
-spec top_level_numbers_page_json(PageSize :: integer(), PageNumber :: integer()) ->
    jsx:json_text().
top_level_numbers_page_json(PageSize, PageNumber) ->
    StartPoint = ((PageNumber - 1) * PageSize) + 1,
    %% TODO: Check that we do not go over the maximum number!
    TotalPages = round(100000000000 / PageSize),
    Meta = [{<<"meta">>, [{<<"total-pages">>, TotalPages}]}],
    Values = [{<<"data">>, numbers_list(StartPoint, PageSize)}],
    Internal = Meta ++ Values,
    jsx:encode(Internal).

%% @doc Return a singe number object as an internal JSON representation
%% @end
-spec resource_number(Number :: integer()) -> jsx:json_term().
resource_number(Number) ->
    [{<<"type">>, <<"numbers">>},
    {<<"id">>, integer_to_binary(Number)},
    {<<"attributes">>,
     [{<<"value">>, fizzbuzz:output(Number)}]}].

%% @doc Given a starting number and a total, return an internal JSON
%% array of numbers representation
%% @end
-spec numbers_list(Start :: integer(), Size :: integer()) -> jsx:json_term().
numbers_list(Start, Size) ->
    Numbers = lists:seq(Start, Start + Size - 1),
    lists:map(fun resource_number/1, Numbers).

%% @doc Given an error code and details, return a JSON object to be used as a response
%% body in error situations
%% @end
-spec error_json(HttpStatusCode :: integer(), Title :: binary(), Detail :: binary()) ->
    jsx:json_text().
error_json(HttpStatusCode, Title, Detail) ->
    Internal = [{<<"errors">>, [{<<"status">>, HttpStatusCode},
                                {<<"title">>, Title},
                                {<<"detail">>, Detail}]}],
    jsx:encode(Internal).
