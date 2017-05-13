%%===================================================================
%% @doc This module exposes functions providing JSON object generation
%% for implementation of the JSON API.
%% @end
%%===================================================================
-module(fizzbuzz_json).

-export([resource_number/1,
         numbers_list/2]).

%%===================================================================
%% Exported functions
%%===================================================================

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
-spec numbers_list(Start :: integer(), Total :: integer()) -> jsx:json_term().
numbers_list(Start, Total) ->
    Numbers = lists:seq(Start, Start + Total),
    lists:map(fun resource_number/1, Numbers).
