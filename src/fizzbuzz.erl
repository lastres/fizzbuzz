%%===================================================================
%% Copyright (c) 2017, Ramon Lastres
%% @doc This module exports functions that allow to calculate the Fizzbuzz
%% value (s) associated with numbers.
%% @end
%%===================================================================
-module(fizzbuzz).

-define(TABLE, favourites).

-export([output/1,
         initialize_table/0,
         set_as_favourite/1,
         set_as_no_favourite/1,
         is_favourite/1]).

%%===================================================================
%% Exoported Functions
%%===================================================================

%% @doc Given a valid input number, returns its "FizzBuzz" value
%% as a binary string
%% @end
-spec output(integer()) -> binary().
output(Number) when (Number rem 5 == 0) andalso (Number rem 3 == 0) ->
    <<"Fizz Buzz">>;
output(Number) when (Number rem 5 == 0) ->
    <<"Buzz">>;
output(Number) when (Number rem 3 == 0) ->
    <<"Fizz">>;
output(Number) ->
    integer_to_binary(Number).

%% @doc This function will initialize the ETS table to store the favourite flags
%% Note that the process calling this function will own the table.
%% @end
%%
-spec initialize_table() -> ets:tid() | atom().
initialize_table() ->
    ets:new(?TABLE, [set, public, named_table, {read_concurrency, true}]).

%% @doc Given a number, it sets it as favourite
%% @end
-spec set_as_favourite(Number :: integer()) -> true.
set_as_favourite(Number) ->
    ets:insert(?TABLE, {Number, true}).

%% @doc Given a number, is sets it as NO favourite
%% @end
-spec set_as_no_favourite(Number :: integer()) -> true.
set_as_no_favourite(Number) ->
    ets:delete(?TABLE, Number).

%% @doc Given a number, check whether is marked as
%% favourite or not
%% @end
-spec is_favourite(Number :: integer()) -> true | false.
is_favourite(Number) ->
    case ets:lookup(?TABLE, Number) of
        [] ->
            false;
        [{Number, true}] ->
            true
    end.
