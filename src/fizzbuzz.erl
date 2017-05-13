%%===================================================================
%% @doc This module exports functions that allow to calculate the Fizzbuzz
%% value (s) associated with numbers.
%% @end
%%===================================================================
-module(fizzbuzz).

-export([output/1]).

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
