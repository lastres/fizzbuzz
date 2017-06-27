%%===================================================================
%% Copyright (c) 2017, Ramon Lastres
%% @doc Module implementing the application behaviour.
%% It also initializes the Cowboy handler implementing the JSON Api.
%% @end
%%===================================================================
-module(fizzbuzz_app).

-behaviour(application).

-include("fizzbuzz.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->

    fizzbuzz:initialize_table(),

    PathMatch = "/numbers/[:number]",
    Constraints = [{number, int},
                   {number, function, fun number_within_range/1}],
    Dispatch = cowboy_router:compile([
                {'_', [{PathMatch, Constraints, fizzbuzz_handler, []}]}
                ]),
    cowboy:start_http(my_http_listener, 100,
                      [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]
                     ),
    fizzbuzz_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
%% @doc Cowboy will return 404 for us when the number is not within
%% range.
%% @end
number_within_range(Number) when Number > 0 andalso Number =< ?MAXNUMBER ->
    true;
number_within_range(_Number) ->
    false.
