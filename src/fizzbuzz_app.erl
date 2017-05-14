%%===================================================================
%% @doc Module implementing the application behaviour.
%% It also initializes the Cowboy handler implementing the JSON Api.
%% @end
%%===================================================================
-module(fizzbuzz_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    PathMatch = "/numbers/[:number]",
    Constraints = [{number, int}],
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
