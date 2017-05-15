%%===================================================================
%% This module implements tests for the JSON Api running on top
%% of Cowboy
%%===================================================================
-module(fizzbuzz_json_api_tests).

-include_lib("eunit/include/eunit.hrl").

-define(BASEURL, "http://127.0.0.1:8080/").
-define(MAXNUMBER, 100000000000).

list_of_tests() ->
    [fun simple_get/1,
     fun get_one_resource/1,
     fun resource_number_must_be_integer/1,
     fun pagination_default_values/1,
     fun pagination_parameters_non_integers/1,
     fun pagination_parameters_zero/1,
     fun pagination_parameters_too_big/1,
     fun resource_number_zero/1,
     fun resource_number_too_big/1].

fizzbuzz_jsonapi_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     list_of_tests()}.

start() ->
    {ok, _} = application:ensure_all_started(fizzbuzz),
    {ok, _} = application:ensure_all_started(hackney).
 
stop(_) ->
    ok.

simple_get(_) ->
    URL = ?BASEURL ++ "numbers",
    {ok, StatusCode, _RespHeaders, _} = hackney:request("GET", URL, [], <<>>, []),
    ?_assertEqual(200, StatusCode).

get_one_resource(_) ->
    URL = ?BASEURL ++ "numbers/30",
    {ok, StatusCode, _RespHeaders, Client} = hackney:request("GET", URL, [], <<>>, []),
    {ok, Body} = hackney:body(Client),
    Decoded = jsx:decode(Body),
    Expected = [{<<"data">>, [{<<"type">>, <<"numbers">>}, {<<"id">>, <<"30">>}, {<<"attributes">>, [{<<"value">>, <<"Fizz Buzz">>}]}]}],
    ?_assertEqual(200, StatusCode),
    ?_assertEqual(Expected, Decoded).

resource_number_must_be_integer(_) ->
    URL = ?BASEURL ++ "numbers/foo",
    {ok, StatusCode, _RespHeaders, _} = hackney:request("GET", URL, [], <<>>, []),
    ?_assertEqual(404, StatusCode).

pagination_default_values(_) ->
    URL1 = ?BASEURL ++ "numbers",
    URL2 = ?BASEURL ++ "numbers?page[number]=1&page[size]=100",
    {ok, StatusCode1, _RespHeaders, Client1} = hackney:request("GET", URL1, [], <<>>, []),
    {ok, Body1} = hackney:body(Client1),
    {ok, StatusCode2, _RespHeaders, Client2} = hackney:request("GET", URL2, [], <<>>, []),
    {ok, Body2} = hackney:body(Client2),
    ?_assertEqual(200, StatusCode1),
    ?_assertEqual(200, StatusCode2),
    ?_assertEqual(true, jsx:is_json(Body1)),
    ?_assertEqual(Body1, Body2).

pagination_parameters_non_integers(_) ->
    URL = ?BASEURL ++ "numbers?page[number]=foo",
    {ok, StatusCode, _RespHeaders, _} = hackney:request("GET", URL, [], <<>>, []),
    ?_assertEqual(400, StatusCode).

pagination_parameters_zero(_) ->
    URL = ?BASEURL ++ "numbers?page[number]=0",
    {ok, StatusCode, _RespHeaders, _} = hackney:request("GET", URL, [], <<>>, []),
    ?_assertEqual(400, StatusCode).

pagination_parameters_too_big(_) ->
    URL = ?BASEURL ++ "numbers?page[number]=100?page[size]=" ++ integer_to_list(?MAXNUMBER),
    {ok, StatusCode, _RespHeaders, _} = hackney:request("GET", URL, [], <<>>, []),
    ?_assertEqual(400, StatusCode).

resource_number_zero(_) ->
    URL = ?BASEURL ++ "numbers/0",
    {ok, StatusCode, _RespHeaders, _} = hackney:request("GET", URL, [], <<>>, []),
    ?_assertEqual(404, StatusCode).

resource_number_too_big(_) ->
    URL = ?BASEURL ++ "numbers/" ++ integer_to_list(?MAXNUMBER + 1),
    {ok, StatusCode, _RespHeaders, _} = hackney:request("GET", URL, [], <<>>, []),
    ?_assertEqual(404, StatusCode).
