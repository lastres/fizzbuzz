%%===================================================================
%% Copyright (c) 2017, Ramon Lastres
%% @doc This module implements the Cowboy request handler behaviour
%% @end
%%===================================================================
-module(fizzbuzz_handler).

-include("fizzbuzz.hrl").

%% Cowboy handler callbacks
-export([init/3,
         handle/2,
         terminate/3]).

%%===================================================================
%% Exported callbacks
%%===================================================================

init(_Type, Req, _Opts) ->
    {ok, Req, no_state}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    case Method of
        <<"GET">> ->
            process_get_request(Req2, State);
        <<"PATCH">> ->
            process_patch_request(Req2, State);
        _ ->
            {ok, Req3} = cowboy_req:reply(405, Req2),
            {ok, Req3, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%===================================================================
%% Internal Functions
%%===================================================================

process_patch_request(Req, State) ->
    case {cowboy_req:binding(number, Req), cowboy_req:has_body(Req)} of
        {{_, Req2}, false} ->
            {ok, Req3} = cowboy_req:reply(404, Req2),
            {ok, Req3, State};
        {{undefined, Req2}, _} ->
            {ok, Req3} = cowboy_req:reply(404, Req2),
            {ok, Req3, State};
        {{Number, Req2}, true} ->
            {ok, Body, Req3} = cowboy_req:body(Req2),
            {ok, Req4 } = case new_favourite_value(Body) of
                true ->
                    fizzbuzz:set_as_favourite(Number),
                    cowboy_req:reply(204, Req3);
                false ->
                    fizzbuzz:set_as_no_favourite(Number),
                    cowboy_req:reply(204, Req3);
                {error, invalid_value} ->
                    cowboy_req:reply(403, fizzbuzz_json:error_json(400, <<"Invalid resource">>,
                                                                   <<"Invalid parameter value">>), Req3)
            end,
            {ok, Req4, State}
    end.

process_get_request(Req, State) ->
    case cowboy_req:binding(number, Req) of
        {undefined, Req2} ->
            %% Here we have pagination
            {Req3, Code, Response} = process_pagination_request(Req2),
            {ok, Req4} = cowboy_req:reply(Code, [
                        {<<"content-type">>, <<"application/vnd.api+json">>}
                        ], Response, Req3),
            {ok, Req4, State};
        {Number, Req2} ->
            JsonResponse = fizzbuzz_json:top_level_numbers_json(Number),
            {ok, Req3} = cowboy_req:reply(200, [
                        {<<"content-type">>, <<"application/vnd.api+json">>}
                        ], JsonResponse, Req2),
            {ok, Req3, State}
    end.

process_pagination_request(Req) ->
    {PageNumber, Req2} = cowboy_req:qs_val(<<"page[number]">>, Req, <<"1">>),
    {PageSize, Req2} = cowboy_req:qs_val(<<"page[size]">>, Req2, <<"100">>),
    case parameters_to_integer(PageSize, PageNumber) of
        {ok, {PageSizeInteger, PageNumberInteger}} ->
            {Req2, 200, fizzbuzz_json:top_level_numbers_page_json(PageSizeInteger, PageNumberInteger)};
        {error, Reason} ->
            {Req2, 400, fizzbuzz_json:error_json(400, <<"Validation Error">>, Reason)}
    end.

parameters_to_integer(PageSize, PageNumber) ->
    try
        PageNumberInteger = binary_to_integer(PageNumber),
        PageSizeInteger = binary_to_integer(PageSize),
        valid_pagesize_number(PageSizeInteger, PageNumberInteger)
    catch
        _:badarg ->
            {error, <<"Page number and page size must be integer values >= 0.">>}
    end.

valid_pagesize_number(PageSize, _PageNumber) when PageSize =< 0 ->
    {error, <<"Page size must be > 0">>};
valid_pagesize_number(_PageSize, PageNumber) when PageNumber =< 0 ->
    {error, <<"Page number must be > 0">>};
valid_pagesize_number(PageSize, PageNumber) when (PageNumber * PageSize) > (?MAXNUMBER + PageSize) ->
    {error, <<"Page number and size values exceed the maximum number of items defined in the system">>};
valid_pagesize_number(PageSize, PageNumber) ->
    {ok, {PageSize, PageNumber}}.

new_favourite_value(Body) ->
    try
        Json = jsx:decode(Body),
        Data = proplists:get_value(<<"data">>, Json),
        Attributes = proplists:get_value(<<"attributes">>, Data),
        Value = proplists:get_value(<<"favourite">>, Attributes),
        case Value of
            <<"true">> ->
                true;
            <<"false">> ->
                false;
            _ ->
                {error, invalid_value}
        end
    catch
        _:_ ->
            %% we should log here...
            {error, invalid_value}
    end.

