%%===================================================================
%% @doc This module implements the Cowboy request handler behaviour
%% @end
%%===================================================================
-module(fizzbuzz_handler).

-define(MAXNUMBER, 100000000000).

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
        %% TODO: Process PATCH for updates of resource!
        _ ->
            {ok, Req3} = cowboy_req:reply(405, Req2),
            {ok, Req3, State}
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%===================================================================
%% Internal Functions
%%===================================================================

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
    %% TODO: validate pagination paramters.
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
valid_pagesize_number(PageSize, PageNumber) when (PageNumber * PageSize) > ?MAXNUMBER ->
    {error, <<"Page number and size values exceed the maximum number of items defined in the system">>};
valid_pagesize_number(PageSize, PageNumber) ->
    {ok, {PageSize, PageNumber}}.

