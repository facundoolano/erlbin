-module(erlbin_list_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%%====================================================================
%% API
%%====================================================================

init(_, Req, _Opts) ->
    {ok, Req, no_state}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req3} = handle_paste(Method, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

handle_paste(<<"POST">>, Req) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    % TODO validate input fields
    DecodedBody = jiffy:decode(Body, [return_maps]),
    Result = erlbin_table:set(DecodedBody),
    erlbin_utils:reply(Req2, Result, 201);

handle_paste(<<"GET">>, Req) ->
    erlbin_utils:reply(Req, erlbin_table:get_all());

handle_paste(_, Req) ->
    Body = #{<<"message">> => <<"Method not allowed">>},
    erlbin_utils:reply(Req, Body, 405).
