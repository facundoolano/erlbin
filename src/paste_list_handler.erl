-module(paste_list_handler).

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
    Result = pastes_table:set(DecodedBody),
    req_utils:reply(Req2, Result, 201);

handle_paste(<<"GET">>, Req) ->
    req_utils:reply(Req, pastes_table:get_all());

handle_paste(_, Req) ->
    Body = #{<<"message">> => <<"Method not allowed">>},
    req_utils:reply(Req, Body, 405).
