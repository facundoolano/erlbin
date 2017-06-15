-module(paste_detail_handler).

-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%%====================================================================
%% API
%%====================================================================

init(_, Req, _Opts) ->
    {Id, Req2} = cowboy_req:binding(id, Req),
    State = #{id => Id},
    {ok, Req2, State}.

handle(Req, State=#{id := Id}) ->
    {Method, Req2} = cowboy_req:method(Req),
    {ok, Req3} = handle_paste(Method, Id, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% FIXME handle not found
handle_paste(<<"GET">>, Id, Req) ->
    req_utils:reply(Req, pastes_table:get(Id));

handle_paste(_Method, _Id, Req) ->
    Body = #{<<"message">> => <<"Method not allowed">>},
    req_utils:reply(Req, Body, 405).
