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
    pastes_table:set(DecodedBody),
    reply(Req2, DecodedBody, 201);

handle_paste(<<"GET">>, Req) ->
    All = [Data#{<<"id">> => Id} || {Id, Data} <- pastes_table:get_all()],
    io:format("this is json ~p", [All]),
    reply(Req, All);

handle_paste(_, Req) ->
    reply(Req, {[{message, <<"Method not allowed">>}]}, 405).

%% TODO move to utils file
reply(Req, Body) -> reply(Req, Body, 200).
reply(Req, Body, Status) ->
    cowboy_req:reply(Status,
                     [{<<"content-type">>, <<"application/json">>}],
                     jiffy:encode(Body),
                     Req).
