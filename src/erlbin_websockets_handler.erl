-module(erlbin_websockets_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3,
         websocket_init/3,
         websocket_terminate/3,
         websocket_info/3,
         websocket_handle/3]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Type, Req, _Opts) ->
    io:format("websocket connected!~n"),
    erlbin_table:subscribe(),
    {ok, Req, no_state, 60000}.

websocket_terminate(_Reason, _Req, _State) ->
    io:format("websocket disconnected.~n"),
    erlbin_table:unsubscribe(),
    ok.

websocket_info({table_action, Action, Data}, Req, State) ->
    Message = jiffy:encode(#{action => Action, data => Data}),
    io:format("Sending message to socket ~p~n", [Message]),
    {reply, {text, Message}, Req, State}.

websocket_handle(_Frame, Req, State) ->
    {ok, Req, State}.
