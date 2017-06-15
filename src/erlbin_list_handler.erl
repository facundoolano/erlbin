-module(erlbin_list_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([to_json/2]).
-export([from_json/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>], Req, State}.

%% TODO add html and text
content_types_provided(Req, State) ->
    {[
      %% {<<"text/html">>, hello_to_html},
      {<<"application/json">>, to_json}
      %% {<<"text/plain">>, hello_to_text}
     ], Req, State}.

to_json(Req, State) ->
    Body = jiffy:encode(erlbin_table:get_all()),
    {Body, Req, State}.

%% TODO add html and text
content_types_accepted(Req, State) ->
    {[
      %% {<<"text/html">>, hello_to_html},
      {<<"application/json">>, from_json}
      %% {<<"text/plain">>, hello_to_text}
     ], Req, State}.

from_json(Req, State) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    % TODO validate input fields
    DecodedBody = jiffy:decode(Body, [return_maps]),
    #{id := Id} = erlbin_table:set(DecodedBody),
    BinId = erlang:integer_to_binary(Id),
    {{true,  <<"/api/pastes/", BinId/binary>>},  Req2, State}.
