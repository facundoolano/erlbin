-module(erlbin_detail_handler).

-export([init/3]).
-export([rest_init/2]).
-export([allowed_methods/2]).
-export([resource_exists/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([to_json/2]).
-export([from_json/2]).
-export([delete_resource/2]).

init(_Transport, _Req, []) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
    {Id, Req2} = cowboy_req:binding(id, Req),
    State = #{id => Id},
    {ok, Req2, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"PUT">>, <<"DELETE">>],
     Req, State}.

resource_exists(Req, State=#{id := Id}) ->
    {erlbin_table:exists(Id), Req, State}.

%% TODO add html and text
content_types_provided(Req, State) ->
    {[
      %% {<<"text/html">>, hello_to_html},
      {<<"application/json">>, to_json}
      %% {<<"text/plain">>, hello_to_text}
     ], Req, State}.

%% FIXME what if not found here?
to_json(Req, State=#{id := Id}) ->
    Body = jiffy:encode(erlbin_table:get(Id)),
    {Body, Req, State}.

content_types_accepted(Req, State) ->
    {[
      %% {<<"text/html">>, hello_to_html},
      {<<"application/json">>, from_json}
      %% {<<"text/plain">>, hello_to_text}
     ], Req, State}.

%% for put handling
from_json(Req, State=#{id := Id})->
    {ok, Body, Req2} = cowboy_req:body(Req),
    %% TODO validate input fields
    DecodedBody = jiffy:decode(Body, [return_maps]),
    erlbin_table:set(Id, DecodedBody),
    Req3 = cowboy_req:set_resp_body(Body, Req2),
    {true, Req3, State}.

delete_resource(Req, State=#{id := Id}) ->
    erlbin_table:delete(Id),
    {true, Req, State}.
