-module(erlbin_utils).

-export([reply/2, reply/3]).

reply(Req, Body) -> reply(Req, Body, 200).
reply(Req, Body, Status) ->
    cowboy_req:reply(Status,
                     [{<<"content-type">>, <<"application/json">>}],
                     jiffy:encode(Body),
                     Req).
