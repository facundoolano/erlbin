%%%-------------------------------------------------------------------
%% @doc erlbin public API
%% @end
%%%-------------------------------------------------------------------

-module(erlbin_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
                                      %% TODO add home html
                                      {'_', [{"/api/pastes", erlbin_list_handler, []},
                                             {"/api/pastes/:id/", [{id, int}], erlbin_detail_handler, []}]}
                                     ]),
    cowboy:start_http(my_http_listener, 100, [{port, 8080}],
                      [{env, [{dispatch, Dispatch}]}]
                     ),
    erlbin_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
