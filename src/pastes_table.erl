-module(pastes_table).

-behaviour(gen_server).

-define(TABLE, ?MODULE).

-export([set/1]).
-export([get/1]).
-export([get_all/0]).

-export([init/1]).
-export([start_link/0]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

start_link() -> gen_server:start_link(?MODULE, [], []).

%% gen_server callbacks

init(_) ->
    ets:new(?TABLE, [set, named_table, public]),
    {ok, []}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
     {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(normal, _State) -> ok.

%% table API
set(Data) ->
    Id = erlang:unique_integer([positive]),
    Term = {Id, Data},
    true = ets:insert(?TABLE, Term),
    Data#{<<"id">> => Id}.

get(Id) ->
    [{Id, Data}] = ets:lookup(?TABLE, Id),
    Data#{<<"id">> => Id}.

get_all() ->
    [Data#{<<"id">> => Id} || {Id, Data} <- ets:tab2list(?TABLE)]. %% I know, I know
