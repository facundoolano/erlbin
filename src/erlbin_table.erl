-module(erlbin_table).

-behaviour(gen_server).

-define(TABLE, ?MODULE).

-export([set/1, set/2]).
-export([exists/1]).
-export([get/1]).
-export([delete/1]).
-export([get_all/0]).

-export([subscribe/0]).
-export([unsubscribe/0]).

-export([init/1]).
-export([start_link/0]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

start_link() -> gen_server:start_link({local, ?TABLE}, ?MODULE, [], []).

%%%%% gen_server callbacks

init(_) ->
    ets:new(?TABLE, [set, named_table, public]),
    {ok, #{subscribers => []}}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

%% add a Pid to the subscribers list
handle_cast({subscribe, Pid}, State=#{subscribers := Subs}) ->
    NewState = State#{subscribers := [Pid | Subs]},
    {noreply, NewState};

%% remove a Pid from the subscribers list
handle_cast({unsubscribe, Pid}, State=#{subscribers := Subs}) ->
    NewState = State#{subscribers := lists:delete(Pid, Subs)},
    {noreply, NewState};

%% publish an table action to all subscribers
handle_cast({publish, Action, Data}, State=#{subscribers := Subs}) ->
    [Pid ! {table_action, Action, Data} || Pid <- Subs],
    {noreply, State};

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(normal, _State) -> ok.

%%%%% table API
set(Data) ->
    Id = erlang:unique_integer([positive]),
    Term = {Id, Data},
    true = ets:insert(?TABLE, Term),
    Result = Data#{id => Id},
    publish(create, Result),
    Result.

set(Id, Data) ->
    Term = {Id, Data},
    true = ets:insert(?TABLE, Term),
    Result = Data#{id => Id},
    publish(update, Result),
    Result.

get(Id) ->
    case ets:lookup(?TABLE, Id) of
        [{Id, Data}] -> Data#{id => Id};
        [] -> throw(not_found)
    end.

exists(Id) ->
    case ets:lookup(?TABLE, Id) of
        [] -> false;
        _ -> true
    end.

delete(Id) ->
    ets:delete(?TABLE, Id),
    publish(delete, Id).

get_all() ->
    [Data#{id => Id} || {Id, Data} <- ets:tab2list(?TABLE)]. %% I know, I know

%% pub sub API

subscribe() ->
    gen_server:cast(?TABLE, {subscribe, self()}).

unsubscribe() ->
    gen_server:cast(?TABLE, {unsubscribe, self()}).

%% used internally by the functions that change the table
publish(Action, Data) ->
    gen_server:cast(?TABLE, {publish, Action, Data}).
