-module(erlbin_table).

-behaviour(gen_server).

-define(TABLE, ?MODULE).

-export([set/1,
         set/2,
         exists/1,
         get/1,
         delete/1,
         get_all/0,

         init/1,
         start_link/0,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

start_link() -> gen_server:start_link({local, ?TABLE}, ?MODULE, [], []).

%%%%% table API

set(Data) ->
    Id = erlang:unique_integer([positive]),
    Term = {Id, Data},
    true = ets:insert(?TABLE, Term),
    Result = Data#{id => Id},
    erlbin_notificator:publish(create, Result),
    Result.

set(Id, Data) ->
    Term = {Id, Data},
    true = ets:insert(?TABLE, Term),
    Result = Data#{id => Id},
    erlbin_notificator:publish(update, Result),
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
    erlbin_notificator:publish(delete, Id).

get_all() ->
    [Data#{id => Id} || {Id, Data} <- ets:tab2list(?TABLE)]. %% I know, I know

%%%%% gen_server callbacks

init(_) ->
    ets:new(?TABLE, [set, named_table, public]),
    {ok, #{subscribers => []}}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(normal, _State) -> ok.
