-module(erlbin_table_manager).

-behaviour(gen_server).

-export([init/1,
         start_link/0,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

start_link() ->
     gen_server:start_link(?MODULE, [], []).

%%%%% gen_server callbacks

init(_) ->
    TabId = ets:new(pastes_table, [set, private, {heir, self(), no_state}]),
    give_away_message(TabId),
    {ok, no_state}.

handle_call(_Req, _From, State) ->
    {noreply, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({give_away, TabId}, State) ->
    case whereis(erlbin_table) of
        undefined ->
            io:format("erlbin_table process not registered, wait for it...~n"),
            timer:sleep(100),
            give_away_message(TabId);
        TabUserId ->
            io:format("give ets away to table user process ~p.~n", [TabId]),
            ets:give_away(TabId, TabUserId, no_state)
    end,
    {noreply, State};

%% when table owner terminates, inherit the table and give it away again
handle_info(Info = {'ETS-TRANSFER', TabId, _OldOwner, _Data}, State) ->
    io:format("table manager inherited table after process termination, ~ninfo: ~p~n",
              [Info]),
    give_away_message(TabId),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(normal, _State) -> ok.

%%%%% internal

give_away_message (TabId) ->
    self() ! {give_away, TabId}.
