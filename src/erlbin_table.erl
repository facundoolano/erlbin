-module(erlbin_table).

-behaviour(gen_server).

-define(SERVER, ?MODULE).
-define(SUBSCRIBERS, paste_subscribers).

-export([set/1,
         set/2,
         exists/1,
         get/1,
         delete/1,
         get_all/0,

         subscribe/0,
         unsubscribe/0,

         init/1,
         start_link/0,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%%% table API

set(Data) ->
    {ok, Result} = gen_server:call(?SERVER, {set, Data}),
    publish(create, Result),
    Result.

set(Id, Data) ->
    {ok, Result} = gen_server:call(?SERVER, {set, Id, Data}),
    publish(update, Result),
    Result.

get(Id) ->
    case gen_server:call(?SERVER, {get, Id}) of
        {ok, Result} -> Result;
        {error, Reason} -> throw(Reason)
    end.

exists(Id) ->
    {ok, Result} = gen_server:call(?SERVER, {exists, Id}),
    Result.

delete(Id) ->
    ok = gen_server:call(?SERVER, {delete, Id}),
    publish(delete, Id).

get_all() ->
    {ok, Result} = gen_server:call(?SERVER, {get_all}),
    Result.

%%%% pubsub API

subscribe() ->
    syn:join(?SUBSCRIBERS, self()).

unsubscribe() ->
    syn:leave(?SUBSCRIBERS, self()).

%% used internally by the functions that change the table
publish(Action, Data) ->
    syn:publish(?SUBSCRIBERS, {table_action, Action, Data}).

%%%%% gen_server callbacks

init(_) ->
    {ok, no_state}.

handle_call({set, Data}, _From, TabId) ->
    Id = erlang:unique_integer([positive]),
    Term = {Id, Data},
    true = ets:insert(TabId, Term),
    Result = {ok, Data#{id => Id}},
    {reply, Result, TabId};

handle_call({set, Id, Data}, _From, TabId) ->
    Term = {Id, Data},
    true = ets:insert(TabId, Term),
    Result = {ok, Data#{id => Id}},
    {reply, Result, TabId};

handle_call({get, Id}, _From, TabId) ->
    Result = case ets:lookup(TabId, Id) of
                 [{Id, Data}] -> {ok, Data#{id => Id}};
                 [] -> {error, not_found}
             end,
    {reply, Result, TabId};

handle_call({exists, Id}, _From, TabId) ->
    Result = case ets:lookup(TabId, Id) of
                 [] -> {ok,  false};
                 _ -> {ok, true}
             end,
    {reply, Result, TabId};

handle_call({delete, Id}, _From, TabId) ->
    ets:delete(TabId, Id),
    {reply, ok, TabId};

handle_call({get_all}, _From, TabId) ->
    Result = [Data#{id => Id} || {Id, Data} <- ets:tab2list(TabId)], %% I know, I know
    {reply, {ok, Result}, TabId};

handle_call(Req, _From, State) ->
    io:format("unknown message ~p~n", Req),
    {noreply, State}.

handle_cast(_Req, State) ->
    {noreply, State}.

%% receive TabId when the table manager gives it away
handle_info({'ETS-TRANSFER', TabId, _OldOwner, _Data}, _State) ->
    io:format("received table from table manager, table: ~p elements: ~p.~n",
              [TabId, ets:tab2list(TabId)]),
    {noreply, TabId};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(normal, _State) -> ok.
