-module(erlbin_notificator).

-behaviour(gen_server).

-define(SUBSCRIBERS, ?MODULE).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init_subscribers/0,
         subscribe/0,
         unsubscribe/0,
         publish/2,

         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Action, Data) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Action, Data], []).

%%%% pubsub API

init_subscribers () ->
    ets:new(?SUBSCRIBERS, [set, named_table, public]).

subscribe() ->
    ets:insert(?SUBSCRIBERS, {self()}).

unsubscribe() ->
    ets:delete(?SUBSCRIBERS, self()).

publish(Action, Data) ->
    supervisor:start_child(erlbin_notificator_sup, [Action, Data]).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Action, Data]) ->
    io:format("starting notification worker~n"),
    self() ! {publish, Action, Data},
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({publish, Action, Data}, State) ->
    io:format("sending notifications~n"),
    SendMsg = fun ({Pid}, _Acc) ->
                      Pid ! {table_action, Action, Data}
              end,
    ets:foldl(SendMsg, acc0, ?SUBSCRIBERS),
    {stop, normal, State}.

terminate(_Reason, _State) ->
    io:format("killing notification worker~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
