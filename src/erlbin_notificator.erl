-module(erlbin_notificator).

-behaviour(gen_server).

-define(SUBSCRIBERS, ?MODULE).

-export([init_subscribers/0,
         subscribe/0,
         unsubscribe/0,
         publish/2,

         start_link/0,

         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%% pubsub API

init_subscribers () ->
    ets:new(?SUBSCRIBERS, [set, named_table, public]).

subscribe() ->
    ets:insert(?SUBSCRIBERS, {self()}).

unsubscribe() ->
    ets:delete(?SUBSCRIBERS, self()).

publish(Action, Data) ->
    {ok, Pid} = supervisor:start_child(erlbin_notificator_sup, []),
    gen_server:cast(Pid, {publish, Action, Data}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    io:format("starting notification worker~n"),
    {ok, no_state}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({publish, Action, Data}, State) ->
    io:format("sending notifications~n"),
    SendMsg = fun ({Pid}, _Acc) ->
                      Pid ! {table_action, Action, Data}
              end,
    ets:foldl(SendMsg, acc0, ?SUBSCRIBERS),
    {stop, normal, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("killing notification worker~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
