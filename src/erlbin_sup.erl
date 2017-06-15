%%%-------------------------------------------------------------------
%% @doc erlbin top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erlbin_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, { #{ strategy => one_for_all, intensity => 0, period => 1 },
           [#{
               id => erlbin_table,
               start => {erlbin_table, start_link, []},
               restart => permanent,
               shutdown => 5000,
               type => worker,
               modules => [erlbin_table]
             }]
         }}.

%%====================================================================
%% Internal functions
%%====================================================================
