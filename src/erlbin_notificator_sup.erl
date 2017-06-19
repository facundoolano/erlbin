-module(erlbin_notificator_sup).

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
    erlbin_notificator:init_subscribers(),
    {ok, { #{ strategy => simple_one_for_one, intensity => 0, period => 1 },
           [#{
               id => erlbin_notificator,
               start => {erlbin_notificator, start_link, []},
               restart => temporary,
               shutdown => 5000,
               type => worker,
               modules => [erlbin_notificator]
             }]
         }}.

%%====================================================================
%% Internal functions
%%====================================================================
