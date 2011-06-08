
-module(erlangDC_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
 	Server             = ?CHILD(dc_server, worker),
 	ParticipantManager = ?CHILD(participant_manager, worker),
	RoundManagerSup    = ?CHILD(round_manager_sup, supervisor), 
    {ok, { {one_for_all, 4, 10}, [Server, ParticipantManager, RoundManagerSup]} }.

