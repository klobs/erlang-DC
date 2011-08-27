-module(workcycle_evt_mgr).

%% API
-export([
		add_handler/1, 
		dump_to_csv/0,
		notify/1,
		start_link/0]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------

dump_to_csv() ->
	workcycle_evt_mgr:notify(dump_to_csv).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | {error,Error}
%% Description: Creates an event manager.
%%--------------------------------------------------------------------
start_link() ->
  gen_event:start_link({local, ?SERVER}). 

%%--------------------------------------------------------------------
%% Function: add_handler(Module) -> ok | {'EXIT',Reason} | term()
%% Description: Adds an event handler
%%--------------------------------------------------------------------
add_handler(Module) ->
  gen_event:add_handler(?SERVER, Module, []).

%%--------------------------------------------------------------------
%% Function: notify(Event) -> ok | {error, Reason}
%% Description: Sends the Event through the event manager.
%%--------------------------------------------------------------------
notify(Event) ->
  gen_event:notify(?SERVER, Event).

