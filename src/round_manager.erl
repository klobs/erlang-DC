%%%-------------------------------------------------------------------
%%% File    : eb_server.erl
%%% Author  : Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%% Description : The ErlyBank account server.
%%%
%%% Created :  5 Sep 2008 by Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%%-------------------------------------------------------------------
-module(round_manager).
-include_lib("stdlib/include/qlc.hrl").
-include("dc_server.hrl").
-behaviour(gen_server).

%% API
-export([join_workcycle/2,
		start_link/0,
		start_next_workcycle/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {current_workcycle= 0,
				participants_joining = [],
				participants_leaving = []}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

join_workcycle(Part, Controller) when is_record(Part, participant) and is_pid(Controller) ->
	gen_server:cast(?MODULE, {joinworkcycle, {Part, Controller}});		
join_workcycle(_, _) ->
	{error, badargs}.

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_next_workcycle() ->
	gen_server:cast(?MODULE, start_next_workcycle).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({joinworkcycle, {_Part, _Controller} = PartController}, State) ->
	PartJoining = State#state.participants_joining,
	NewPartJoining = [PartController | PartJoining], 
	NewState = State#state{participants_joining = NewPartJoining},
	CurrentWorkCycle = State#state.current_workcycle,
	AT = fun() ->
			qlc:e(
			  qlc:q([ 1 || X <- mnesia:table(participant_mgmt), 
									X#participant_mgmt.active_from =< CurrentWorkCycle,
									X#participant_mgmt.active_from >= 0
									]))
		end,
	{atomic, AList} = mnesia:transaction(AT),
	case (length(AList) >= ?MIN_ACTIVE_PARTICIPANTS) of
		true -> 
			{noreply, NewState};
		false ->
			case (length(NewPartJoining) >= ?MIN_ACTIVE_PARTICIPANTS) of
				true -> start_next_workcycle(), 
						{noreply, NewState};
				false -> 
						{noreply, NewState}
				end
		end;

handle_cast(start_next_workcycle, State) ->
	io:format("Starting next workcycle"),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
