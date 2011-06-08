%%%-------------------------------------------------------------------
%%% File    : eb_atm.erl
%%% Author  : Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%% Description : The ATM backend for ErlyBank
%%%
%%% Created :  6 Sep 2008 by Mitchell Hashimoto <mitchell.hashimoto@gmail.com>
%%%-------------------------------------------------------------------
-module(workcycle).
-include_lib("stdlib/include/qlc.hrl").
-include("dc_server.hrl").
-behaviour(gen_fsm).

%% API
-export([add_message_handler/2,
		join_workcycle/2,
		start_link/0]).

%% gen_fsm callbacks
-export([init/1, 
		waiting/2,
		waiting/3,
		startup/2, 
		startup/3, 
		reservation/2, 
		reservation/3, 
		sending/2, 
		sending/3, 
		finish_workcycle/2,
		finish_workcycle/3,
		handle_event/3,
        handle_sync_event/4, 
		handle_info/3, 
		terminate/3, 
		code_change/4]).

-define(SERVER, ?MODULE).

-define(ACCEPTED, 0).
-define(REJECTED, 1).

-define(RESERVATION_LENGTH, 12).

-record(state,  {   current_workcycle    = 0,
					participants_joining = [],
					participants_leaving = [],
					participants_active  = [],
					ticktimeout          = ?DEFAULTTICKTIMEOUT,
					rtmsgtimeout         = ?DEFAULTRTTIMEOUT}).

-record(specialStateReservation, {  normalState  = #state{},
									add_up_msg = << 0:32, 0:32, 0:32, 0:32 >>,
									round_number = 0}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%%--------------------------------------------------------------------

join_workcycle(Part, Controller) when is_record(Part, participant) and is_pid(Controller) ->
	gen_fsm:send_event(?MODULE, {joinworkcycle, {Part, Controller}});		

join_workcycle(_, _) ->
	{error, badargs}.


start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to
%% initialize.
%%--------------------------------------------------------------------
init([]) ->
	{ok, waiting, #state{}}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName,
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also
%% called if a timeout occurs.
%%--------------------------------------------------------------------

waiting({joinworkcycle, {_Part, _Controller} = PartController}, State) ->
	PartJoining = State#state.participants_joining,
	NewPartJoining = [PartController | PartJoining], 
	NewState = State#state{participants_joining = NewPartJoining},
	CurrentWorkcycle = State#state.current_workcycle,
	AList = get_active_participant_list(CurrentWorkcycle),
	case (length(AList) >= ?MIN_ACTIVE_PARTICIPANTS) of
		true -> 
			{next_state, waiting, #state{}};
		false ->
			case (length(NewPartJoining) + length(AList) >= ?MIN_ACTIVE_PARTICIPANTS) of
				true -> 
						gen_fsm:send_event(?MODULE, start),
						{next_state, startup, NewState};
				false -> 
						{next_state, waiting, NewState}
			end
	end;

waiting(leavingParticipant, State) ->
	% TODO
	{next_state, waiting, State};

waiting(_Event, State) ->
	io:format("This event should not happen in waiting state! Ingoring!~n"),
	{next_state, waiting, State}.


startup(start, State) ->
	{JPartList, JPartCons} = lists:unzip(State#state.participants_joining),
	CurrentWorkcycle = State#state.current_workcycle,
	APartRecList = get_active_participant_list(CurrentWorkcycle),
	{APartList, APartCons} = lists:unzip(APartRecList),
	AllExpectedCons = JPartCons ++ APartCons,
	AllExpectedRecs = APartRecList ++ State#state.participants_joining,
	% TODO edit timeout.
	W2WMsg = management_message:welcome2workcycle(?ACCEPTED, CurrentWorkcycle, 2000, APartList),
	send_to_participants(JPartCons, W2WMsg),		
	IUJMsg = management_message:info_update_joining_participants(JPartList, CurrentWorkcycle),
	send_to_participants(AllExpectedCons, IUJMsg),
	set_participants_active(JPartList, CurrentWorkcycle),
	% TODO leaving connections
	receive 
		after ?DEFAULTTICKTIMEOUT ->
			true
	end,
	TickMsg = management_message:tick(CurrentWorkcycle),	
	lists:foreach(fun(X) -> spawn(?MODULE, add_message_handler, [State#state.rtmsgtimeout, X]) end, AllExpectedRecs),
	send_to_participants(AllExpectedCons, TickMsg),
	ReservationState = #specialStateReservation{normalState = State},
	{next_state, reservation, ReservationState};

startup({joinworkcycle, {_Part, _Controller} = PartController}, State) ->
	NewPartJoining = [PartController | State#state.participants_joining],
	NewState = State#state{participants_joining = NewPartJoining},
	{next_state, startup, NewState};

startup(leavingParticipant, State) ->
	% TODO
	{next_state, startup, State};

startup(Event, State) ->
	io:format("Unknown event for state startup. Ignoring ~w!~n",[Event]),
	{next_state, startup, State}.


reservation({add, {_Part, Controller} = APart, 
				WorkcycleNumber, Roundnumber, <<AddMsg:96>>}, State) ->
	io:format("Add message arrived for ~w!~n",[Controller]),
	{next_state, reservation, State};

reservation({joinworkcycle, {_Part, _Controller} = PartController}, State) ->
	NormalState = State#specialStateReservation.normalState,
	NewPartJoining = [PartController | NormalState#state.participants_joining],
	NewNormalState = NormalState#state{participants_joining = NewPartJoining},
	NewState = State#specialStateReservation{normalState = NewNormalState},
	{next_state, startup, NewState};

reservation(_Event, State) ->
	{next_state, reservation, State}.

sending(_Event, State) ->
	{next_state, sending, State}.

finish_workcycle(_Event, State) ->
	{next_state, finish_workcycle, State}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName,
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName,
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
waiting(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, waiting, State}.

startup(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, startup, State}.

reservation(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, reservation, State}.

sending(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, reservation, State}.

finish_workcycle(_Event, _From, State) ->
  Reply = ok,
  {reply, Reply, reservation, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_event(Event, StateName, State) -> {next_state, NextStateName,
%%                                                NextState} |
%%                                          {next_state, NextStateName,
%%                                                NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_sync_event(Event, From, StateName,
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState,
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState,
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState,
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

send_to_participants(PartConList, Msg) when is_list(PartConList) ->
	lists:foreach( fun(X) -> X ! {forward, Msg} end, PartConList),
	ok;
send_to_participants(_PList, _Msg) ->
	ok.

add_message_handler(Timeout, {Part, Controller}) when is_record(Part, participant) ->
	Controller ! {add_message_handler, self()},
	link(Controller),
	receive 
		{add, {Part, Controller}, WorkcycleNumber, RoundNumber, Msg} ->
			gen_fsm:send_event(?MODULE, {add, {Part, Controller}, WorkcycleNumber, RoundNumber, Msg}),
			add_message_handler(Timeout, {Part, Controller})
		after Timeout ->
			gen_fsm:send_event(?MODULE, {addtimeout, {Part, Controller}}),
			add_message_handler(Timeout, {Part, Controller})
	end;
add_message_handler(Error1, Error2) ->
	io:format("this is not the added handler ~w ~w ~n",[Error1, Error2]),
	badarg.

get_active_participant_list(CurrentWorkcycle) ->
	AT = fun() ->
			qlc:e(
			  qlc:q([ 1 || X <- mnesia:table(participant_mgmt), 
									X#participant_mgmt.active_from =< CurrentWorkcycle,
									X#participant_mgmt.active_from >= 0
									]))
		end,
	{atomic, AList} = mnesia:transaction(AT),
	AList.

set_participants_active(PartList, ForWhichWorkcycle) ->
	SAT = fun() ->
				PartRecs = lists:flatmap(fun(X) -> 
									 	mnesia:read({participant_mgmt, X})
									end, PartList),
				lists:foreach(fun(X) ->
								NPMI = X#participant_mgmt{active_from=ForWhichWorkcycle},
								mnesia:write(NPMI) end, 
								PartRecs) 
			end,
	case mnesia:transaction(SAT) of
		{atomic, ok} -> ok;
		Error -> 
			io:format("There was something wrong with setting the participant active ~w~n",[Error]),
			false
	end.
