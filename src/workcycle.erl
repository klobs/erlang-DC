-module(workcycle).
-compile([debug_info]).
-include_lib("stdlib/include/qlc.hrl").
-include("dc_server.hrl").
-behaviour(gen_fsm).

%% API
-export([ join_workcycle/2,
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

-define(RESERVATION_LENGTH , 12).
-define(MODULO             , 16#100000000).

-record(state,{     %Needed for every state   
	   				current_workcycle                    = 0,
					participants_joining                 = [],
					participants_leaving                 = [],
					rtmsgtimeout                         = ?DEFAULTRTTIMEOUT,
					ticktimeout                          = ?DEFAULTTICKTIMEOUT,
					%Needed for reservation and sinding:
					add_up_msg                           = << 0:96 >>,
					current_round_number                 = 0,
					individual_message_lengths           = [],
					participants_expected                = [],
					participants_confirmed               = [],
					rounds_expected                      = -1
				}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.
%%--------------------------------------------------------------------

join_workcycle(Part, Controller) when 
						is_record(Part, participant) and is_pid(Controller) ->
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
	io:format("[waiting]: new participant joining our workcycles~n"),
	NewPartJoining = [PartController | State#state.participants_joining], 
	NewState = State#state{participants_joining = NewPartJoining},
	CurrentWorkcycle = State#state.current_workcycle,
	AList = generic_get_active_participant_list(CurrentWorkcycle),
	case (length(AList) >= ?MIN_ACTIVE_PARTICIPANTS) of
		true -> 
			%% this can not be true and we have to restart
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
	io:format("[waiting]: This event should not happen in waiting state! Ingoring!~n"),
	{next_state, waiting, State}.


startup(start, State) ->
	io:format("[startup]: entering startup state~n"),
	{JPartL, JConsL} = lists:unzip(State#state.participants_joining),
	CurrentWorkcycle = State#state.current_workcycle,
	io:format("[startup]: current workcycle is ~w~n",[CurrentWorkcycle]),
	APartConsL = generic_get_active_participant_list(CurrentWorkcycle),
	{APartL, AConsL} = lists:unzip(APartConsL),
	NExpdConsL = JConsL ++ AConsL,
	NExpdPartConsL = APartConsL ++ State#state.participants_joining,
	startup_send_w2wc(CurrentWorkcycle, APartL, JConsL),
	startup_send_iujp(JPartL, CurrentWorkcycle, NExpdConsL),
	startup_set_participants_active(JPartL, CurrentWorkcycle),
	% TODO leaving connections
	receive 
		after ?DEFAULTTICKTIMEOUT ->
			true
	end,
	startup_send_tick(NExpdConsL, CurrentWorkcycle, State#state.rtmsgtimeout),
	io:format("[startup]: entering reservation state~n"),
	ReservationState = #state{
		current_workcycle = State#state.current_workcycle,
		participants_expected=NExpdPartConsL, 
		participants_joining=[]},
	{next_state, reservation, ReservationState};

startup({joinworkcycle, {_Part, _Controller} = PartController}, State) ->
	io:format("[startup]: New participant joining our workcycles"),
	NewPartJoining = [PartController | State#state.participants_joining],
	NewState = State#state{participants_joining = NewPartJoining},
	{next_state, startup, NewState};

startup(leavingParticipant, State) ->
	% TODO
	{next_state, startup, State};

startup(Event, State) ->
	io:format("[startup]: Unknown event for state startup. Ignoring ~w!~n",[Event]),
	{next_state, startup, State}.


reservation({add, {part, P}, {con, C}, {wcn, W}, {rn, R}, {addmsg, <<AddMsg:96>>}}, State) 
		 		when 
				(W == State#state.current_workcycle) and 
				(R == State#state.current_round_number) ->
	io:format("[reservation]: Add message arrived for ~w ~w~n",[C,AddMsg]),
	NLocalSum = generic_add_up_dcmsg(State#state.add_up_msg, <<AddMsg:96>>),
	NExpdPartConsL = lists:delete({P,C}, State#state.participants_expected),
	NConfPartConsL = [{P,C}| State#state.participants_confirmed],
	case length(NExpdPartConsL) == 0 of
		true -> 
			io:format("[reservation]: broadcast and new round~n"),
			AddedMsg = management_message:added(State#state.current_workcycle, 
											State#state.current_round_number, NLocalSum),
			{_, NConfConsL} = lists:unzip(NConfPartConsL),
			generic_send_to_participants(NConfConsL, AddedMsg),
			io:format("[reservation]: Checking if reservation has finished?~n"),
			case reservation_evaluate_reservation(State#state.rounds_expected, 
							State#state.individual_message_lengths, AddedMsg) of
				{not_finished} ->	%% Weitere Reservierungsrunde
					io:format("[reservation]: Not finished, just continuing~n"),
					NewState = State#state{
						add_up_msg             = << 0:96 >>,
						participants_expected  = NConfPartConsL,
						participants_confirmed = [],
						current_round_number   = State#state.current_round_number +1},
					{next_state, reservation, NewState};
				{not_finished, {ec,EC}} -> 
					io:format("[reservation]: Not finished, but at least we know how many rounds we are probably going to take: ~w~n",[EC]),
					NewState = State#state{
						add_up_msg             = << 0:96 >>,
						participants_expected  = NConfPartConsL,
						participants_confirmed = [],
						rounds_expected        = EC,
						current_round_number   = State#state.current_round_number +1},
					{next_state, reservation, NewState};
				{not_finished, {iml, IML}} ->
					io:format("[reservation]: Not finished, but there is at least one new message length collected.~n"),
					NewState = State#state{
						add_up_msg                 = << 0:96 >>,
						participants_expected      = NConfPartConsL,
						participants_confirmed     = [],
						individual_message_lengths = IML,
						current_round_number       = State#state.current_round_number +1},
					{next_state, reservation, NewState};
				{finished, {iml,[]}} -> %% Es gibt keine Teilnehmer -> neuer Workcycle
					io:format("Reservation finished - no participant wanted to send -> next workcycle~n"),
					NewState = #state{
							current_workcycle     = W + 1,
							rtmsgtimeout          = State#state.rtmsgtimeout,
							ticktimeout           = State#state.ticktimeout,
							participants_expected = NConfPartConsL,
							participants_joining  = State#state.participants_joining,
							participants_leaving  = State#state.participants_leaving
						},
					gen_fsm:send_event(?MODULE, start),
					{next_state, startup, NewState};
				{finished, {iml, [NextMsgLengthBytes|RestMessages]}} ->
					io:format("Reservation finished -> next round~n"),
					generic_send_to_connections(NConfConsL, 
						{ 
						wait_for_realtime_msg, {wcn, W}, 
						{rn, R}, {timeout, State#state.rtmsgtimeout}}),
						NextMsgLengthBits = NextMsgLengthBytes * 8,	
					NewState = State#state{
						add_up_msg                 = <<0:NextMsgLengthBits>>,
						participants_expected      = NConfPartConsL,
						participants_confirmed     = [],
						individual_message_lengths = RestMessages},
					{next_state, sending, NewState}
			end;
		_ ->
			{next_state, reservation, State#state{add_up_msg = NLocalSum, 
											participants_expected = NExpdPartConsL, 
											participants_confirmed = NConfPartConsL}}
	end;

%% TODO Timeout
%reservation({addtimeout, {part, P}, {con, C}, {wcn, W}, {rn, R}}, State) ->
reservation({addtimeout, _, _, _, _}, State) ->
	io:format("[reservation]: Todo implement handling for timeouts in reservation~n"),
	{next_state, reservation, State};

reservation({joinworkcycle, {_Part, _Controller} = PartController}, State) ->
	io:format("[reservation]: new participant joining our workcycles~n"),
	NewPartJoining = [PartController | State#state.participants_joining],
	NewState = State#state{participants_joining = NewPartJoining},
	{next_state, startup, NewState};

reservation(Event, State) ->
	io:format("[reservation]: dont know how to handle event ~w in state ~w~n",[Event, State]),
	{next_state, reservation, State}.

sending({add, {part, P}, {con, C}, {wcn, W}, {rn, R}, {addmsg, AddMsg}}, State)
		 		when 
				is_binary(AddMsg) and
				(size(AddMsg) == size(State#state.add_up_msg)) and
				(W == State#state.current_workcycle) and 
				(R == State#state.current_round_number) ->
	io:format("[sending]: Add message arrived for ~w ~w~n",[C,AddMsg]),
	NLocalSum = generic_add_up_dcmsg(State#state.add_up_msg, AddMsg),
	NExpdPartConsL = lists:delete({P,C}, State#state.participants_expected),
	NConfPartConsL = [{P,C}| State#state.participants_confirmed],
	case length(NExpdPartConsL) == 0 of
		true -> 
			io:format("[sending]: broadcast and new round~n"),
			AddedMsg = management_message:added(State#state.current_workcycle, 
											State#state.current_round_number, NLocalSum),
			io:format("[sending]: Unzipping lists~n"),
			{_, NConfConsL} = lists:unzip(NConfPartConsL),
			io:format("[sending]: distributing added message~n"),
			generic_send_to_participants(NConfConsL, AddedMsg),
			io:format("[sending]: Checking whether workcycle has finished..."),
			case length(State#state.individual_message_lengths) of
				0 ->
					io:format("...yup: no more new messages, starting new workcycle"),
					NState = #state{ current_workcycle    = W + 1,
									rtmsgtimeout          = State#state.rtmsgtimeout,
									ticktimeout           = State#state.ticktimeout,
									participants_expected = NConfPartConsL,
									participants_joining  = State#state.participants_joining,
									participants_leaving  = State#state.participants_leaving
										},
					gen_fsm:send_event(?MODULE, start),
					{next_state, startup, NState};
				L ->
					io:format("... nope: there are ~w further rounds expectd~n",[L]),
					[NextMsgLengthBytes| RestMsgL] = State#state.individual_message_lengths,
					NextMsgLengthBits = NextMsgLengthBytes *8,
					NState = State#state{   individual_message_lengths = RestMsgL,
											add_up_msg = <<0:NextMsgLengthBits>>,
											current_round_number = R + 1,
											participants_confirmed = [],
											participants_expected = NConfPartConsL
										},
					{next_state, sening, NState}
			end;
		false ->
			NState = State#state{   add_up_msg = NLocalSum,
									participants_confirmed = NConfPartConsL,
									participants_expected = NExpdPartConsL},
			{next_state, sending, NState}
	end;

sending(Event, State) ->
	io:format("[sending]: don't know propper reaction for ~w in ~w state~n", [Event, State]),
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

generic_send_to_participants([], _Msg) ->
	ok;
generic_send_to_participants(PartConList, Msg) when is_list(PartConList) ->
	io:format("[generic_send]: send arguemtns received: ~w ~w~n",[PartConList, Msg]),
	lists:foreach( fun(X) -> 
				%io:format("forwarding ~w to ~w~n",[Msg, X]),
				X ! {forward_to_participant, {msg, Msg}} end, PartConList),
	ok;
generic_send_to_participants(_PList, _Msg) ->
	ok.

generic_send_to_connections([], _Msg) ->
	ok;
generic_send_to_connections(ConsL, Msg) when is_list(ConsL) ->
	lists:foreach( fun(X) -> 
				X ! Msg end, ConsL),
	ok;
generic_send_to_connections(_PList, _Msg) ->
	ok.

generic_add_up_dcmsg(A,B) ->
	generic_add_up_dcmsg(A, B, []).

generic_add_up_dcmsg(<<AH:32, AT/binary>>, <<BH:32, BT/binary>>, CList)->
	Remainder= (AH + BH) rem ?MODULO,
	generic_add_up_dcmsg(AT, BT, [<<Remainder:32>> | CList]);
generic_add_up_dcmsg(<<>>, <<>>, CList) ->
	RList = lists:reverse(CList),
	list_to_binary(RList).



generic_get_active_participant_list(CurrentWorkcycle) ->
	AT = fun() ->
			qlc:e(
				qlc:q([ {X#participant_mgmt.participant, X#participant_mgmt.controller} || 
									X <- mnesia:table(participant_mgmt), 
									X#participant_mgmt.active_from =< CurrentWorkcycle,
									X#participant_mgmt.active_from >= 0
									]))
		end,
	{atomic, AList} = mnesia:transaction(AT),
	AList.

startup_send_iujp(_JP, _CurrentWorkcycle, []) -> ok;
startup_send_iujp([], _CurrentWorkcycle, _ExptConsL) -> ok;
startup_send_iujp(JPartL, CurrentWorkcycle, ExptConsL) ->
	IUJMsg = management_message:info_update_joining_participants(JPartL, CurrentWorkcycle),
	generic_send_to_participants(ExptConsL, IUJMsg),
	ok.

% TODO edit timeout.
startup_send_w2wc(_C, _A, []) -> ok;
startup_send_w2wc(CurrentWorkcycle, APartL, JConsL) ->
	W2WMsg = management_message:welcome2workcycle(?ACCEPTED, 
										CurrentWorkcycle, 2000, APartL),
	generic_send_to_participants(JConsL, W2WMsg),
	ok.

startup_send_tick([], _CurrentWorkcycle, _Timeout) -> ok;
startup_send_tick(ExpdConsL, CurrentWorkcycle, Timeout) ->
	TickMsg = management_message:tick(CurrentWorkcycle),	
	generic_send_to_connections(ExpdConsL, 
		{ 
		wait_for_realtime_msg, {wcn, CurrentWorkcycle}, 
		{rn, 0}, {timeout, Timeout}
	}),
	generic_send_to_participants(ExpdConsL, TickMsg),
	ok.

startup_set_participants_active([], _) ->
	ok;
startup_set_participants_active(PartList, ForWhichWorkcycle) ->
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
			io:format("[startup_set_participants_active]: There was something wrong with setting the participant active ~w~n",[Error]),
			false
	end.

reservation_evaluate_reservation(-1, _IndividualMessageLengths ,
			<<_:112, 1:32, IndividualMessageLength:32, _Random:32>>) ->
	io:format("-1, _, 1, ~w, _~n",[IndividualMessageLength]),
	{finished, {iml, [IndividualMessageLength]}};
reservation_evaluate_reservation(-1, _Confirmed ,
					<<_:112, 0:32, _IndividualLength:32, _Random:32>>) ->
	io:format("-1, _, 0, _, _~n"),
	{finished, {iml,[]}};
reservation_evaluate_reservation(ExpectedRounds, IndividualMessageLengths, 
								<<_:112, 1:32, IndividualMessageLength:32, _Random:32>>) 
	when length(IndividualMessageLengths) == (ExpectedRounds - 1) ->
	io:format("~w, ~w, 1, ~w, _~n",[ExpectedRounds, IndividualMessageLengths,IndividualMessageLength]),
	{finished, {iml,lists:reverse([IndividualMessageLength|IndividualMessageLengths])}};
reservation_evaluate_reservation(_ExpectedRounds, IndividualMessageLengths, 
								<<_:112, 1:32, IndividualMessageLength:32, _Random:32>>) ->
	io:format("_, ~w, 1, ~w, _~n",[IndividualMessageLengths,IndividualMessageLength]),
	{not_finished, {iml,[IndividualMessageLength|IndividualMessageLengths]}};
reservation_evaluate_reservation(-1, _IndividualMessageLengths ,
						<<_:112, ParticipantCount:32, _IndividualLength:32, _Random:32>>) ->
	io:format("-1, _, ~w, _, _~n",[ParticipantCount]),
	{not_finished, {ec,ParticipantCount}};
reservation_evaluate_reservation(ExpectedRounds, IndividualMessageLengths, AddMsg) ->
	io:format("Expr ~w, Indiv ~w, Addm ~w~n", [ExpectedRounds, IndividualMessageLengths, AddMsg]),
	{not_finished}.