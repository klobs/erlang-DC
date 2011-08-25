-module(workcycle_detailed_evt_hdlr).
-include_lib("stdlib/include/qlc.hrl").
-behaviour(gen_event).

-include("util.hrl").

-record(wcn_detailed_stats , {
			wcn            ,
			rn             ,
			type           ,
			msglength      ,
			peername         ,
			timestamp }).


%% API
-export([dump_to_csv/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------

dump_to_csv() ->
	workcycle_evt_mgr:notify(dump_to_csv).

%%====================================================================
%% gen_event callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}
%% Description: Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%--------------------------------------------------------------------
init([]) ->
	util:safe_mnesia_create_table(wcn_detailed_stats,
		[{attributes, record_info(fields, wcn_detailed_stats)}, {type, bag}]),
	error_logger:info_msg("Detailed statistic event handler is coming up~n"),
	{ok, state}.

%%--------------------------------------------------------------------
%% Function:
%% handle_event(Event, State) -> {ok, State} |
%%                               {swap_handler, Args1, State1, Mod2, Args2} |
%%                               remove_handler
%% Description:Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is called for
%% each installed event handler to handle the event.
%%--------------------------------------------------------------------

handle_event({msg_arrived_res, WCN, RN, Peername, Length, Timestamp}, State) ->
	T = fun() ->
			mnesia:write(#wcn_detailed_stats{wcn = WCN, rn = RN, type = reservation, peername = Peername, 
					msglength = Length, timestamp = Timestamp})
	end,
	{atomic, ok} = mnesia:transaction(T),
	{ok, State};

handle_event({msg_arrived, WCN, RN, Peername, Length, Timestamp}, State) ->
	T = fun() ->
			mnesia:write(#wcn_detailed_stats{wcn = WCN, rn = RN, type = normal, peername = Peername, 
					msglength = Length, timestamp = Timestamp})
	end,
	{atomic, ok} = mnesia:transaction(T),
	{ok, State};

handle_event(dump_to_csv, State) ->
	T = fun() ->
			qlc:e( qlc:q([ X || X <- mnesia:table(wcn_detailed_stats)]))
		end,
	case mnesia:transaction(T) of
			{atomic, AList} ->
					Filename = ["log/detailed-log-"|integer_to_list(util:mk_timestamp_us())],
					{ok, IODevice} = file:open(Filename, [append]),
					io:format(IODevice, " wcn, rn, msglength, peername, timestamp~n", []),
					lists:foreach( fun(X) -> 
										N1 = X#wcn_detailed_stats.wcn        , N2 = X#wcn_detailed_stats.rn ,
										N3 = X#wcn_detailed_stats.msglength  , N4 = X#wcn_detailed_stats.peername,
										N5 = X#wcn_detailed_stats.timestamp  , 
										io:format(IODevice, "~w, ~w, ~w, ~w, ~w~n", 
											[N1, N2, N3, N4, N5]) end, AList),
					file:close(IODevice);
			Error -> 
				error_logger:error_msg("Error reading detailed statistics database: ~w ~n", [Error])
	end,
	{ok, State};

handle_event(_Event, State) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_call(Request, State) -> {ok, Reply, State} |
%%                                {swap_handler, Reply, Args1, State1,
%%                                  Mod2, Args2} |
%%                                {remove_handler, Reply}
%% Description: Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified event
%% handler to handle the request.
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
  Reply = ok,
  {ok, Reply, State}.

%%--------------------------------------------------------------------
%% Function:
%% handle_info(Info, State) -> {ok, State} |
%%                             {swap_handler, Args1, State1, Mod2, Args2} |
%%                              remove_handler
%% Description: This function is called for each installed event handler when
%% an event manager receives any other message than an event or a synchronous
%% request (or a system message).
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description:Whenever an event handler is deleted from an event manager,
%% this function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Function: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
