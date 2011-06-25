-module(workcycle_total_evt_hdlr).
-include_lib("stdlib/include/qlc.hrl").
-behaviour(gen_event).

-record(wcn_total_stats, {
			wcn           = -1,
			wc_start      = -1,
			wc_stop       = -1,
			res_start     = -1,
			res_stop      = -1,
			send_start    = -1,
			send_stop     = -1,
			count_active  = -1,
			count_joining = -1,
			count_kicked  = 0,
			count_leaving = -1,
			count_rounds  = -1}).


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
	error_logger:info_msg("Total statistic event handler is coming up~n"),
	utils:safe_mnesia_create_table(wcn_total_stats,
		[{attributes, record_info(fields,wcn_total_stats)}]),
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

handle_event({wc_start, WCN, Timestamp}, State) ->
	wc_start(WCN, Timestamp),
	{ok, State};

handle_event({wc_stop, WCN, Timestamp}, State) ->
	wc_stop(WCN, Timestamp),
	{ok, State};

handle_event({res_start, WCN, Timestamp}, State) ->
	res_start(WCN, Timestamp),
	{ok, State};

handle_event({res_stop, WCN, Timestamp}, State) ->
	res_stop(WCN, Timestamp),
	{ok, State};

handle_event({send_start, WCN, Timestamp}, State) ->
	send_start(WCN, Timestamp),
	{ok, State};

handle_event({send_stop, WCN, Timestamp}, State) ->
	send_stop(WCN, Timestamp),
	{ok, State};

handle_event({count_active, WCN, Count}, State) ->
	count_active(WCN, Count),
	{ok, State};

handle_event({count_joining, WCN, Count}, State) ->
	count_joining(WCN, Count),
	{ok, State};

handle_event({count_kicked, WCN, Count}, State) ->
	count_kicked(WCN, Count),
	{ok, State};

handle_event({count_leaving, WCN, Count}, State) ->
	count_leaving(WCN, Count),
	{ok, State};

handle_event({count_rounds, WCN, Count}, State) ->
	count_rounds(WCN, Count),
	{ok, State};

handle_event(dump_to_csv, State) ->
	io:format("[totat_stats]: dumping to csv~n"),
	T = fun() ->
			qlc:e( qlc:q([ X || X <- mnesia:table(wcn_total_stats)]))
		end,
	case mnesia:transaction(T) of
			{atomic, AList} ->
					Filename = integer_to_list(util:mk_timestamp_us()),
					{ok, IODevice} = file:open(Filename, [append]),
					io:format(IODevice, "~w~n", [AList]),
					%lists:foreach
					file:close(IODevice);
			Error -> 
				error_logger:error_msg("Error reading total statistics database: ~w ~n", [Error])
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

wc_start(WCN, Timestamp) ->
	T = fun() ->
			case mnesia:read() of
				[] ->
					mnesia:write(#wcn_total_stats{wcn = WCN, wc_start = Timestamp});
				[R] ->
					mnesia:write(R#wcn_total_stats{wcn = WCN, wc_start = Timestamp})
			end
	end,
	{atomic, ok} = mnesia:transaction(T),
	ok.

wc_stop(WCN, Timestamp) ->
	T = fun() ->
			case mnesia:read() of
				[] ->
					mnesia:write(#wcn_total_stats{wcn = WCN, wc_stop = Timestamp});
				[R] ->
					mnesia:write(R#wcn_total_stats{wcn = WCN, wc_stop = Timestamp})
			end
	end,
	{atomic, ok} = mnesia:transaction(T),
	ok.

res_start(WCN, Timestamp) ->
	T = fun() ->
			case mnesia:read() of
				[] ->
					mnesia:write(#wcn_total_stats{wcn = WCN, res_start = Timestamp});
				[R] ->
					mnesia:write(R#wcn_total_stats{wcn = WCN, res_start = Timestamp})
			end
	end,
	{atomic, ok} = mnesia:transaction(T),
	ok.

res_stop(WCN, Timestamp) ->
	T = fun() ->
			case mnesia:read() of
				[] ->
					mnesia:write(#wcn_total_stats{wcn = WCN, res_stop = Timestamp});
				[R] ->
					mnesia:write(R#wcn_total_stats{wcn = WCN, res_stop = Timestamp})
			end
	end,
	{atomic, ok} = mnesia:transaction(T),
	ok.

send_start(WCN, Timestamp) ->
	T = fun() ->
			case mnesia:read() of
				[] ->
					mnesia:write(#wcn_total_stats{wcn = WCN, send_start = Timestamp});
				[R] ->
					mnesia:write(R#wcn_total_stats{wcn = WCN, send_start = Timestamp})
			end
	end,
	{atomic, ok} = mnesia:transaction(T),
	ok.

send_stop(WCN, Timestamp) ->
	T = fun() ->
			case mnesia:read() of
				[] ->
					mnesia:write(#wcn_total_stats{wcn = WCN, send_stop = Timestamp});
				[R] ->
					mnesia:write(R#wcn_total_stats{wcn = WCN, send_stop = Timestamp})
			end
	end,
	{atomic, ok} = mnesia:transaction(T),
	ok.

count_active(WCN, Count) ->
	T = fun() ->
			case mnesia:read() of
				[] ->
					mnesia:write(#wcn_total_stats{wcn = WCN, count_active = Count});
				[R] ->
					mnesia:write(R#wcn_total_stats{wcn = WCN, count_active = Count})
			end
	end,
	{atomic, ok} = mnesia:transaction(T),
	ok.

count_joining(WCN, Count) ->
	T = fun() ->
			case mnesia:read() of
				[] ->
					mnesia:write(#wcn_total_stats{wcn = WCN, count_joining = Count});
				[R] ->
					mnesia:write(R#wcn_total_stats{wcn = WCN, count_joining = Count})
			end
	end,
	{atomic, ok} = mnesia:transaction(T),
	ok.

count_kicked(WCN, Count) ->
	T = fun() ->
			case mnesia:read() of
				[] ->
					mnesia:write(#wcn_total_stats{wcn = WCN, count_kicked = Count});
				[R] ->
					mnesia:write(R#wcn_total_stats{wcn = WCN, count_kicked = Count})
			end
	end,
	{atomic, ok} = mnesia:transaction(T),
	ok.

count_leaving(WCN, Count) ->
	T = fun() ->
			case mnesia:read() of
				[] ->
					mnesia:write(#wcn_total_stats{wcn = WCN, count_leaving = Count});
				[R] ->
					mnesia:write(R#wcn_total_stats{wcn = WCN, count_leaving = Count})
			end
	end,
	{atomic, ok} = mnesia:transaction(T),
	ok.

count_rounds(WCN, Count) ->
	T = fun() ->
			case mnesia:read() of
				[] ->
					mnesia:write(#wcn_total_stats{wcn = WCN, count_rounds = Count});
				[R] ->
					mnesia:write(R#wcn_total_stats{wcn = WCN, count_rounds = Count})
			end
	end,
	{atomic, ok} = mnesia:transaction(T),
	ok.
