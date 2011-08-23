-module(workcycle_total_evt_hdlr).
-include_lib("stdlib/include/qlc.hrl").
-behaviour(gen_event).

-include("util.hrl").

-record(wcn_total_stats, {
			wcn           ,
			wc_start      ,
			wc_stop       ,
			res_start     ,
			res_stop      ,
			send_start    ,
			send_stop     ,
			count_active  ,
			count_joining ,
			count_kicked  = 0,
			count_leaving ,
			count_rounds}).


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
	util:safe_mnesia_create_table(wcn_total_stats,
		[{attributes, record_info(fields,wcn_total_stats)}]),
	error_logger:info_msg("Total statistic event handler is coming up~n"),
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
					Filename = ["log/total_log-"|integer_to_list(util:mk_timestamp_us())],
					{ok, IODevice} = file:open(Filename, [append]),
					io:format(IODevice, "wcn,wc_start, wc_stop, res_start, res_stop, send_start, send_stop, count_active, count_joining, count_kicked, count_leaving, count_rounds~n", []),
					lists:foreach( fun(X) -> 
										N1 = X#wcn_total_stats.wcn            , N2 = X#wcn_total_stats.wc_start     ,
										N3 = X#wcn_total_stats.wc_stop        , N4 = X#wcn_total_stats.res_start    ,
										N5 = X#wcn_total_stats.res_stop       , N6 = X#wcn_total_stats.send_start   ,
										N7 = X#wcn_total_stats.send_stop      , N8 = X#wcn_total_stats.count_active ,
										N9 = X#wcn_total_stats.count_joining  , N10 = X#wcn_total_stats.count_kicked,
										N11= X#wcn_total_stats.count_leaving  , N12 = X#wcn_total_stats.count_rounds,
										io:format(IODevice, "~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w~n", 
											[N1, N2, N3, N4, N5, N6, N7, N8, N9, N10, N11, N12]) end, AList),
					file:close(IODevice);
			Error -> 
				error_logger:error_msg("Error reading total statistics database: ~w ~n", [Error])
	end,
	{ok, State};

handle_event(Event, State) ->
  error_logger:warning_msg("[total statistics]: Unknown event~w~n",[Event]),
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

?CREATE_OR_UPDATE(wcn_total_stats, wcn, wc_start).
?CREATE_OR_UPDATE(wcn_total_stats, wcn, wc_stop).
?CREATE_OR_UPDATE(wcn_total_stats, wcn, res_start).
?CREATE_OR_UPDATE(wcn_total_stats, wcn, res_stop).
?CREATE_OR_UPDATE(wcn_total_stats, wcn, send_start).
?CREATE_OR_UPDATE(wcn_total_stats, wcn, send_stop).
?CREATE_OR_UPDATE(wcn_total_stats, wcn, count_active).
?CREATE_OR_UPDATE(wcn_total_stats, wcn, count_joining).
?CREATE_OR_UPDATE(wcn_total_stats, wcn, count_kicked).
?CREATE_OR_UPDATE(wcn_total_stats, wcn, count_leaving).
?CREATE_OR_UPDATE(wcn_total_stats, wcn, count_rounds).
