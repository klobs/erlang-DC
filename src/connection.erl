-module(connection).
-include("dc_server.hrl").
-export([welcome/1]).

welcome(Sock) ->
	NumPart = participant_manager:passive_participant_count(),
	W2M = management_message:welcome2service(?PROTOCOL_VERSION, ?SYMBOL_LENGTH,NumPart, 0, ?FEATURE_LIST),
	case gen_tcp:send(Sock, W2M) of
		ok -> 
			case gen_tcp:recv(Sock, 2, 2000) of
				{ok, MsgTypeBin} ->
					case gen_tcp:recv(Sock, 2, 100) of
						{ok, LengthBin} ->
							<<Length:16/integer-signed>> = LengthBin,
								case gen_tcp:recv(Sock, Length, 100) of
									{ok, MsgBin} -> 
										{register_at_service, Part} = management_message:parse_message(MsgTypeBin, MsgBin),
										participant_manager:register_participant(Part, self()),
										listen(Sock, Part, noneYet),
										ok;
									{error,Reason} -> {error, Reason}       
								end;
						{error, Reason} -> {error, Reason}
					end;
					{error, Reason} -> {error, Reason}
				end,
				ok;
		{error, Reason} -> 
			gen_tcp:close(Sock),
			{error, Reason}
	end.

listen(Sock, Part, AddMessageHandler) ->
	inet:setopts(Sock,[{active,once}, {keepalive, true}]),
	receive
		%% messages that can be received by the socket
		{tcp, Sock, <<MsgType:16, MsgLen:16, MsgBin:MsgLen/binary>>} ->
			case management_message:parse_message(<<MsgType:16>>, MsgBin) of
				{irq, Irq} -> 
					handle_irq(Irq);
				{joinworkcycle} -> 
					workcycle:join_workcycle(Part, self());
				{add, WCN, RN, AddMsg} ->
					case is_pid(AddMessageHandler) of
						true ->
							AddMessageHandler ! {add, {Part, self()}, WCN, RN, AddMsg};
						_ -> 
							io:format("There is no add message event handler to send to")
					end;
				{error, Reason} -> 
					io:format("Parseerror happended during message parsing: ~w!~n",[Reason])
			end,
			listen(Sock, Part, AddMessageHandler);
		{tcp_closed,Sock} ->
			io:format("Socket closed, unregistering participant~n"),
			participant_manager:unregister_participant(Part),
			ok;
		{tcp, Sock, Data} ->
			io:format("Arbritrary message on Socket ~w: ~w ~n", [Sock, Data]),
			listen(Sock,Part, AddMessageHandler);

		%% Messages from other processes to forward to the socket
		{forward, Msg} when is_binary(Msg)->
			gen_tcp:send(Sock, Msg),
			listen(Sock, Part, AddMessageHandler);

		{add_message_handler, AMHPid} ->
			listen(Sock, Part, AMHPid);

		Error ->
			io:format("Arbritrary message on Socket ~w: ~w~n", [Sock, Error]),
			listen(Sock,Part, AddMessageHandler)
	end.


handle_irq(passivelist) ->
	participant_manager:send_passive_partlist(self());
handle_irq(activelist) ->
	participant_manager:send_active_partlist(self());
handle_irq(_) ->
	ok.

