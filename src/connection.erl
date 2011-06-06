-module(connection).
-include("dc_server.hrl").
-export([welcome/1]).

welcome(Sock) ->
	W2M = management_message:welcome2service(?PROTOCOL_VERSION, ?SYMBOL_LENGTH,0,0, ?FEATURE_LIST),
	case gen_tcp:send(Sock, W2M) of
		ok -> 
			case gen_tcp:recv(Sock, 2, 2000) of
				{ok, MsgTypeBin} ->
					case gen_tcp:recv(Sock, 2, 100) of
						{ok, LengthBin} ->
							<<Length:16/integer-signed>> = LengthBin,
								case gen_tcp:recv(Sock, Length, 100) of
									{ok, MsgBin} -> 
										{ok, Part} =management_message:parseMessage(MsgTypeBin, MsgBin),
										participant_manager:register_participant(Part, Sock),
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


