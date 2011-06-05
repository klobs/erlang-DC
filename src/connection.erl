-module(connection).
-include("dc_server.hrl").
-export([welcome/1]).

welcome(Sock) ->
	W2M = management_message:welcome2service(?PROTOCOL_VERSION, ?SYMBOL_LENGTH,0,0, ?FEATURE_LIST),
	case gen_tcp:send(Sock, W2M) of
		ok -> 
			case gen_tcp:recv(Sock, 4) of
				{ok, <<MsgTypeBin:16, LengthBin:16>>} ->
					<<Length:16/big-integer-unsigned>> = LengthBin,
						case gen_tcp:recv(Sock, Length, 100) of
							{ok, MsgBin} -> 
								management_message:parseMessage(MsgTypeBin, MsgBin),
								ok;
							{error,Reason} -> {error, Reason}	
						end;
					{error, Reason} -> {error, Reason}
			end,
			ok;
		{error, Reason} -> 
			gen_tcp:close(Sock),
			{error, Reason}
	end.
