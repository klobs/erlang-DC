-module(connection).
-include("dc_server.hrl").
-export([listen/1]).

listen(Sock) ->
	gen_tcp:send(Sock, management_message:welcome2service(?PROTOCOL_VERSION, ?SYMBOL_LENGTH,0,0, ?FEATURE_LIST)),
	gen_tcp:close(Sock).
