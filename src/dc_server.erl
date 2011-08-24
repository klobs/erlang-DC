-module(dc_server).

-behaviour(gen_server).

-include("dc_server.hrl").

-export([start_link/0]).

%callbacks for gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% basic definitions
-define(TCPOPTIONS, [binary, 
					{packet, 0}, 
					{active, false}, 
					{reuseaddr, true}, 
					{packet_size, 65540}, 
					{send_timeout_close, true}]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, []}.

%% callbacks
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(listen, _State) ->
	listen();	
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions

listen() ->
	case gen_tcp:listen(?DEFAULTPORT, ?TCPOPTIONS) of
			{ok, LSock} ->
				accept(LSock);
			{error, Reason} ->
				io:format("ERROR: ~s!~n",[Reason])
	end.

accept(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
	Controller = spawn(connection, welcome, [Sock]),
	gen_tcp:controlling_process(Sock, Controller),
	accept(LSock).
		
