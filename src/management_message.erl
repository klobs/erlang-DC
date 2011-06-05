-module(management_message).
-include("dc_server.hrl").
-export([parseMessage/2,
		welcome2service/5]).

-define(WELCOME2SERVICE   , <<0:16>>).  % S->P
-define(REGISTERATSERVICE , <<1:16>>).  % P->S
-define(ACCEPTED4SERVICE  , <<2:16>>).  % S->P
-define(INFOREQ           , <<3:16>>).  % S->P, P->S
-define(INFO              , <<4:16>>).  % S->P, P->S
-define(JOINWORKCYCLE     , <<5:16>>).  % P->S
-define(WELCOME2WORKCYCLE , <<6:16>>).  % S->P
-define(ADD               , <<7:16>>).  % P->S
-define(ADDED             , <<8:16>>).  % S->P
-define(ADDRESERVATION    , <<9:16>>).  % P->S
-define(ADDEDRESERVATION  , <<10:16>>). % S->P
-define(TICK              , <<11:16>>). % S->P
-define(LEAVEWORKCYCLE    , <<12:16>>). % P->S
-define(QUITSERVICE       , <<13:16>>). % P->S
-define(KTHXBYE           , <<14:16>>). % S->P

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Message parsing (only messages from participant ->  server)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Register at service
parseMessage(TypeBin, << PidLen:8, Pid:PidLen/binary, UidLen:8, Uid:UidLen/binary, 
		 	SigLen:16, Sig:SigLen/binary, DHLen:16, DH:DHLen/binary, DHSigLen:16, 
			DHSig:DHSigLen/binary >>) when TypeBin =:= ?REGISTERATSERVICE ->
				{ok, #participant{   participantid    = Pid,
								userid           = Uid,
								sig              = Sig,
								diffiehellman    = DH,
								diffiehellmansig = DHSig}};

parseMessage(TypeBin, _MsgBin) when TypeBin =:= ?REGISTERATSERVICE ->
	io:format("Malformed Register at service message~n"),
	{error, malformed};

%% Inforequest
parseMessage(TypeBin, _MsgBin) when TypeBin =:= ?INFOREQ ->
	io:format("Inforequest"),
	ok;

%% Info
parseMessage(TypeBin, _MsgBin) when TypeBin =:= ?INFO ->
	io:format("Info"),
	ok;

%% Join Workcycle
parseMessage(TypeBin, _MsgBin) when TypeBin =:= ?JOINWORKCYCLE ->
	io:format("Join Workcycle"),
	ok;	

%% ADD
parseMessage(TypeBin, _MsgBin) when TypeBin =:= ?ADD ->
	io:format("ADD"),
	ok;

%% ADD RESERVATION
parseMessage(TypeBin, _MsgBin) when TypeBin =:= ?ADDRESERVATION ->
	io:format("ADDRESERVATION"),
	ok;

%% Leave workcycle
parseMessage(TypeBin, _MsgBin) when TypeBin =:= ?LEAVEWORKCYCLE ->
	io:format("LEAVEWORKCYCLE"),
	ok;

%% Quit service
parseMessage(TypeBin, _MsgBin) when TypeBin =:= ?QUITSERVICE ->
	io:format("QUITSERVICE"),
	ok;

%% Anything else
parseMessage(TypeBin, _MsgBin) when is_binary(TypeBin)->
	<<Type:16/integer-signed>> = TypeBin,
	io:format("This type of message should not be sent to this DC Server: ~B!~n",[Type]),
	ok;
parseMessage(_, _) ->
	io:format("Unkown everything!~n"),
	error.

%% Messages from perver -> participant
welcome2service(Version, SymbolLength, ParticipantCount, AcceptReject, FeatureArray) ->
    FeatureBin = << <<X:16, Y:16>> || {X, Y} <- FeatureArray >>,
	MsgTail = list_to_binary([<< Version:16, SymbolLength:16, ParticipantCount:16, AcceptReject:16 >>, FeatureBin]),
	TailSize = byte_size(MsgTail),
	list_to_binary([?WELCOME2SERVICE, << TailSize:16>>,  MsgTail]).
