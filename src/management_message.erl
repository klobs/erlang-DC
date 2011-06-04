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

%% Register At Service
parseMessage(TypeBin, _MsgBin) when TypeBin =:= ?REGISTERATSERVICE ->
	io:format("Register at service"),
	ok;
parseMessage(TypeBin, _MsgBin) when TypeBin =:= ?INFOREQ ->
	io:format("Inforequest"),
	ok;
parseMessage(TypeBin, _MsgBin) when TypeBin =:= ?INFO ->
	io:format("Info"),
	ok;
parseMessage(TypeBin, _MsgBin) when TypeBin =:= ?JOINWORKCYCLE ->
	io:format("Join Workcycle"),
	ok;
parseMessage(TypeBin, _MsgBin) when TypeBin =:= ?ADD ->
	io:format("ADD"),
	ok;
parseMessage(TypeBin, _MsgBin) when TypeBin =:= ?ADDRESERVATION ->
	io:format("ADDRESERVATION"),
	ok;
parseMessage(TypeBin, _MsgBin) when TypeBin =:= ?LEAVEWORKCYCLE ->
	io:format("LEAVEWORKCYCLE"),
	ok;
parseMessage(TypeBin, _MsgBin) when TypeBin =:= ?QUITSERVICE ->
	io:format("QUITSERVICE"),
	ok;
parseMessage(TypeBin, _MsgBin) when is_binary(TypeBin)->
	<<Type:16/integer-signed>> = TypeBin,
	io:format("Unkown/unhandled message type: ~B!~n",[Type]),
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
