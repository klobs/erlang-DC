-module(management_message).
-include("dc_server.hrl").
-export([welcome2service/5]).

-define(WELCOME2SERVICE, <<0:16>>).

%% exported functions
welcome2service(Version, SymbolLength, ParticipantCount, AcceptReject, FeatureArray) ->
    FeatureBin = << <<X:16, Y:16>> || {X, Y} <- FeatureArray >>,
	MsgTail = list_to_binary([<< Version:16, SymbolLength:16, ParticipantCount:16, AcceptReject:16 >>, FeatureBin]),
	TailSize = byte_size(MsgTail),
	list_to_binary([?WELCOME2SERVICE, << TailSize:16>>,  MsgTail]).
