-module(management_message).
-compile([debug_info]).
-include("dc_server.hrl").

%% For inbound messages
-export([parse_message/2]).

%% For outbound messages
-export([
		accepted4service/1,
		added/3,
		info_passive_partlist/1,
		info_update_joining_participants/2,
		tick/1,
		welcome2service/5,
		welcome2workcycle/4]).

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

-define(IRQ_PASSIVEPARTICIPANTLIST , 0).
-define(IRQ_ACTIVEPARTICIPANTLIST  , 1).

-define(INFO_PASSIVEPARTICIPANTLIST , 0).
-define(INFO_ACTIVEPARTICIPANTLIST  , 1).
-define(INFO_UPDATEACTIVEJOINING    , 2).
-define(INFO_UPDATEACTIVELEAVING    , 3).
-define(INFO_COMMITKEYEXCHANGE      , 4).
-define(INFO_EARLYQUITNOTIFICATION  , 5).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Message parsing (only messages from participant ->  server)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Register at service
parse_message(TypeBin, << PidLen:8, Pid:PidLen/binary, UidLen:8, Uid:UidLen/binary, 
		 	SigLen:16, Sig:SigLen/binary, DHLen:16, DH:DHLen/binary, DHSigLen:16, 
			DHSig:DHSigLen/binary >>) when TypeBin == ?REGISTERATSERVICE ->
				{register_at_service, #participant{   
								participantid    = Pid,
								userid           = Uid,
								sig              = Sig,
								diffiehellman    = DH,
								diffiehellmansig = DHSig}};
parse_message(TypeBin, _MsgBin) when TypeBin == ?REGISTERATSERVICE ->
	{error, malformed_register_at_service};

%% Inforequest
parse_message(TypeBin, <<InfoServiceRequest:16>>) when TypeBin == ?INFOREQ ->
	case InfoServiceRequest of
		?IRQ_PASSIVEPARTICIPANTLIST ->
			{irq, passivelist};
		?IRQ_ACTIVEPARTICIPANTLIST ->
			{irq, activelist}
	end;
parse_message(TypeBin, _MsgBin) when TypeBin == ?INFOREQ ->
	{error, malformed_inforequest};

%% Info
parse_message(TypeBin, _MsgBin) when TypeBin == ?INFO ->
	{error, unimplemented_info};

%% Join Workcycle
parse_message(TypeBin, _MsgBin) when TypeBin == ?JOINWORKCYCLE ->
	{joinworkcycle};

%% ADD
parse_message(TypeBin, <<WCN:64, RN:16, AddMsg/binary>>) when TypeBin == ?ADD ->
	{add, WCN, RN, AddMsg};
parse_message(TypeBin, _MsgBin) when TypeBin == ?ADD ->
	{error, malformed_add};

%% ADD RESERVATION
parse_message(TypeBin, _MsgBin) when TypeBin == ?ADDRESERVATION ->
	{error, unimplemented_addreservation};

%% Leave workcycle
parse_message(TypeBin, _MsgBin) when TypeBin == ?LEAVEWORKCYCLE ->
	{error, unimplemented_leaveworkcycle};

%% Quit service
parse_message(TypeBin, _MsgBin) when TypeBin == ?QUITSERVICE ->
	{error, unimplemented_quitservice};

% Anything else

parse_message(TypeBin, _MsgBin) when is_binary(TypeBin)->
	{error, unkown_messagetype};
parse_message(_, _) ->
	{error, unkown_error}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Messages from perver -> participant
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accepted4service(true) ->
	Accepted = <<0:8>>,	
	AccceptedSize = byte_size(Accepted),
	list_to_binary([?ACCEPTED4SERVICE, <<AccceptedSize:16>>, Accepted]);
accepted4service(_) ->
	Rejected = <<1:8>>,
	RejectedSize = byte_size(Rejected),
	list_to_binary([?ACCEPTED4SERVICE, <<RejectedSize:16>>, Rejected]).
	
added(WCN, RN, MsgBin) ->
	Payload  = list_to_binary([<<WCN:64, RN:16>>, MsgBin]),
	PayloadSize = size(Payload),
	list_to_binary([?ADDED, <<PayloadSize:16>>,Payload]).

info_passive_partlist(PartList) when is_list(PartList) ->
	InfoHead = << ?INFO_PASSIVEPARTICIPANTLIST:16>>,
	InfoList = participantlist(PartList),
	TotalSize = size(list_to_binary([InfoHead,InfoList])),
	list_to_binary([?INFO, <<TotalSize:16 >>, InfoHead, InfoList]).

info_update_joining_participants(PartList, JoiningWhen) when is_list(PartList) ->
	InfoHead = << ?INFO_UPDATEACTIVEJOINING:16>>,
	InfoList = participant_update_list(PartList, JoiningWhen),
	TotalSize = size(list_to_binary([InfoHead,InfoList])),
	list_to_binary([?INFO, <<TotalSize:16 >>, InfoHead, InfoList]).

tick(WorkCycleNumber) ->
	TotalSize = size( << WorkCycleNumber:64 >> ),
	list_to_binary([?TICK, <<TotalSize:16>>, <<WorkCycleNumber:64>>]).

welcome2service(Version, SymbolLength, ParticipantCount, AcceptReject, FeatureList) ->
    FeatureBin = << <<X:16, Y:16>> || {X, Y} <- FeatureList >>,
	MsgTail = list_to_binary([<< Version:16, SymbolLength:16, ParticipantCount:16, AcceptReject:16 >>, FeatureBin]),
	TailSize = byte_size(MsgTail),
	list_to_binary([?WELCOME2SERVICE, <<TailSize:16>>,  MsgTail]).

welcome2workcycle(AcceptReject, ForWorkCycleNumber, Timeout, ActivePartList) ->
	APartList = participantlist(ActivePartList),
	Msg = list_to_binary([<< AcceptReject:8, ForWorkCycleNumber:64, Timeout:16 >>, APartList]),
	TotalSize = size(Msg),
	list_to_binary([?WELCOME2WORKCYCLE, <<TotalSize:16>>, Msg]).

%% Returns first two bytes of how many participants are in the list,
%% followed by the five tupled participants.
participantlist(PartList) ->
	LengthList = length(PartList),
	ListHead = << LengthList:16 >>,
	ListMeat = lists:map(fun participant_to_binary/1, PartList),
	list_to_binary([ListHead, ListMeat]).

participant_update_list(PartList, JoiningWhen) ->
	LengthList = length(PartList),
	ListHead = << LengthList:16 >>,
	ListMeat = lists:map(fun(X) -> 
							[PartBin] = participant_to_binary(X), 
							[list_to_binary([<< JoiningWhen:64 >>, PartBin])]
						   end, PartList),
	list_to_binary([ListHead, ListMeat]).

participant_to_binary(Participant) when is_record(Participant, participant) ->
	#participant{participantid=Pid, 
		userid=Uid, 
		sig=Sig, 
		diffiehellman=DH, 
		diffiehellmansig=DHSig} = Participant,
	PidSize = size(Pid), UidSize = size(Uid),
	SigSize = size(Sig), DHSize = size(DH),
	DHSigSize = size(DHSig),
	[<< PidSize:16, Pid:PidSize/binary, 
		UidSize:16, Uid:UidSize/binary,
		SigSize:16, Sig:SigSize/binary,
		DHSize:16, DH:DHSize/binary,
		DHSigSize:16, DHSig:DHSigSize/binary>>];
participant_to_binary(_) ->
	io:format("wrong argument"),
	[<<>>].

