-module(management_message).
-include("dc_server.hrl").

%% For inbound messages
-export([parse_message/2]).

%% For outbound messages
-export([
		accepted4service/1,
		info_passive_partlist/1,
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
	{error, unimplemented_joinworkcycle};	

%% ADD
parse_message(TypeBin, _MsgBin) when TypeBin == ?ADD ->
	{error, unimplemented_add};

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

%% Welcome 2 Service
welcome2service(Version, SymbolLength, ParticipantCount, AcceptReject, FeatureList) ->
    FeatureBin = << <<X:16, Y:16>> || {X, Y} <- FeatureList >>,
	MsgTail = list_to_binary([<< Version:16, SymbolLength:16, ParticipantCount:16, AcceptReject:16 >>, FeatureBin]),
	TailSize = byte_size(MsgTail),
	list_to_binary([?WELCOME2SERVICE, << TailSize:16>>,  MsgTail]).

%% Accepted 4 Service
accepted4service(true) ->
	Accepted = <<0:8>>,	
	AccceptedSize = byte_size(Accepted),
	list_to_binary([?ACCEPTED4SERVICE, <<AccceptedSize:16>>, Accepted]);
accepted4service(_) ->
	Rejected = <<1:8>>,
	RejectedSize = byte_size(Rejected),
	list_to_binary([?ACCEPTED4SERVICE, <<RejectedSize:16>>, Rejected]).
	
info_passive_partlist(PartList) when is_list(PartList) ->
	LengthList = length(PartList),
	InfoHead = << ?INFO_PASSIVEPARTICIPANTLIST:16, LengthList:16>>,
	InfoList = lists:flatmap(fun(X) -> 
								#participant{   participantid=Pid, 
												userid=Uid, 
												sig=Sig, 
												diffiehellman=DH, 
												diffiehellmansig=DHSig} = X,
								PidSize = size(Pid), UidSize = size(Uid),
								SigSize = size(Sig), DHSize = size(DH),
								DHSigSize = size(DHSig),
								[<< PidSize:16, Pid:PidSize/binary, 
									UidSize:16, Uid:UidSize/binary,
									SigSize:16, Sig:SigSize/binary,
									DHSize:16, DH:DHSize/binary,
									DHSigSize:16, DHSig:DHSigSize/binary>>]
								end, PartList),
	io:format("There are ~w participants in my generated list~n", [LengthList]),
	TotalSize = size(list_to_binary([InfoHead,InfoList])),
	list_to_binary([?INFO, <<TotalSize:16 >>, InfoHead, InfoList]);
info_passive_partlist(_) ->
	<< >>.

