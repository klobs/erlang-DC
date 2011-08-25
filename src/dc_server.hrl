%% General config options
-define(PROTOCOL_VERSION        , 2).
-define(SYMBOL_LENGTH           , 65000).
-define(MIN_ACTIVE_PARTICIPANTS , 3).
-define(DEFAULTTICKTIMEOUT      , 10).
-define(DEFAULTRTTIMEOUT        , infinity).

-define(DEFAULTPORT, 6867).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Different Features of a server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(FEATURE_KEG, 0). % Key generation
-define(FEATURE_LCH, 1). % Lost connection handling
-define(FEATURE_KEX, 2). % Key exchange
-define(FEATURE_VPL, 3). % Variable payload length
-define(FEATURE_EQR, 4). % Early quit reaction

% different key generation methods
-define(KEG_NULL               , 0).
-define(KEG_DC                 , 1).
-define(KEG_FAIL_STOP_WC       , 2).
-define(KEG_POBAB_FAIL_STOP_WC , 3).

% different lost connection handlings
-define(LCH_NONE, 0).

% different key exchange methods
-define(KEX_MANUALLY        , 0).
-define(KEX_FULLY_AUTOMATIC , 2).

% Variable and fixed sized payloads
-define(VPL_FIXED    , 0).
-define(VPL_VARIABLE , 1).

%
-define(EQR_MUST_NOT_HAPPEN , 0).
-define(EQR_RESTART_WC      , 2).

-define(FEATURE_LIST, [
						{?FEATURE_KEG, ?KEG_POBAB_FAIL_STOP_WC}, 
						{?FEATURE_LCH, ?LCH_NONE}, 
						{?FEATURE_KEX, ?KEX_FULLY_AUTOMATIC},
						{?FEATURE_VPL, ?VPL_FIXED},
						{?FEATURE_EQR, ?EQR_RESTART_WC}
					]).
%% Records
-record(participant, {  participantid    = undefined,
						userid           = undefined,
						sig              = undefined,
						diffiehellman    = undefined,
						diffiehellmansig = undefined}).

-record(participant_mgmt, { participant  = #participant{},
							controller   = undefined,
							peername     = undefined,
							active_from  = -1,
							active_until = -1}).
