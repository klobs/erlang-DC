-module(config).
-include("dc_server.hrl").
-export([
		get_feature_list/0,
		get_keg_method/0,
		get_kex_method/0,
		get_port/0,
		get_min_active_participants/0,
		get_rt_timeout/0,
		get_symbol_length/0,
		get_tick_timeout/0,
		get_variable_payload_length/0
	]).

get_feature_list() ->
	[
		{?FEATURE_KEG, get_keg_method()}, 
		{?FEATURE_LCH, ?LCH_NONE}, 
		{?FEATURE_KEX, get_kex_method()},
		{?FEATURE_VPL, get_variable_payload_length()},
		{?FEATURE_EQR, ?EQR_RESTART_WC}
	].

get_keg_method() ->
	case application:get_env(erlangDC, keg_method) of
		{ok, keg_null} -> ?KEG_NULL;
		{ok, keg_dc}   -> ?KEG_DC;
		{ok, keg_fail_stop_wc} -> ?KEG_FAIL_STOP_WC;
		{ok, keg_probab_fail_stop} -> ?KEG_POBAB_FAIL_STOP_WC;
		_ -> ?KEG_POBAB_FAIL_STOP_WC
	end.

get_kex_method() ->
	case application:get_env(erlangDC, kex_method) of
		{ok, kex_manual} -> ?KEX_MANUALLY;
		{ok, kex_fully_automatic} -> ?KEX_FULLY_AUTOMATIC;
		_ -> ?KEX_FULLY_AUTOMATIC
	end.

get_port() ->
	case application:get_env(erlangDC, port) of
		{ok, Port} -> Port;
		_ -> ?DEFAULTPORT
	end.

get_min_active_participants() ->
	case application:get_env(erlangDC, min_active_participants) of
		{ok, MinPart} -> MinPart;
		_ -> ?MIN_ACTIVE_PARTICIPANTS
	end.


get_rt_timeout() ->
	case application:get_env(erlangDC, rt_timeout) of
		{ok, RTTimeout} -> RTTimeout;
		_ -> ?DEFAULTRTTIMEOUT
	end.

get_symbol_length() ->
	case application:get_env(erlangDC, symbol_length) of
		{ok, Length} -> Length;
		_ -> ?SYMBOL_LENGTH
	end.

get_tick_timeout() ->
	case application:get_env(erlangDC, tick_timeout) of
		{ok, TickTimeout} -> TickTimeout;
		_ -> ?DEFAULTTICKTIMEOUT
	end.

get_variable_payload_length() ->
	case application:get_env(erlangDC, variable_payloads) of
		{ok, yes} -> ?VPL_VARIABLE;
		{ok, no} -> ?VPL_FIXED;
		_ -> ?VPL_VARIABLE
	end.
