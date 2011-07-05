-module(util).

-export([ mk_timestamp_us/0,
		safe_mnesia_create_table/2]).

mk_timestamp_us() ->
	{MS, S, SS} = erlang:now(),
	(MS * 1000000 + S) * 1000000 + SS.

safe_mnesia_create_table(Name, TabDef) ->
    case mnesia:create_table(Name, TabDef) of
    {atomic, ok} ->
        ok; 
    {aborted, {already_exists, Name}} ->
        error_logger:info_msg("Picked up existing database ~p~n", [Name]),
        %% TODO: check attributes
        ok; 
    E -> exit(E)
    end.

