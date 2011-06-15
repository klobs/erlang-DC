-module(rt_message_handler).
-export([rt_message_handler/1]).

rt_message_handler({wait, {part, P}, {con, C}, {wcn, W}, {bufferlist, B}}) ->
	receive 
		{wait_for_realtime_msg, {wcn, W}, {rn, R}, {timeout, T}} ->
			rt_message_handler({receive_rt, {part, P}, {con, C}, {wcn, W}, {rn, R}, {bufferlist, B}, {timeout, T}});
		{wait_for_realtime_msg, {wcn, NW}, {rn, R}, {timeout, T}} ->
			rt_message_handler({receive_rt, {part, P}, {con, C}, {wcn, NW}, {rn, R}, {bufferlist, []}, {timeout, T}});
		{add, {part, P}, {wcn, W}, {rn, R}, {addmsg, A}} ->
			io:format("rt_message_handler: not yet in receive_rt mode, buffering message: ~w~n",[A]),
				rt_message_handler({wait, {part, P}, {con, C}, {wcn, W}, 
						{bufferlist, B ++ [{add, {part, P}, {con, C}, {wcn, W}, {rn, R}, {addmsg, A}}]}});
		Error ->
			io:format("This message is not for me [waiting]: ~w ~n",[Error]),
			rt_message_handler({wait, {part, P}, {con, C}, {wcn, W}, {bufferlist, B}})
	end;

rt_message_handler({receive_rt, {part, P}, {con, C}, {wcn, W}, {rn, R}, {bufferlist, []}, {timeout, T}}) ->
	receive 
		{add, {part, P}, {wcn, W}, {rn, R}, {addmsg, A}} ->
			%io:format("Add message received for wcn ~w round ~w~n",[W,R]),
			gen_fsm:send_event(workcycle, {add, {part, P}, {con, C}, {wcn, W}, {rn, R}, {addmsg, A}}),
			rt_message_handler({wait, {part, P}, {con, C}, {wcn, W}, {bufferlist, []}});
		Error ->
			io:format("This message is not for me [rt]: ~w ~n (waiting for wcn ~w and rn ~w~n)",[Error, W, R]),
			rt_message_handler({receive_rt, {part, P}, {con, C}, {wcn, W}, {rn, R}, {bufferlist, []}, {timeout, T}})
		after T ->
			gen_fsm:send_event(workcycle, {addtimeout, {part, P}, {con, C}, {wcn, W}, {rn, R}}),
			rt_message_handler({wait, {part, P}, {con, C}, {wcn, W}, {bufferlist, []}})
	end;

rt_message_handler({receive_rt, {part, P}, {con, C},{wcn, W}, {rn, _RN}, {bufferlist, [H|T]}, {timeout, _To}}) ->
	io:format("[rt_handler] sending buffered message ~n"),
	gen_fsm:send_event(workcycle, H),
	rt_message_handler({wait, {part, P}, {con, C}, {wcn, W}, {bufferlist, T}});

rt_message_handler(E) ->
	io:format("sorry, what? ~w~n", [E]).
