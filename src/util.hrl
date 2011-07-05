-define(CREATE_OR_UPDATE(Record, Keyfield, Field), Field(Key, Value) ->
			T = fun() ->
					case mnesia:read({Record, Key}) of
						[] ->
							mnesia:write(#Record{Keyfield = Key, Field = Value});
						[R] ->
							mnesia:write(R#Record{Keyfield = Key, Field = Value})
					end
			end,
			{atomic, ok} = mnesia:transaction(T),
			ok).
