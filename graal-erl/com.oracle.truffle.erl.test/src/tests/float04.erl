-module(float04).
-export([main/0]).

main() ->
	{
		try float_to_list(1.25, {}) catch error:badarg -> {ok} end,
		try float_to_list(1.25, [atom]) catch error:badarg -> {ok} end,
		float_to_list(1.25),
		float_to_list(1.25, [])
	}.
