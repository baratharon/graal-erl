-module(builtin26).
-export([main/0]).

main() ->
	{
		try maps:from_list(not_a_list) catch error:badarg -> badarg end,
		try maps:from_list([a]) catch error:badarg -> badarg end,
		try maps:from_list([{a,b,c}]) catch error:badarg -> badarg end,
		try maps:from_list([{a,b}|{c,d}]) catch error:badarg -> badarg end,
		maps:from_list([{a,b}]),
		maps:from_list([{a,b}, {a,bb}]),
		maps:from_list([{a,b},{c,d}]),
		maps:from_list([])
	}.
