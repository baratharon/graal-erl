-module(tuple06).
-export([main/0]).

main() ->
	{
		try erlang:tuple_to_list(x) catch error:badarg -> {ok, badarg} end,
		erlang:tuple_to_list({a,b,c}),
		erlang:tuple_to_list({a,a,b,b,c,c}),
		erlang:tuple_to_list({1,a,2,b,3,c}),
		erlang:tuple_to_list({})
	}.
