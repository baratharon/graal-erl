-module(tuple07).
-export([main/0]).

main() ->
	{
		try erlang:list_to_tuple(x) catch error:badarg -> {ok, badarg} end,
		erlang:list_to_tuple([a,b,c]),
		erlang:list_to_tuple([a,a,b,b,c,c]),
		erlang:list_to_tuple([1,a,2,b,3,c]),
		erlang:list_to_tuple([])
	}.
