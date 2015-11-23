-module(builtin48).
-export([main/0]).

main() ->
	{
		erlang:is_builtin(?MODULE, main, 0),
		erlang:is_builtin(erlang, tl, 1),
		erlang:is_builtin(erlang, hd, 1),
		erlang:is_builtin(erlang, is_integer, 1),
		erlang:is_builtin(erlang, is_number, 1),
		erlang:is_builtin(erlang, is_map, 1),
		erlang:is_builtin(erlang, is_function, 1),
		erlang:is_builtin(erlang, is_function, 2),
		erlang:is_builtin(lists, reverse, 2),
		erlang:is_builtin(maps, get, 2)
	}.
