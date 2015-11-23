-module(builtin27).
-export([main/0]).

main() ->
	{
		try maps:to_list(not_a_map) catch error:{badmap, V} -> {ok, badmap, V} end,
		maps:to_list(#{a => b}),
		maps:to_list(#{a => b, c => d}),
		maps:to_list(#{a => 1, b => 2, c => 3, b => 4, a => 5}),
		maps:to_list(#{})
	}.
