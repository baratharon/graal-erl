-module(builtin18).
-export([main/0]).

main() ->
	Map = #{ a => 10, b => 20, 10 => a, 20 => b},
	{
		try maps:is_key(a, not_a_map) catch error:{badmap, V} -> {ok, badmap, V} end,
		maps:is_key(no_such, Map),
		maps:is_key(a, Map),
		maps:is_key(20, Map)
	}.
