-module(builtin23).
-export([main/0]).

main() ->
	Map = #{ a => 100, b => bb},
	{
		try maps:put(key, value, not_a_map) catch error:{badmap, V} -> {ok, badmap, V} end,
		maps:put(key, value, Map),
		maps:put(a, val, Map)
	}.
