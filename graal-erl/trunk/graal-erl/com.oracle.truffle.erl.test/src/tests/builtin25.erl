-module(builtin25).
-export([main/0]).

main() ->
	Map = #{ a => 100, b => bb},
	{
		try maps:remove(key, not_a_map) catch error:{badmap, V} -> {ok, badmap, V} end,
		maps:remove(key, Map),
		maps:remove(a, Map)
	}.
