-module(builtin24).
-export([main/0]).

main() ->
	Map = #{ a => 100, b => bb},
	{
		try maps:update(key, value, not_a_map) catch error:{badmap, V} -> {ok, badmap, V} end,
		try maps:update(key, value, Map) catch error:{badkey, V} -> {ok, badkey, V} end,
		maps:update(a, val, Map)
	}.
