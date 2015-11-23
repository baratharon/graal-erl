-module(builtin17).
-export([main/0]).

main() ->
	Map = #{ a => 10, b => 20, 10 => a, 20 => b},
	{
		try maps:find(a, not_a_map) catch error:{badmap, V} -> {ok, badmap, V} end,
		maps:find(no_such, Map),
		maps:find(a, Map),
		maps:find(20, Map)
	}.
