-module(builtin19).
-export([main/0]).

main() ->
	Map = #{ a => 100, b => 200, 10 => aa, 20 => bb},
	{
		try maps:keys(not_a_map) catch error:{badmap, V} -> {ok, badmap, V} end,
		maps:keys(Map)
	}.
