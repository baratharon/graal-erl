-module(builtin20).
-export([main/0]).

main() ->
	Map = #{ a => 100, b => 1.4, 10 => aa, 20 => [], 11 => 1.1, 1.2 => {}},
	{
		try maps:values(not_a_map) catch error:{badmap, V} -> {ok, badmap, V} end,
		maps:values(Map)
	}.
