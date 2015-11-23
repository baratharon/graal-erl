-module(builtin22).
-export([main/0]).

main() ->
	Map = #{ a => 100, b => 1.4, 10 => aa, 20 => [], 11 => 1.1, 1.2 => {}},
	{
		try map_size(not_a_map) catch error:{badmap, V} -> {ok, badmap, V} end,
		map_size(Map),
		map_size(Map #{ a := overwrite_key }),
		map_size(Map #{ new_key => new_value })
	}.
