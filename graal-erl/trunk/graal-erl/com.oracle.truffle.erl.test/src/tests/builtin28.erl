-module(builtin28).
-export([main/0]).

main() ->
	{
		try maps:merge(not_a_map1, #{}) catch error:{badmap, V} -> {ok, badmap, V} end,
		try maps:merge(#{}, not_a_map2) catch error:{badmap, V} -> {ok, badmap, V} end,
		maps:merge(#{}, #{})
	}.
