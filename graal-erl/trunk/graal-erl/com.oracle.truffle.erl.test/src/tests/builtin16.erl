-module(builtin16).
-export([main/0]).

main() ->
	Map = #{ a => 10, b => 20, 10 => a, 20 => b, 1.1 => fun(X) -> X*2 end},
	{
		try maps:get(a, not_a_map) catch error:{badmap, V} -> {ok, badmap, V} end,
		try maps:get(no_such, Map) catch error:{badkey, V} -> {ok, badkey, V} end,
		maps:get(a, Map),
		maps:get(20, Map),
		(maps:get(1.1, Map))(13)
	}.
