-module(builtin29).
-export([main/0]).

main() ->
	{
		maps:merge(#{a => b}, #{}),
		maps:merge(#{}, #{a => b, c => d}),
		maps:merge(#{a => 1, b => 2, c => 3}, #{b => 4, a => 5}),
		maps:merge(#{}, #{})
	}.
