-module(list12).
-export([main/0]).

main() ->
	{
		[],
		[1] ++ [2,2] ++ [3,3,3] ++ [4,4,4,4],
		'-- end --'
	}.
