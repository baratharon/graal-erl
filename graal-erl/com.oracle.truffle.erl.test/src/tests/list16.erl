-module(list16).
-export([main/0]).

main() ->
	{
		[] -- [],
		[] -- [1],
		[1,2,3,4,5] -- [1,2,3],
		[1,2,3,4,5] -- [2,3,4],
		[1,2,3,4,5] -- [4,3,2],
		[2,4,2] -- [4,2,2],
		'-- end --'
	}.
