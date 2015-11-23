-module(list15).
-export([main/0]).

f([]) ->
	0;
f([H,H,H | L]) ->
	2 + f(L);
f([H,H | L]) ->
	1 + f(L);
f([_ | L]) ->
	f(L).

main() ->
	{
		f([]),
		f([1,2,2,3,3,3]),
		f([1] ++ [2,2] ++ [3,3,3] ++ [4,4,4,4]),
		'-- end --'
	}.
