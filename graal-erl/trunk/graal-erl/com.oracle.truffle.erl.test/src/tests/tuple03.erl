-module(tuple03).
-export([main/0]).

main() ->
	Tuple = {1, bb, 333, dddd},
	{
		element(1, Tuple),
		element(2, Tuple),
		element(3, Tuple),
		element(4, Tuple)
	}.
