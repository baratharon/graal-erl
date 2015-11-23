-module(list26).
-export([main/0]).

main() ->
	[X || true, X <- [1,2]].
