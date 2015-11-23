-module(list25).
-export([main/0]).

main() ->
	[{X,Y} || X <- [2,3], Y <- []].
