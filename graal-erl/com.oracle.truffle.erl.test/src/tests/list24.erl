-module(list24).
-export([main/0]).

main() ->
	[{X,Y} || X <- [], Y <- [2,3]].
