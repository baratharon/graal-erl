-module(list37).
-export([main/0]).

main() ->
	[catch S || S <- [1,2,3]].
