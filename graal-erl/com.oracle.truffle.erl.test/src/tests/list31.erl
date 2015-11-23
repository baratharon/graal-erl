-module(list31).
-export([main/0]).

flatten(ListOfLists) ->
	[X || List <- ListOfLists, X <- List].

main() ->
	flatten([[1,2], [3,4,5], [6,a], [b,c,d,e]]).
