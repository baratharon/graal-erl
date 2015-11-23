-module(list28).
-export([main/0]).

base_list() ->
	[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16].

f(N) ->
	N.

main() ->
	[f(X) || X <- base_list()].
