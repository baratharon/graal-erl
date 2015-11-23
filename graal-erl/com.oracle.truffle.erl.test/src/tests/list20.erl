-module(list20).
-export([main/0]).

base_list() ->
	[1, 2, 3, 4, 5].

main() ->
	[X || X <- base_list(), 1==(X rem 2)].
