-module(list17).
-export([main/0]).

base_list() ->
	[1, 2, 3].

main() ->
	[X || X <- base_list()].
