-module(list22).
-export([main/0]).

base_list() ->
	[1, 2, 3].

main() ->
	[{X,Y} || X <- base_list(), Y <- base_list(), 1==(X rem 2)].
