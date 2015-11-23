-module(list19).
-export([main/0]).

base_list() ->
	[1, 2].

main() ->
	[{X,Y} || X <- base_list(), Y <- base_list()].
