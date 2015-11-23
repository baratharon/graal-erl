-module(list18).
-export([main/0]).

base_list() ->
	[1, 2, 3].

main() ->
	[{X,Y} || X=Y <- base_list()].
