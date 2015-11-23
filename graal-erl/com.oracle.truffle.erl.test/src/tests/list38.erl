-module(list38).
-export([main/0]).

main() ->
	[ [N || N <- lists:seq(1, M) ] || M <- [1,2,3] ].
