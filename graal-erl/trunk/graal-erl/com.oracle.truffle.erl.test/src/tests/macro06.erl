-module(macro06).
-export([main/0]).
-define(ADD2(A,B), A+B+?TWO).
-define(TWO, 2).

main() ->
	?ADD2(3, 4).
