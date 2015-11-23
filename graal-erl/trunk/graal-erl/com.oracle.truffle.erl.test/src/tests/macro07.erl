-module(macro07).
-export([main/0]).
-define(ADD2(A), A+?TWO).
-define(ADD2(A,B), A+B+?TWO).
-define(ADD2(A,B,C), A+B+C+?TWO).
-define(TWO, 2).

main() ->
	?ADD2(?ADD2(1, 2, 3), ?ADD2(4)).
