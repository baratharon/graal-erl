-module(adder03).
-export([adder/1, adder/2, main/0]).

adder(X) ->
	X + 1.

adder(X, Y) ->
	X + Y.

main() ->
	adder(adder(39), adder(3)).
