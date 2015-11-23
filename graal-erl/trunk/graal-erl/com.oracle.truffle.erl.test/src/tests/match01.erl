-module(match01).
-export([adder/1, try_match/1, eq/2, neg/1, main/0]).

adder(X) ->
	Y = X,
	Y = Y,
	Y + 1.

try_match(X) ->
	X = X.

eq(A, B) ->
	A == B.

neg(Y) ->
	-Y.

main() ->
	neg(adder(neg(try_match(5)))).
