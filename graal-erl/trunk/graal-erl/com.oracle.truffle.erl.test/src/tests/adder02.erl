-module(adder02).
-export([adder/1, main/0, main2/0]).

adder(X) ->
	X + 1.

main2() ->
	adder(2).

main() ->
	main2().
