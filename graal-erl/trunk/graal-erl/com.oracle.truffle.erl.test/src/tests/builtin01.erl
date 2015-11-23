-module(builtin01).
-export([adder/1, main/0]).

adder(X) when is_integer(X) ->
	X + 1;
adder(X) ->
	0.

main() ->
	adder(20).
