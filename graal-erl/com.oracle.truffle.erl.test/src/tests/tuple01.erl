-module(tuple01).
-export([guards/2, main/0]).

guards(X, Y) when is_integer(X), is_integer(Y), X>Y ->
	X;
guards(X, Y) ->
	Y.

main() ->
	{guards(3, 2), guards(a, b)}.
