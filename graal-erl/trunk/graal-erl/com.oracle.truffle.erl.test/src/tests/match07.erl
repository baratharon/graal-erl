-module(match07).
-export([main/0]).

f(((X = Y) = Z) = Z) ->
	{X, Y, Z}.

main() ->
	f(42).
