-module(match05).
-export([main/0]).

f(X = Y = Z) ->
	{X, Y, Z}.

main() ->
	f(42).
