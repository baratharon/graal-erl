-module(match06).
-export([main/0]).

f(X = Y = Z = W) ->
	{X, Y, Z, W}.

main() ->
	f(42).
