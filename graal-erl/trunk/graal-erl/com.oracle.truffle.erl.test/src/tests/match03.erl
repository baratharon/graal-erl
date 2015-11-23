-module(match03).
-export([main/0]).

f(1+1) -> a;
f(X = Y) ->
	{X, Y}.

main() ->
	f(42).
