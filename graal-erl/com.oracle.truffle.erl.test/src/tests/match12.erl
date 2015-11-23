-module(match12).
-export([main/0]).

f(L = [H|T] = J) ->
	{H, T, L, J}.

main() ->
	f([100, 200, 300, 400]).
