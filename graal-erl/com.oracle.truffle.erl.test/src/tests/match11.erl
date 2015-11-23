-module(match11).
-export([main/0]).

f(L = J = [H|T]) ->
	{H, T, L, J}.

main() ->
	f([100, 200, 300, 400]).
