-module(match10).
-export([main/0]).

f([H|T] = L) ->
	{H, T, L}.

main() ->
	f([100, 200, 300, 400]).
