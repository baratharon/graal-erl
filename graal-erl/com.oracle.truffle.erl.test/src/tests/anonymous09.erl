-module(anonymous09).
-export([main/0]).

'g*'(Fun, Arg) ->
	N = 88,
	M = 99,
	{N, M, Fun(Arg)}.

g(Fun, Arg) ->
	'g*'(Fun, Arg).

f(A)->
	X = 1,
	Fun = fun(N) when is_integer(N) -> M=3, A + X + M; (_) -> 0 end,
	g(Fun, 0).

main() ->
	f(10).
