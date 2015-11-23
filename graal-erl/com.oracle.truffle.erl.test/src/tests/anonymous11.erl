-module(anonymous11).
-export([main/0]).

'g*'(Fun, Arg) ->
	N = 10 + Arg,
	M = 20 + Arg,
	{N, M, Fun(Arg)}.

g(Fun, Arg) ->
	'g*'(Fun, Arg).

f(A)->
	X = 1,
	Fun = fun
		(N) when is_integer(N) ->
			M=3,
			(fun() -> M + 1 end)() + A + X;
		(_) ->
			0
	end,
	g(Fun, A*2).

main() ->
	{ f(1), f(2) }.
