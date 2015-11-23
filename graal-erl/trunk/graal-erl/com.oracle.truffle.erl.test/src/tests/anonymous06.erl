-module(anonymous06).
-export([main/0]).

g(Fun, Arg) ->
	Fun(Arg).

f(A)->
	X = 1,
	Fun = fun(N) when is_integer(N) -> A + X; (_) -> 0 end,
	g(Fun, 0).

main() ->
	f(10).
