-module(anonymous05).
-export([main/0]).

f(A)->
	X = 1,
	Fun = fun(N) when is_integer(N) -> A + X; (_) -> 0 end,
	Fun(0).

main() ->
	f(10).
