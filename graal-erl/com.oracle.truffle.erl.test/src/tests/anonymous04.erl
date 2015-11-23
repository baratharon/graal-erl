-module(anonymous04).
-export([main/0]).

g(Fun, Arg) ->
	Fun(Arg).

main()->
	X = 1,
	Fun = fun(N) when is_integer(N) -> N + X; (_) -> 0 end,
	g(Fun, 10).
