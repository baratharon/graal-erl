-module(anonymous03).
-export([main/0]).

main()->
	X = 1,
	Fun = fun(N) when is_integer(N) -> N + X; (_) -> 0 end,
	Fun(10).
