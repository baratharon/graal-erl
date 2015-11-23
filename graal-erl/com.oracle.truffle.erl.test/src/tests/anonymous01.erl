-module(anonymous01).
-export([main/0]).

main()->
	Fun = fun(N) -> N + 1 end,
	Fun(10).
