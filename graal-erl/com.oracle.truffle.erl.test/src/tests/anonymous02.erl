-module(anonymous02).
-export([main/0]).

main()->
	Fun = fun(N) when is_integer(N) -> N + 1; (_) -> 0 end,
	Fun(10).
