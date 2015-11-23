-module(fun02).
-export([main/0]).

main() ->
	F = fun Fact(0) -> 1; Fact(N) -> N*Fact(N-1) end,
	F(5).
