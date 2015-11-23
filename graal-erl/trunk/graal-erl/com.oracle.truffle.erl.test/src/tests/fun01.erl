-module(fun01).
-export([main/0]).

main() ->
	F = fun (0, _) -> 1; (N, Fact) -> N*Fact(N-1, Fact) end,
	F(5, F).
