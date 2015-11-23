-module(call01).
-export([main/0]).

f(N) ->
	N.

main() ->
	f(42).
