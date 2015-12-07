-module(match20).
-export([main/0]).

f() ->
	5.

main() ->
	Fun = fun(Fun) -> Fun() end,
	Fun(fun f/0).
