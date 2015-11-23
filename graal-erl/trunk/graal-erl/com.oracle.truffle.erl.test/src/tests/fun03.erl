-module(fun03).
-export([main/0]).

main() ->
	F = fun NamedFun() -> is_function(NamedFun) end,
	F().
