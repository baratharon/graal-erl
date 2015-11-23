-module(hash09).
-export([main/0, fun1/0, fun2/1]).

fun1() ->
	ok.

fun2(X) ->
	[[]|X].

main() ->
	Range = (1 bsl 27) - 1,
	Fun1 = fun fun1/0,
	Fun2 = fun fun2/1,
	{
		erlang:phash(Fun1, Range) =:= erlang:phash(Fun1, Range),
		erlang:phash(Fun2, Range) =:= erlang:phash(Fun2, Range),
		erlang:phash(Fun1, Range) =/= erlang:phash(Fun2, Range)
	}.
