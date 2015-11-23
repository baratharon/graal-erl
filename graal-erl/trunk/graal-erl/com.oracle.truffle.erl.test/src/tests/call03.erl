-module(call03).
-export([main/0]).

func(Func, Arg) ->
	Func(Arg).

main()->
	func(fun erlang:is_integer/1, 11+22+33).
