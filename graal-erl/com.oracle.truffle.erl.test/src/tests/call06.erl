-module(call06).
-export([main/0]).

my_call(Module, Func, X) ->
	Module:Func(X).

main()->
	my_call(erlang, is_integer, 10+20) xor my_call(erlang, is_integer, 2==3).
