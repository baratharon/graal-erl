-module(call07).
-export([main/0]).

my_apply(Fun, Arg) ->
	Fun(Arg).

my_call(Module, Func, X) ->
	my_apply(fun Module:Func/1, X).

main()->
	my_call(erlang, is_integer, 10+20) xor my_call(erlang, is_integer, 2==3).
