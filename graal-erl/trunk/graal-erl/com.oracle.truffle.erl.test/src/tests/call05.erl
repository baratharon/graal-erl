-module(call05).
-export([main/0]).
-compile({no_auto_import, [is_integer/1]}).

is_integer(Module, X) ->
	Module:is_integer(X).

main()->
	is_integer(erlang, 10+20) xor is_integer(erlang, 2==3).
