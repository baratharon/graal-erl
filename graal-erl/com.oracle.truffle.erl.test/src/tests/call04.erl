-module(call04).
-export([main/0]).
-compile({no_auto_import, [is_integer/1]}).

is_integer(X) ->
	erlang:is_integer(X).

main()->
	is_integer(10+20) xor is_integer(2==3).
