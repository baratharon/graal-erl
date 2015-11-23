-module(builtin04).
-export([main/0]).

main() ->
	Fun = fun main/0,
	{
		is_function(Fun),
		is_function(Fun, 0),
		is_function(Fun, 1),
		is_function(Fun, 2),
		is_function(Fun, (1 bsl 1000))
	}.
