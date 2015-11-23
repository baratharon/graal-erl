-module(call02).
-export([main/0, get_main/0, main2/0]).

secret_adder(X, Y) ->
	X + Y.

my_invoke(F, A, B) ->
	(F())(A, B).

secret_wrapper() ->
	fun secret_adder/2.

main() ->
	my_invoke(fun secret_wrapper/0, 4, 5).

get_main() ->
	fun main/0.

main2() ->
	{main(), get_main()}.
