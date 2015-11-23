-module(builtin14).
-export([main/0]).

main() ->
	{
		lists:keyfind(1, 1, [a]),
		lists:keyfind(a, 1, [a]),
		lists:keyfind(a, 1, [{a}]),
		lists:keyfind(1, 1, [{1.0}]),
		lists:keyfind(1, 10, [{1.0}])
	}.
