-module(builtin13).
-export([main/0]).

main() ->
	{
		lists:keysearch(1, 1, [a]),
		lists:keysearch(a, 1, [a]),
		lists:keysearch(a, 1, [{a}]),
		lists:keysearch(1, 1, [{1.0}]),
		lists:keysearch(1, 10, [{1.0}])
	}.
