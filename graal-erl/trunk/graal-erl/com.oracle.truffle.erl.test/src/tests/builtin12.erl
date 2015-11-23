-module(builtin12).
-export([main/0]).

main() ->
	{
		lists:keymember(1, 1, [a]),
		lists:keymember(a, 1, [a]),
		lists:keymember(a, 1, [{a}]),
		lists:keymember(1, 1, [{1.0}]),
		lists:keymember(1, 10, [{1.0}])
	}.
