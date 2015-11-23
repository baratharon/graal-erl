-module(dict01).
-export([main/0]).

main() ->
	erase(),
	X = put(hello, world),
	{X, get(hello)}.
