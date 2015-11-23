-module(dict02).
-export([main/0]).

main() ->
	erase(),
	X = put(hello, world),
	Y = put(hello, world2),
	{X, Y, get(hello)}.
