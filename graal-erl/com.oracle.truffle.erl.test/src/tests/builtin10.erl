-module(builtin10).
-export([main/0]).

main() ->
	List = [1, 2, 3, 4, a, b, c, d],
	{
		lists:member(x, List),
		lists:member(1.0, List),
		lists:member(a, List),
		lists:member(4, List)
	}.
