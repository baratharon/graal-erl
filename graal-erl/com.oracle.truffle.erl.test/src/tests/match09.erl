-module(match09).
-export([main/0]).

main() ->
	(A = B) = 42,
	{A, B}.
