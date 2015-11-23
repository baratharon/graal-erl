-module(hash03).
-export([main/0]).

main() ->
	Range = (1 bsl 27) - 1,
	IntMax = (1 bsl 32),
	{
		erlang:phash(IntMax, Range),
		erlang:phash(-IntMax, Range),
		erlang:phash(1000000000000, Range),
		erlang:phash(-1000000000000, Range)
	}.
