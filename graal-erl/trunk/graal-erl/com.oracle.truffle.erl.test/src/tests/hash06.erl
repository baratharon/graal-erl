-module(hash06).
-export([main/0]).

main() ->
	Range = (1 bsl 27) - 1,
	{
		erlang:phash(1000000000000000000000000000000000000, Range),
		erlang:phash([1000000000000000000000000000000000000], Range),
		erlang:phash([1000000000000000000000000000000000000,1], Range),
		erlang:phash([1000000000000000000000000000000000000|1], Range)
	}.
