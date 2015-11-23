-module(hash02).
-export([main/0]).

main() ->
	Range = (1 bsl 27) - 1,
	{
		erlang:phash(a, Range),
		erlang:phash(aa, Range),
		erlang:phash(aaa, Range),
		erlang:phash(aaaa, Range),
		erlang:phash(0, Range),
		erlang:phash(1, Range),
		erlang:phash(10000, Range),
		erlang:phash(-10000, Range)
	}.
