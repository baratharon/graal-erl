-module(hash05).
-export([main/0]).

main() ->
	Range = (1 bsl 27) - 1,
	{
		erlang:phash([], Range),
		erlang:phash([1], Range),
		erlang:phash([[]], Range),
		erlang:phash([1, 10000000], Range),
		erlang:phash([10000000], Range),
		erlang:phash([aaaa|bbbb], Range)
	}.
