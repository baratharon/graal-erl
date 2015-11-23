-module(hash08).
-export([main/0]).

main() ->
	Range = (1 bsl 27) - 1,
	{
		erlang:phash(<<>>, Range),
		erlang:phash(<<1>>, Range),
		erlang:phash(<<2>>, Range),
		erlang:phash(<<1:1>>, Range),
		erlang:phash(<<2:2>>, Range),
		erlang:phash(<<3:3>>, Range),
		erlang:phash(<<11,22,33,44:7>>, Range),
		erlang:phash(<<4:4>>, Range),
		erlang:phash(<<5:5>>, Range)
	}.
