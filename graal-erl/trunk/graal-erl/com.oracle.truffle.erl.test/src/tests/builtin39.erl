-module(builtin39).
-export([main/0]).

main() ->
	{
		erlang:crc32_combine(123, 456, 12),
		erlang:crc32_combine(123, 456, 13),
		erlang:crc32_combine(123123, 456456, 12),
		erlang:crc32_combine(123123, 456456, 13)
	}.
