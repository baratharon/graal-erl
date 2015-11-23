-module(builtin40).
-export([main/0]).

main() ->
	{
		try erlang:crc32(1) catch error:badarg -> {ok} end,
		try erlang:crc32([1000]) catch error:badarg -> {ok} end,
		erlang:crc32([]),
		erlang:crc32(<<>>),
		erlang:crc32([1]),
		erlang:crc32(<<1>>),
		erlang:crc32([1,2,3,4]),
		erlang:crc32(<<1,2,3,4>>),
		erlang:crc32([[<<1>>,[<<2,3>>],<<4,5>>,<<>>,[<<6>>]],[[[[<<>>,<<7>>]]]]])
	}.
