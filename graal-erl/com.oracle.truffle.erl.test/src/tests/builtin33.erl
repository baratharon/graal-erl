-module(builtin33).
-export([main/0]).

main() ->
	{
		try erlang:adler32(1) catch error:badarg -> {ok} end,
		try erlang:adler32([1000]) catch error:badarg -> {ok} end,
		erlang:adler32([]),
		erlang:adler32(<<>>),
		erlang:adler32([1]),
		erlang:adler32(<<1>>),
		erlang:adler32([1,2,3,4]),
		erlang:adler32(<<1,2,3,4>>),
		erlang:adler32([[<<1>>,[<<2,3>>],<<4,5>>,<<>>,[<<6>>]],[[[[<<>>,<<7>>]]]]])
	}.
