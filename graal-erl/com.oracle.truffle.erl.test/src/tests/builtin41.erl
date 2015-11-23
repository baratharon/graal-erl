-module(builtin41).
-export([main/0]).

main() ->
	Old = 123,
	{
		try erlang:crc32(Old, 1) catch error:badarg -> {ok} end,
		try erlang:crc32(Old, [1000]) catch error:badarg -> {ok} end,
		erlang:crc32(Old, []),
		erlang:crc32(Old, <<>>),
		erlang:crc32(Old, [1]),
		erlang:crc32(Old, <<1>>),
		erlang:crc32(Old, [1,2,3,4]),
		erlang:crc32(Old, <<1,2,3,4>>),
		erlang:crc32(Old, [[<<1>>,[<<2,3>>],<<4,5>>,<<>>,[<<6>>]],[[[[<<>>,<<7>>]]]]])
	}.
