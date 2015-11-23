-module(builtin34).
-export([main/0]).

main() ->
	Old = 123,
	{
		try erlang:adler32(Old, 1) catch error:badarg -> {ok} end,
		try erlang:adler32(Old, [1000]) catch error:badarg -> {ok} end,
		erlang:adler32(Old, []),
		erlang:adler32(Old, <<>>),
		erlang:adler32(Old, [1]),
		erlang:adler32(Old, <<1>>),
		erlang:adler32(Old, [1,2,3,4]),
		erlang:adler32(Old, <<1,2,3,4>>),
		erlang:adler32(Old, [[<<1>>,[<<2,3>>],<<4,5>>,<<>>,[<<6>>]],[[[[<<>>,<<7>>]]]]])
	}.
