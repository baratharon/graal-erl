-module(builtin31).
-export([main/0]).

main() ->
	{
		try erlang:iolist_to_binary(1) catch error:badarg -> {ok} end,
		try erlang:iolist_to_binary([1000]) catch error:badarg -> {ok} end,
		erlang:iolist_to_binary([]),
		erlang:iolist_to_binary(<<>>),
		erlang:iolist_to_binary([1]),
		erlang:iolist_to_binary(<<1>>),
		erlang:iolist_to_binary([1,2,3,4]),
		erlang:iolist_to_binary(<<1,2,3,4>>),
		erlang:iolist_to_binary([[<<1>>,[<<2,3>>],<<4,5>>,<<>>,[<<6>>]],[[[[<<>>,<<7>>]]]]])
	}.
