-module(builtin30).
-export([main/0]).

main() ->
	{
		try erlang:iolist_size(1) catch error:badarg -> {ok} end,
		try erlang:iolist_size([1000]) catch error:badarg -> {ok} end,
		erlang:iolist_size([]),
		erlang:iolist_size(<<>>),
		erlang:iolist_size([1]),
		erlang:iolist_size(<<1>>),
		erlang:iolist_size([1,2,3,4]),
		erlang:iolist_size(<<1,2,3,4>>),
		erlang:iolist_size([[<<1>>,[<<2,3>>],<<4,5>>,<<>>,[<<6>>]],[[[[<<>>,<<7>>]]]]])
	}.
