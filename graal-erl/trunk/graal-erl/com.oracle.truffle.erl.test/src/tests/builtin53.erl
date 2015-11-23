-module(builtin53).
-export([main/0]).

main() ->
	{
		try erlang:md5(1) catch error:badarg -> {ok} end,
		try erlang:md5([1000]) catch error:badarg -> {ok} end,
		erlang:md5([]),
		erlang:md5(<<>>),
		erlang:md5([1]),
		erlang:md5(<<1>>),
		erlang:md5([1,2,3,4]),
		erlang:md5(<<1,2,3,4>>),
		erlang:md5([[<<1>>,[<<2,3>>],<<4,5>>,<<>>,[<<6>>]],[[[[<<>>,<<7>>]]]]])
	}.
