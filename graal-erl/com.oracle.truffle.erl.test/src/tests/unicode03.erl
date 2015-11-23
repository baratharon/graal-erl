-module(unicode03).
-export([main/0]).

main() ->
	{
		unicode:characters_to_binary(<<97, 98, 99, 100, 101>>, latin1),
		unicode:characters_to_binary([12345, 23456, 100000], unicode),
		unicode:characters_to_binary(<<0, 97, 0, 98, 0, 99, 0, 100, 0, 101>>, utf16),
		unicode:characters_to_binary(<<0, 97, 0, 98, 0, 99, 0, 100, 0, 101>>, {utf16, big}),
		unicode:characters_to_binary(<<97, 0, 98, 0, 99, 0, 100, 0, 101, 0>>, {utf16, little})
	}.
