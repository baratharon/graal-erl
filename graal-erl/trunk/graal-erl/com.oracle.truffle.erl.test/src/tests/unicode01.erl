-module(unicode01).
-export([main/0]).

main() ->
	{
		unicode:characters_to_list(<<97, 98, 99, 100, 101>>, latin1),
		unicode:characters_to_list(<<0, 97, 0, 98, 0, 99, 0, 100, 0, 101>>, utf16),
		unicode:characters_to_list(<<0, 97, 0, 98, 0, 99, 0, 100, 0, 101>>, {utf16, big}),
		unicode:characters_to_list(<<97, 0, 98, 0, 99, 0, 100, 0, 101, 0>>, {utf16, little})
	}.
