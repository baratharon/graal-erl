-module(unicode02).
-export([main/0]).

main() ->
	{
		unicode:characters_to_list(<<226, 156, 144, 240, 158, 137, 128, 196, 172>>, utf8),
		unicode:characters_to_list(<<0, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0, 100, 0, 0, 0, 101>>, utf32),
		unicode:characters_to_list(<<0, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0, 100, 0, 0, 0, 101>>, {utf32, big}),
		unicode:characters_to_list(<<97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0, 100, 0, 0, 0, 101, 0, 0, 0>>, {utf32, little})
	}.
