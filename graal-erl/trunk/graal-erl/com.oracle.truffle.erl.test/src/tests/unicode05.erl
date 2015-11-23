-module(unicode05).
-export([main/0]).

main() ->
	{
		unicode:bin_is_7bit(<<>>),
		unicode:bin_is_7bit(<<226, 156, 144, 240, 158, 137, 128, 196, 172>>),
		unicode:bin_is_7bit(<<0, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0, 100, 0, 0, 0, 101>>),
		unicode:bin_is_7bit(<<97, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0, 100, 0, 0, 0, 101, 0, 0, 0>>)
	}.
