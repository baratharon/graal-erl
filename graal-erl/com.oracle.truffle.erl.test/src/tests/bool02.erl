-module(bool02).
-export([main/0]).

main() ->
	{
		false or false,
		false or true,
		true or false,
		true or true
	}.
