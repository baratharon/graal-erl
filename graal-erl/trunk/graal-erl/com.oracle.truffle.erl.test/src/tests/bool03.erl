-module(bool03).
-export([main/0]).

main() ->
	{
		false xor false,
		false xor true,
		true xor false,
		true xor true
	}.
