-module(bool01).
-export([main/0]).

main() ->
	{
		false and false,
		false and true,
		true and false,
		true and true
	}.
