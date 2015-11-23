-module(compare01).
-export([main/0]).

main() ->
	{
		false < false,
		true < false,
		false < true,
		true < true
	}.
