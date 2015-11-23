-module(bin31).
-export([main/0]).

main() ->
	{
		<<123456:4/little-signed-integer-unit:8>>,
		<<123456:4/big-signed-integer-unit:8>>
	}.
