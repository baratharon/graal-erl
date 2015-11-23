-module(bin34).
-export([main/0]).

main() ->
	try
		<<123456:4/big-little-signed-integer-unit:8>>
	catch
		X:Y -> {caught_error, {X, Y}}
	end.
