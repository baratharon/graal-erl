-module(float06).
-export([main/0]).

main() ->
	{
		float_to_list(1.0, [{decimals, 20}, compact]),
		float_to_list(10.0, [{decimals, 20}, compact]),
		float_to_list(0.0625, [{decimals, 20}, compact])
	}.
