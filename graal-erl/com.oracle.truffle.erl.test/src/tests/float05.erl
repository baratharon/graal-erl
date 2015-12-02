-module(float05).
-export([main/0]).

main() ->
	{
		float_to_list(1.25, [{decimals, 20}]),
		float_to_list(1.25, [{decimals, 20}, compact]),
		float_to_list(1.25, [{decimals, 20}, {scientific, 11}, {decimals, 4}])
	}.
