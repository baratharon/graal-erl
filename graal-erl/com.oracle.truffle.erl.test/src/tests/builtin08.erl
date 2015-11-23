-module(builtin08).
-export([main/0]).

main() ->
	{
		trunc(1),
		trunc(2),
		trunc(1.1),
		trunc(1.5),
		trunc(100.9),
		trunc(101.01),
		trunc(999.99),
		trunc(-1.1),
		trunc(-1.5),
		trunc(-1.9)
	}.
