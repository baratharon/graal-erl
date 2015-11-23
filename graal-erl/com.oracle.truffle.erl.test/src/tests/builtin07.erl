-module(builtin07).
-export([main/0]).

main() ->
	{
		float(1),
		float(2),
		float(1.1),
		float(1.5),
		float(100.9),
		float(101.01),
		float(999.99),
		float(-1.1),
		float(-1.5),
		float(-1.9)
	}.
