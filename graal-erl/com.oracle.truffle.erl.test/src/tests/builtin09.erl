-module(builtin09).
-export([main/0]).

main() ->
	{
		round(1),
		round(2),
		round(1.1),
		round(1.5),
		round(100.9),
		round(101.01),
		round(999.99),
		round(-1.1),
		round(-1.5),
		round(-1.9)
	}.
