-module(number03).
-export([main/0]).

main() ->
	{
		(13 bsl 65) band (2#10101 + (1 bsl 67)),
		(13 bsl 65) bor 2#10101,
		(13 bsl 65) bxor 2#10101,
		(13 bsl 65) bsr 2#10101,

		'-- end --'
	}.
