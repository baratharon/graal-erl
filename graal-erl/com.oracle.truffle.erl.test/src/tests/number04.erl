-module(number04).
-export([main/0]).

main() ->
	{
		bnot (-2),
		bnot 1,
		bnot (-3),
		bnot 0,
		bnot (7 bsl 66),
		bnot (-(7 bsl 66)),
		-(bnot(-(bnot(2000000000000000000000)))),
		(-(bnot(-(bnot(2000000000000000000000))))) band 255,
		(-(bnot(-(bnot(6514236417253165134684163165165346323547716549849847781877))))) band 255,

		'-- end --'
	}.
