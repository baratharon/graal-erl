-module(bin46).
-export([main/0]).

main() ->
	BitCount = 112,
	Bin = <<(255 bsl (BitCount - 8)):BitCount>>,
	<<N:BitCount>> = Bin,
	{ BitCount, Bin, N }.
