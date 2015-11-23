-module(bin47).
-export([main/0]).

main() ->
	BitCount = 112,
	Bin = <<(255 bsl (BitCount - 8)):BitCount>>,
	<<N:BitCount/signed>> = Bin,
	{ BitCount, Bin, N }.
