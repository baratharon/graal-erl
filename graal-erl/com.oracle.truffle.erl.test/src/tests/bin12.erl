-module(bin12).
-export([main/0]).

main() ->
	Bin1 = <<11, 12>>,
	Bin2 = <<Bin1/binary, 13, 14>>,
	<<X:16, Rest/bitstring>> = Bin2,
	{X, Rest}.
