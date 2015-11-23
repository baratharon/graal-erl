-module(bin14).
-export([main/0]).

main() ->
	Bin1 = <<11, 22>>,
	Bin2 = <<Bin1/binary, 33, 44>>,
	<<X1:1, X2:2, X3:3, X4:4, X5:5, X6:6, X7:7, Rest/bitstring>> = Bin2,
	{X1, X2, X3, X4, X5, X6, X7, Rest}.
