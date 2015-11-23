-module(bin15).
-export([main/0]).

main() ->
	Bin1 = <<11, 22>>,
	Bin2 = <<Bin1/binary, 33, 44>>,
	<<X1:1, Z1:0, X2:2, Z2:0, X3:3, Z3:0, X4:4, Z4:0,
		X5:5, Z5:0, X6:6, Z6:0, X7:7, Z7:0,
		Rest/bitstring>> = Bin2,
	{X1, X2, X3, X4, X5, X6, X7, Rest, Z1+Z2+Z3+Z4+Z5+Z6+Z7}.
