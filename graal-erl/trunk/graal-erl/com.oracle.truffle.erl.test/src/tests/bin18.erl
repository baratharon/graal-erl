-module(bin18).
-export([main/0]).

main() ->
	Bin1 = <<11, 22>>,
	Bin2 = <<Bin1/binary, 33, 44>>,
	<<
		X1:1/bitstring,
		X2:2/bitstring,
		X3:3/bitstring,
		X4:4/bitstring,
		X5:5/bitstring,
		X6:6/bitstring,
		X7:7/bitstring,
		Rest/bitstring
	>> = Bin2,
	[X1, X2, X3, X4, X5, X6, X7, Rest].
