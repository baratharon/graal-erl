-module(bin17).
-export([main/0]).

main() ->
	Bin1 = <<11, 22>>,
	Bin2 = <<Bin1/binary, 33, 44>>,
	<<
		X1:(1+2+3+4+5)/bitstring,
		X6:6/bitstring,
		X7:7/bitstring,
		Rest/bitstring
	>> = Bin2,
	[X1, X6, X7, Rest].
