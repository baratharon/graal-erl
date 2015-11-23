-module(bin16).
-export([main/0]).

main() ->
	Bin1 = <<1:1, 1:1, 1:1, 0:7>>,
	<<
		X1:1/bitstring,
		X2:2/bitstring,
		Rest/bitstring
	>> = Bin1,
	[X1, X2, Rest].
