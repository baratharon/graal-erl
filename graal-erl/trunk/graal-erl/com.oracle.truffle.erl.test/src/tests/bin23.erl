-module(bin23).
-export([main/0]).

main() ->
	Bin = <<41351:17>>,
	<<
		X1:7/bitstring,
		X2:2/bitstring,
		X3:6/bitstring,
		X4:2/bitstring
	>> = Bin,
	[X1, X2, X3, X4].
