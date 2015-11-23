-module(bin22).
-export([main/0]).

main() ->
	Bin = <<41351:17>>,
	<<
		X1:7,
		X2:2,
		X3:6,
		X4:2
	>> = Bin,
	[X1, X2, X3, X4].
