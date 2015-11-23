-module(bin07).
-export([main/0]).

main() ->
	Bin1 = <<11, 22>>,
	Bin2 = <<Bin1/binary, 33, 44>>,
	Bin2.
