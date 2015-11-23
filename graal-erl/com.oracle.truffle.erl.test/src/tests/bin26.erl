-module(bin26).
-export([main/0]).

main() ->
	Bin = <<11, 22, 33, 44, 55, 66, 77, 88, 99, 111>>,
	<<X:77, Rest/bitstring>> = Bin,
	{X, Rest}.
