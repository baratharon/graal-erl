-module(bin20).
-export([main/0]).

main() ->
	Bin = <<1:1>>,
	<<
		X1:1/bitstring,
		Rest/bitstring
	>> = Bin,
	{X1, Rest}.
