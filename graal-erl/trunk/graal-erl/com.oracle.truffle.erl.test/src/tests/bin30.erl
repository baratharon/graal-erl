-module(bin30).
-export([main/0]).

main() ->
	Bin = <<10,20,30>>,
	Len = bit_size(Bin) + 13,
	try
		<<Bin:Len/bitstring>>
	catch
		error:badarg -> {ok, badarg}
	end.
