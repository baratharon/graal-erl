-module(bin29).
-export([main/0]).

main() ->
	Bin = <<11, 22, 33>>,
	Bit = <<Bin/binary, 3:3>>,
	{
		try bit_size(not_a_binary) catch error:badarg -> {ok} end,
		try byte_size(not_a_binary) catch error:badarg -> {ok} end,
		bit_size(<<>>),
		byte_size(<<>>),
		bit_size(Bin),
		byte_size(Bin),
		bit_size(Bit),
		byte_size(Bit)
	}.
