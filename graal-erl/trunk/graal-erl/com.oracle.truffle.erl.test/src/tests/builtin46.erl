-module(builtin46).
-export([main/0]).

main() ->
	B1 = <<>>,
	B2 = <<11, 22>>,
	B3 = <<11, 22:7>>,
	B4 = <<11:3, 22:3, 33:3, 44:3>>,
	B5 = <<11:3, 22:3, 33:3, 44:3, 55:4>>,
	{
		is_binary(B1),
		is_bitstring(B1),
		is_binary(B2),
		is_bitstring(B2),
		is_binary(B3),
		is_bitstring(B3),
		is_binary(B4),
		is_bitstring(B4),
		is_binary(B5),
		is_bitstring(B5)
	}.
