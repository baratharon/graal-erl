-module(bin49).
-export([main/0]).

main() ->
	Bin = <<1,2,3,4, 5:5>>,
	{
		try binary_to_list(Bin, 0, 0) catch error:badarg -> {ok} end,
		try binary_to_list(Bin, 4, 5) catch error:badarg -> {ok} end,
		try binary_to_list(Bin, 1, 5) catch error:badarg -> {ok} end,
		binary_to_list(Bin, 1, 1),
		binary_to_list(Bin, 1, 2),
		binary_to_list(Bin, 1, 3),
		binary_to_list(Bin, 1, 4),
		binary_to_list(Bin, 2, 3)
	}.
