-module(bin45).
-export([main/0]).

main() ->
	{
		try binary_to_list(10) catch error:badarg -> badarg end,
		try binary_to_list({}) catch error:badarg -> badarg end,
		try binary_to_list([]) catch error:badarg -> badarg end,
		try binary_to_list(<<1000:17>>) catch error:badarg -> badarg end,
		binary_to_list(<<>>),
		binary_to_list(<<1000:16>>),
		binary_to_list(<<10,20,30>>),
		binary_to_list(<<11:7, 22:9, 33:7, 44:9>>)
	}.
