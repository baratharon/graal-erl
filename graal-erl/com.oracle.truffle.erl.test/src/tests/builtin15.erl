-module(builtin15).
-export([main/0]).

main() ->
	{
		try lists:keymember(1, 0, []) catch error:badarg -> ok end,
		try lists:keymember(1, 1, not_a_list) catch error:badarg -> ok end,
		try lists:keysearch(1, 0, []) catch error:badarg -> ok end,
		try lists:keysearch(1, 1, not_a_list) catch error:badarg -> ok end,
		try lists:keyfind(1, 0, []) catch error:badarg -> ok end,
		try lists:keyfind(1, 1, not_a_list) catch error:badarg -> ok end
	}.
