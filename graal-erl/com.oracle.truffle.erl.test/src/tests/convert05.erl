-module(convert05).
-export([main/0]).

main() ->
	{
		try list_to_integer("ffff", 15) catch error:badarg -> ok end,
		try list_to_integer("2", 2) catch error:badarg -> ok end,
		try list_to_integer("zzz", 2) catch error:badarg -> ok end,
		try list_to_integer("0", 1) catch error:badarg -> ok end,
		try list_to_integer("f", 500) catch error:badarg -> ok end,
		try list_to_integer("aa", 10) catch error:badarg -> ok end,
		try list_to_integer("AA", 10) catch error:badarg -> ok end,
		try list_to_integer("9", 9) catch error:badarg -> ok end,
		try list_to_integer("b", 11) catch error:badarg -> ok end,
		try list_to_integer("B", 11) catch error:badarg -> ok end
	}.
