-module(convert02).
-export([main/0]).

main() ->
	{
		try list_to_integer("-") catch error:badarg -> ok end,
		try list_to_integer("@#") catch error:badarg -> ok end,
		try list_to_integer("--1") catch error:badarg -> ok end,
		try list_to_integer("aasf") catch error:badarg -> ok end
	}.
