-module(builtin35).
-export([main/0]).

main() ->
	Old = 123,
	{
		try erlang:adler32([<<>>,<<10,20>>,<<1:1>>]) catch error:badarg -> ok end,
		try erlang:adler32(Old, [<<>>,<<10,20>>,<<1:1>>]) catch error:badarg -> ok end
	}.
