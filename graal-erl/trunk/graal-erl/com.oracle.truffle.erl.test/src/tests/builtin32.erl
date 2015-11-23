-module(builtin32).
-export([main/0]).

main() ->
	{
		erlang:adler32_combine(123, 456, 12),
		erlang:adler32_combine(123, 456, 13),
		erlang:adler32_combine(123123, 456456, 12),
		erlang:adler32_combine(123123, 456456, 13)
	}.
