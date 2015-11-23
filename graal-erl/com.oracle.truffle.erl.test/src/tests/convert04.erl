-module(convert04).
-export([main/0]).

main() ->
	{
		list_to_integer("ffff", 16),
		list_to_integer("ffff", 32),
		list_to_integer("1000", 16),
		list_to_integer("1000", 32)
	}.
