-module(ext07).
-export([main/0]).

main() ->
	Num = -(16#100000000000000000000000000000),
	term_to_binary(Num).
