-module(ext06).
-export([main/0]).

main() ->
	term_to_binary(16#100000000000000000000000000000).
