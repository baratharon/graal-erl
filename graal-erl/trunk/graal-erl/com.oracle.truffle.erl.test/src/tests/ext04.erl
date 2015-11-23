-module(ext04).
-export([main/0]).

main() ->
	term_to_binary(16#1fffefdfc).
