-module(ext03).
-export([main/0]).

main() ->
	term_to_binary(10000).
