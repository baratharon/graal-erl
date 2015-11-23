-module(ext18).
-export([main/0]).

main() ->
	term_to_binary(3.14).
