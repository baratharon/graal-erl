-module(ext01).
-export([main/0]).

main() ->
	term_to_binary(atom).
