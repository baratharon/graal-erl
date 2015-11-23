-module(ext13).
-export([main/0]).

main() ->
	term_to_binary([500|600]).
