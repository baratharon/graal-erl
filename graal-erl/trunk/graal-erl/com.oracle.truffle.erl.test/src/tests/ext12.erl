-module(ext12).
-export([main/0]).

main() ->
	term_to_binary([500]).
