-module(ext10).
-export([main/0]).

main() ->
	term_to_binary({1,atom,2,other}).
