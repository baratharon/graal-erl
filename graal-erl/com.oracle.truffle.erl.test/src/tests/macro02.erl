-module(macro02).
-export([main/0]).
-define(FIVE, (1+2*2)).

main() ->
	2 * ?FIVE.
