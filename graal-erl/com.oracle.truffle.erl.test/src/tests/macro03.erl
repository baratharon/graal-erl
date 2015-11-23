-module(macro03).
-export([main/0]).
-define(TWO, (1+1)).
-define(FIVE, (1+?TWO*?TWO)).

main() ->
	?TWO * ?FIVE.
