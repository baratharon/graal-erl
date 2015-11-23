-module(macro11).
-export([main/0]).
-define(MACRO(), hello).

main() ->
	?MACRO().
