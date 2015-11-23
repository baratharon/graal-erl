-module(libstring02).
-export([main/0]).
-import(string,[chars/3]).

main() ->
	chars($A, 15, ".").
