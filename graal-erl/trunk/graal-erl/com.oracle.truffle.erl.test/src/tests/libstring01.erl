-module(libstring01).
-export([main/0]).

main() ->
	string:chars($A, 15, ".").
