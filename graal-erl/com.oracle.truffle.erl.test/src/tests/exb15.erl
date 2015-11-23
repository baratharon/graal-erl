-module(exb15).
-export([main/0]).

main() ->
	binary_to_term(<<131,116,0,0,0,0>>).
