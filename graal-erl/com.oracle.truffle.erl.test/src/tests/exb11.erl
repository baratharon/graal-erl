-module(exb11).
-export([main/0]).

main() ->
	binary_to_term(<<131,106>>).
