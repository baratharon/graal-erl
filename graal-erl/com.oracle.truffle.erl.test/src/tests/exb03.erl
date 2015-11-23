-module(exb03).
-export([main/0]).

main() ->
	binary_to_term(<<131,98,0,0,39,16>>).
