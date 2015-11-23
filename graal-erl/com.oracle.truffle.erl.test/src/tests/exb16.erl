-module(exb16).
-export([main/0]).

main() ->
	binary_to_term(<<131,116,0,0,0,3,97,1,97,2,97,2,97,3,97,3,97,4>>).
