-module(exb01).
-export([main/0]).

main() ->
	binary_to_term(<<131,100,0,4,97,116,111,109>>).
