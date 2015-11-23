-module(exb02).
-export([main/0]).

main() ->
	binary_to_term(<<131,97,10>>).
