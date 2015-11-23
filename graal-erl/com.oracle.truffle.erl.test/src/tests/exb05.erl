-module(exb05).
-export([main/0]).

main() ->
	binary_to_term(<<131,110,5,1,69,220,138,237,19>>).
