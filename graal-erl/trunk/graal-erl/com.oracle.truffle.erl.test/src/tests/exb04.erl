-module(exb04).
-export([main/0]).

main() ->
	binary_to_term(<<131,110,5,0,252,253,254,255,1>>).
