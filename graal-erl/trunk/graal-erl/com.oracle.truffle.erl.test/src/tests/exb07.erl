-module(exb07).
-export([main/0]).

main() ->
	binary_to_term(<<131,110,15,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16>>).
