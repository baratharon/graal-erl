-module(exb06).
-export([main/0]).

main() ->
	binary_to_term(<<131,110,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16>>).
