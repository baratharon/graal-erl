-module(exb13).
-export([main/0]).

main() ->
	binary_to_term(<<131,108,0,0,0,1,98,0,0,1,244,98,0,0,2,88>>).
